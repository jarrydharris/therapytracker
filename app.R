#App for speech pathologists to record activity prompt success during speech and language therapy sessions
#Results can be communicated through data visualisation at the end of the session and over time to keep track of childs progress
#Author: Jarryd Harris

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(shinyalert)
library(googlesheets4)

fields <- c("first_name", "last_name", "date", "activity", "attempt", "prompt_value")
activity_url <- "https://docs.google.com/spreadsheets/d/1ncHMI_PCqVUOKbLgebe2dit3tYZsNFxoKqRVdOzGfLg/edit?usp=sharing"

todays_date <- function() {
    #returns date in iso format
    format(as.POSIXct(Sys.time(), tz="AEDT"), "%d/%m/%Y")
}

latest_date <- function(){
    #returns the latest session date from the response dataframe
    date_return <- loadData() %>% 
        select(date) %>%
        filter(as.Date(date, "%d/%m/%y") == max(as.Date(loadData()$date, "%d/%m/%y"))) %>% 
        unique()
    return(date_return[[1]])
}

saveData <- function(data) {
    #Code altered from:
    #https://deanattali.com/blog/shiny-persistent-data-storage/
    data <- as.data.frame(t(data))
    if (exists("responses")) {
        responses <<- rbind(responses, data)
    }else if(exists("in_file")){
        responses <<- in_file
    }else{
        responses <<- data
    }
    
    write.csv(
        x = responses,
        file = file_name(),
        row.names = FALSE, quote = TRUE
    )
}

loadData <- function() {
    #https://deanattali.com/blog/shiny-persistent-data-storage/
    if (exists("responses")) {
        responses
    }
}

CleanEnvir <- function(x) {
    #https://stackoverflow.com/questions/4837477/remove-objects-in-globalenv-from-within-a-function
    rm(list=deparse(substitute(x)),envir=.GlobalEnv)
}

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "side1",
        menuItem("Home",
                 tabName = "home",
                 icon = icon("home")),
        menuItem("Session",
                 tabName = "session",
                 icon = icon("comments")),
        menuItem("Results",
                 tabName = "results",
                 icon = icon("chart-bar")),
        menuItem("Data",
                 tabName = "data",
                 icon = icon("database"))
    )
)

body <- dashboardBody(
    useShinyalert(),
    
    tabItems(
        tabItem(
            tabName = "home",
            tabBox(title = "Start a session",
                   tabPanel("New client",
                            h3("Getting started: "),
                            p("1. Enter your clients first and last name"),
                            p("2. Click the 'Create client' button"),
                            p("3. After confirmation, navigate to the sidebar and click session"),
                            textInput("first_name", "First Name"),
                            textInput("last_name", "Last Name"),
                            actionButton("create", "Create client")),
                   tabPanel("Existing client",
                            h3("Continue with existing client: "),
                            p("1. Under the 'Choose file to upload' label, click 'Browse...'"),
                            p("2. Select your .csv file to upload"),
                            p("3. After confirmation, navigate to the sidebar and click session"),
                            fileInput("open_file",
                                      "Choose file to upload: "))
            )
        ),
        tabItem(
            tabName = "session",
            fluidRow(
                box(width = 8,
                    align = "center",
                    selectizeInput("activities", 
                                "Activity Name:",
                                choices = "",
                                options = list(create = TRUE))
                    )
            ),
            fluidRow(
                box(width = 8,
                    align = "center",
                    actionButton("alpha",
                                 "α",
                                 style='padding:50px; font-size:200%'),
                    actionButton("beta",
                                 "β",
                                 style='padding:50px; font-size:200%')
                    
                ),
                box(width = 8,
                    align = "center",
                    infoBoxOutput(
                        "attempt"
                    ))
            )
        ),
        tabItem(
            tabName = "results",
            tabBox(
                tabPanel(
                    title = "Session results",
                    plotOutput("todays_plot"),
                    selectInput("todays_date", "Choose session date: ", 
                                choices = "",
                                selected = ""),
                    dataTableOutput("todays_summary")
                ),
                tabPanel(
                    title = "All results",
                    plotOutput("all_plot"),
                    selectInput("all_select",
                                "Choose activites: ",
                                choices = NULL,
                                multiple = TRUE)
                )
            )
        ),
        tabItem(
            tabName = "data",
            dataTableOutput("responses"),
            downloadButton("download", label = "Download", icon = icon("file-download")),
            actionButton("reset", label = "Reset session", icon = icon("power-off"))
        )
    )
    
)

ui <- dashboardPage(
    header = dashboardHeader(
        title = "SLP Therapy Tracker"
    ),
    sidebar = sidebar,
    body = body
)


server <- function(input, output, session) {
    
    #Prevents need to log into google sheets to get the activity list
    sheets_deauth()
    try(
        #loads activities from a public google sheet to allow user to prefill planned activities
        #wrapped in try block just in case there is any issues with the public sheet access
        sheet_activities <- read_sheet(activity_url)
    )
    
    #a reactive value list that stores the current state of the system
    #and can be looped over for the data storage part
    state <- reactiveValues(
        first_name = NULL,
        last_name = NULL,
        date = todays_date(),
        activity = NULL,
        attempt = 0,
        prompt_value = 0
    )
    
    
    observeEvent(
        input$open_file,
        {
            in_file <- input$open_file
            #TODO: error handling for invalid data
            responses <<- read.csv(in_file$datapath)
            state$first_name <- as.character(responses$first_name)[1]
            state$last_name <- as.character(responses$last_name)[1]
            shinyalert(
                title = "Success!",
                text = paste("Client ", state$first_name, state$last_name, "has been uploaded."),
                closeOnEsc = TRUE,
                closeOnClickOutside = TRUE,
                type = "success",
                animation = "true"
            )
        }
    )
    
    observeEvent(
        input$side1,
        {
            if(!is.null(loadData())){
                updateSelectInput(session, "todays_date", 
                                  choices = unique(loadData()$date),
                                  selected = latest_date())
                }
        }
    )
    
    #TODO: Refactor first/last_name state updates
    observeEvent(
        input$first_name,
        {
            state$first_name <- input$first_name
        }
    )
    
    observeEvent(
        input$last_name,
        {
            state$last_name <- input$last_name
        }
    )
    
    #Prompts user if they have followed the correct procedure when creating a client
    observeEvent(
        input$create,
        {
            if(input$first_name != "" & input$last_name != ""){
                column_names <- data.frame()
                for(k in fields) column_names[[k]] <- as.character()
                write.csv(column_names, file_name(), row.names = FALSE)
                shinyalert(
                    title = "Success!",
                    text = paste("Client ", input$first_name, input$last_name, "has been created."),
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    type = "success",
                    animation = "true"
                )
            }else{
                shinyalert(
                    title = "Hold on!",
                    text = "Please enter the clients name to continue",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    type = "error",
                    animation = "true"
                )
            }
        }
    )
    
    #increases every time user completes a attempt
    attempt_number <- reactiveValues(counter = 1)
    
    #called anytime the user inputs a attempt, updates current state and writes to csv
    update_state <- function(){
        state$attempt <- attempt_number$counter #updates the system state for data collection
        state$activity <- input$activities
        state$date <- todays_date()
        saveData(formData()) #submitting new state to dataframe
        attempt_number$counter <- attempt_number$counter + 1
    }
    
    observeEvent(
        input$alpha, 
        {
            state$prompt_value <- 1
            update_state()
        })
    
    observeEvent(
        input$beta,
        {
            state$prompt_value <- 0
            update_state()
        })
    
    formData <- reactive({
        data <- sapply(fields, function(x) state[[x]])
        data
    })
    
    observeEvent(
        input$open_file,
                 {
                     #TODO: Refactor
                     if(todays_date() == latest_date()){
                         #TODO: current activity should change to activity with the largest atttempt
                         if(exists("sheet_activities")){
                         updateSelectizeInput(session, "activities", 
                                              choices = c(as.character(unique(loadData()$activity)), #TODO: Refactor 
                                                          sheet_activities))
                         }else{
                             updateSelectizeInput(session, "activities", 
                                                  choices = c(as.character(unique(loadData()$activity))))
                             }
                         state$attempt <- max(loadData()$attempt) + 1
                         attempt_number$counter <- max(loadData()$attempt) + 1
                     }else{
                         if(exists("sheet_activities")){
                             updateSelectizeInput(session, "activities", 
                                                  choices = c(as.character(unique(loadData()$activity)),
                                                              sheet_activities))
                         }else{
                             updateSelectizeInput(session, "activities", 
                                                  choices = c(as.character(unique(loadData()$activity))))
                         }
                     }
                 })
    
    observeEvent(
        input$create,
        {
            updateSelectizeInput(session, "activities", choices = c(sheet_activities))
        }
    )

    
    #TODO: Clean up axis
    blank_plot <- function(){
        #The blank plot displayed when user has not uploaded any data
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + 
            scale_y_continuous(limits = c(0, 1)) + scale_x_continuous(limits = c(0, 1))
        
        }
    
    output$todays_plot <- renderPlot({
        input$side1
        
        if(!is.null(loadData())){
            plot_data <- loadData() %>% filter(as.Date(date, "%d/%m/%y") == as.Date(input$todays_date, "%d/%m/%y")) %>% group_by(activity) %>% 
                summarize(correctness = mean(prompt_value == 1)) %>% as.data.frame()
            #TODO: Can this be done without the plot redrawn each time?
            plot_data %>% ggplot(aes(x = activity, y = correctness, fill = activity)) + 
                geom_bar(stat = "identity") + scale_y_continuous(limits = c(0, 1)) + 
                geom_abline(slope = 0, intercept = .8, color = "deepskyblue", linetype = "dashed", size = 1) 
        }else{
            blank_plot()
        }
    })
    
    output$todays_summary <- renderDataTable({
        input$side1
        
        if(!is.null(loadData())){
            loadData() %>% filter(as.Date(date, "%d/%m/%y") == as.Date(input$todays_date, "%d/%m/%y")) %>%
                select(activity, prompt_value) %>% 
                group_by(activity) %>% 
                summarize(alpha = sum(prompt_value == 1),
                          beta = sum(prompt_value == 0),
                          correctness = round((alpha / (alpha + beta))*100)) %>% 
                datatable(rownames = FALSE, filter = "none", autoHideNavigation = TRUE)
            }
    })
    
    output$all_plot <- renderPlot({
        input$side1

        if(!is.null(loadData())){
            plot_data <- loadData() %>% group_by(date, activity) %>% 
                summarize(correctness = mean(prompt_value == 1)) %>% as.data.frame() 
            if(length(input$all_select) == 0){
                plot_data %>% ggplot(aes(x = date, y = correctness)) + geom_point() +
                    geom_line(color = "red1" , size = 1, group = 1) + facet_wrap(~activity) +
                    scale_y_continuous(limits = c(0, 1)) + theme(axis.text.x = element_text(angle = 30)) + 
                    geom_abline(slope = 0, intercept = .8, color = "deepskyblue", linetype = "dashed", size = 1) 
            }else{
                plot_data %>%  filter(activity %in% input$all_select) %>% ggplot(aes(x = date, y = correctness)) + 
                    geom_line(color = "red1" , size = 1, group = 1) + geom_point() + facet_wrap(~activity) + 
                    scale_y_continuous(limits = c(0, 1)) + theme(axis.text.x = element_text(angle = 30)) + 
                    geom_abline(slope = 0, intercept = .8, color = "deepskyblue", linetype = "dashed", size = 1) 
            }
        }else{
            blank_plot()
        }
    })
    
    #global filename variable updates with clients name
    file_name <<- reactive(paste(state$first_name, state$last_name, ".csv", sep = ""))
    
    observe({
        #Updates the activities generated in all sessions so the user can select what activities to display
        if(input$side1 == "results" && exists("responses")){
            updated_activites = unique(responses$activity)
            updateSelectInput(session, "all_select", choices = updated_activites, selected = updated_activites)
        }
        
    })
    
    observe({
        #Notifies the user they are attempting to begin a session or view results without uploading or correctly creating a profile
        #A first and last name must be entered and the create client button must be clicked to generate a csv of the format "firstnamelastname.csv"
        #TODO: Check for correct file creation
        if(input$side1 != "home" && ((state$first_name == "") || (state$last_name == ""))){
            shinyalert(
                title = "Hold on!",
                text = "The clients profile has not been entered correctly.",
                closeOnEsc = TRUE,
                closeOnClickOutside = TRUE,
                type = "error",
                animation = "true"
            )
        }
    })
    
    output$download <- downloadHandler(
        filename = file_name(),
        content = function(file) {
            responses %>% write.csv(
                file,
                row.names = FALSE, quote = TRUE
        )}
    )
    
    output$attempt <- renderInfoBox({
        infoBox(
            "attempt",
            state$attempt,
            icon = icon("pen-nib")
        )
    })
    
    output$responses <- renderDataTable({
        input$side1
        if(!is.null(loadData())){
            loadData() %>% datatable(editable = TRUE)
            }
        })
    
    observeEvent(input$reset,
        shinyalert(
            title = "Under construction! (Check back later)",
            text = "Please close the tab and revist the app to start a new session!",
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            showCancelButton = TRUE,
            type = "warning",
            animation = "true",
            callbackR = reset_session(),
        )
    )
    
    session$onSessionEnded(function() {
        #cleaning session variables before exiting session for security concerns
        if(exists("responses")){
            CleanEnvir(responses)
        }
        if(exists("file_name")){
            CleanEnvir(file_name)
        }
    })

}

shinyApp(ui, server)
