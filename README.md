# Speech and Language Therapy Tracker

This is a data collection and visulation app designed to be used by Speech and Language pathologists during intervention therapy.
<!-- ## Screenshot:
![https://jharr.shinyapps.io/therapytracker/](/images/bar-cropbig.png "https://jharr.shinyapps.io/therapytracker/") -->

## Demo:
1. Go to https://jharr.shinyapps.io/therapytracker/
2. Under the home tab create a new client or use the existing testdata.csv file provided to explore the app
3. Once a client has been loaded, click session in the sidebar
4. Enter a new activity or choose a preloaded one from the "Activity Name" select input field
5. Choose alpha if the child correctly responds to the interaction, beta if not (these symbols have been chosen so the child does not register positive or negative feedback from use of the app)
6. After the session is done, click results to explore the outcomes. The "Session results" enables you to choose a specific date and drill down on the activities, "All results" allows you to compare them over time.
7. Click the data tab to edit or save the results.
8. To start a new session please close and reopen the tab (A reset button will be added later to make this a nicer process)

## Description:
Client needed to:
 
- Record outcomes of patients response to therapeutic prompts
- Communicate the results of a session to parents
- Show how the results change over multiple sessions
- Store results for separate patients
- Use on both an iPad and laptop
 
I met the needs by:
 
- Developing a simple Web App in Shiny with data stored in google sheets. This ensures everything works on both the devices requested
- Client can create "profiles" for each patient
- Client can add in new therapies if needed
- Simple bar graphs to show the percentage of successful responses
- Line graph to show success over different sessions
 
 
## Skills demonstrated
- Interactive dashboard
- Data storage via google sheets api
- Data Visualization
- Web App in R/Shiny
 
