#Random data generator for testing slp app

fields = c("first_name", "last_name", "date", "activity", "attempt", "prompt_value")
n = 100
data <- data.frame("first_name" = "Jarryd",
                   "last_name" = "Harris", 
                   "date" = sample(c("15/01/2020", "14/01/2020", "13/01/2020", "12/01/2020", "11/01/2020", "10/01/2020"),
                                   size = 100, replace = TRUE), 
                   "activity" = sample(c("Speech", "Language", "Vocabulary"), size = 100, replace = TRUE, prob = c(0.8, 0.25, 0.35)), 
                   "attempt" = 0,
                   "prompt_value" = sample(0:1, size = 100, replace = TRUE, prob = c(0.3, 0.7)))

write.csv(data, "testdata.csv", row.names = FALSE)

