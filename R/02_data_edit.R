## ---- process-data-demographics

demographics <- 
  data.frame(lapply(demographics, 
                    function(x) {
                      gsub("Native", "C2", x)
                      }
                    ))

demographics <- 
  data.frame(lapply(demographics, 
                    function(x) {
                      gsub("B1.2", "B1", x)
                    }
  ))

demographics <- 
  data.frame(lapply(demographics, 
                    function(x) {
                      gsub("NA", "C1", x)
                    }
  ))

# add observation to demographics based on auxiliary information
data_edit_demographics <- data.frame(id = "d12", 
                                            age = NA, 
                                            sex = "female", 
                                            major = "computer science", 
                                            language = "{English}", 
                                            language_proficiency = "{C2}", 
                                            countries_resided = "{Ghana, Germany}", 
                                            countries_health_care = "{Ghana, Germany}", 
                                            country_birth = "Ghana", 
                                            year_arrival = 2022)

demographics_processed <- rbind(demographics, data_edit_demographics)

demographics <- demographics_processed

# create variable of months since arrival
demographics$date_workshop <- 
  as.Date("2024-02-06")

demographics$year_arrival_date_estimate <- 
  as.Date(paste0(demographics$year_arrival,"-", "09-01"))

demographics$weeks_arrival <- 
  difftime(demographics$date_workshop, 
           demographics$year_arrival_date_estimate, 
           units = "weeks")

demographics$month_approximation <- 
  (demographics$weeks_arrival/4)

## ---- process-data-health-info

# add observation to health related info based on auxiliary information
data_edit_health_related_info <- data.frame(id = "h12", 
                                            pre_existing_conditions = "yes", 
                                            pre_existing_conditions_list = "{asthma}", 
                                            visits = NA)

health_related_information_processed <- rbind(health_related_information, 
                                              data_edit_health_related_info)

health_related_information <- health_related_information_processed

# standardise visit variable
health_related_information$visits_standardised <- 
  ifelse(health_related_information$visits %in% c("not frequently cause the response wasn't up to the mark", 
                                                  "1", "1 to 2", "2", "3", "4", "3 to 4"), "< 5", 
         ifelse(health_related_information$visits %in% c("5", "6", "5 to 6"), "5 - 10", 
                ifelse(health_related_information$visits == "more than 10", "> 10", NA)))

health_related_information$visits_standardised <- 
  ifelse(is.na(health_related_information$visits_standardised), "Not specified", 
         health_related_information$visits_standardised)

# specify order for plotting
visits_standardised_order <- c('< 5', 
                               '5 - 10', 
                               '> 10')

## ---- process-data-communication-survey

# code missingness
communication_exp[3, "listened"] <- NA

# convert responses to scale labels
communication_exp$comm_hcw <- ifelse(communication_exp$hcw_only == 1, 
                                     "Strongly disagree", 
                                     ifelse(communication_exp$hcw_only == 2, 
                                            "Disagree", 
                                            ifelse(communication_exp$hcw_only == 3, 
                                                   "Neutral", 
                                                   ifelse(communication_exp$hcw_only == 4, 
                                                          "Agree", 
                                                          ifelse(communication_exp$hcw_only == 5, 
                                                                 "Strongly agree", NA)))))

communication_exp$comm_pat <- ifelse(communication_exp$patient_only == 1, 
                                     "Strongly disagree", 
                                     ifelse(communication_exp$patient_only == 2, 
                                            "Disagree", 
                                            ifelse(communication_exp$patient_only == 3, 
                                                   "Neutral", 
                                                   ifelse(communication_exp$patient_only == 4, 
                                                          "Agree", 
                                                          ifelse(communication_exp$patient_only == 5, 
                                                                 "Strongly agree", NA)))))

communication_exp$both <- ifelse(communication_exp$both == 1, 
                                 "Strongly disagree", 
                                 ifelse(communication_exp$both == 2, 
                                        "Disagree", 
                                        ifelse(communication_exp$both == 3, 
                                               "Neutral", 
                                               ifelse(communication_exp$both == 4, 
                                                      "Agree", 
                                                      ifelse(communication_exp$both == 5, 
                                                             "Strongly agree", NA)))))

# specify order for plotting
likert_standardised_order <- c('Strongly disagree', 
                               'Disagree', 
                               'Neutral', 
                               'Agree', 
                               'Strongly agree')

misunderstood_order <- c('Not sure', 
                         'No', 
                         'Yes')

listening_gaps_order <- c('Not sure ', 
                          'No', 
                          'Yes')

## ---- process-waiting-room-survey

# create standard levels of how people arrange visit
waiting_room_visit <- waiting_room_exp

waiting_room_visit$arrange_visit <- 
  factor(waiting_room_visit$arrange_visit, levels = c("Calling ahead and making an appointment ", 
                                                      "Walking into the clinic and waiting  ", 
                                                      "Other"))

# standardise waiting time
waiting_room_exp$waiting_time_standardised <- 
  ifelse(grepl("20 minutes| 15| 5", waiting_room_exp$clinic_wait), "< 30", 
         ifelse(grepl("30", waiting_room_exp$clinic_wait), ">= 30", NA))

# impute waiting time
waiting_room_exp$waiting_time_standardised <- 
  ifelse(!is.na(waiting_room_exp$waiting_time_standardised), 
         waiting_room_exp$waiting_time_standardised, 
         ifelse(waiting_room_exp$clinic_busy == "Very busy", ">= 30", "< 30"))

# edit mobile devices response
waiting_room_exp$mobile_devices <- 
  ifelse(grepl("Yes", waiting_room_exp$mobile_devices), "Yes", 
         ifelse(grepl("No", waiting_room_exp$mobile_devices), "No", 
                "Not sure"))

waiting_room_exp$mobile_devices <- factor(waiting_room_exp$mobile_devices, 
                                          levels = c("Not sure", "No", "Yes"))

# edit mobile data responses
waiting_room_exp$mobile_data <- 
  ifelse(grepl("Other", waiting_room_exp$mobile_data), "No connection", 
         ifelse(grepl("Weak", waiting_room_exp$mobile_data), "Weak", 
                ifelse(grepl("Moderate", waiting_room_exp$mobile_data), "Moderate", 
                       ifelse(grepl("Strong", waiting_room_exp$mobile_data), "Strong", 
                              "Not sure"))))

waiting_room_exp$mobile_data <- factor(waiting_room_exp$mobile_data, 
                                       levels = c("Not sure", 
                                                  "No connection", 
                                                  "Weak", 
                                                  "Moderate", 
                                                  "Strong"))