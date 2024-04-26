## ---- aggregate-demographics

# aggregate data for regions resided in
region_input <- data.frame(region = countries_resided)

region_input$region <- ifelse(region_input$region %in% c("Myanmar", 
                                                         "Cyprus", 
                                                         "India", 
                                                         "Bahrain", 
                                                         "Oman", 
                                                         "Turkey", 
                                                         "PalestineWestBank"), 
                              "Asia", 
                              ifelse(region_input$region %in% c("Serbia",
                                                                "Poland", 
                                                                "France"), 
                                     "Europe", 
                                     ifelse(region_input$region %in% c("Barbados", 
                                                                       "Brazil", 
                                                                       "Nicaragua"), 
                                            "Americas", NA)))

region_input$co_creator <- cr_respondents_match

region_input$co_creator <- 
  ifelse(
    region_input$co_creator == "CC1", 
    "CC01", 
    ifelse(
      region_input$co_creator == "CC2", 
      "CC02", 
      ifelse(
        region_input$co_creator == "CC3", 
        "CC03", 
        ifelse(
          region_input$co_creator == "CC4", 
          "CC04", 
          ifelse(
            region_input$co_creator == "CC5", 
            "CC05", 
            ifelse(
              region_input$co_creator == "CC6", 
              "CC06", 
              ifelse(
                region_input$co_creator == "CC7", 
                "CC07", 
                ifelse(
                  region_input$co_creator == "CC8", 
                  "CC08", 
                  ifelse(
                    region_input$co_creator == "CC9", 
                    "CC09", 
                    region_input$co_creator)))))))))


region_input <- region_input[!is.na(region_input$region), ]

# aggregate data for country of birth and for country received health care in
healthcare_received <- data.frame(region_birth = cob_respondents_match, 
                                  healthcare = country_health_care_received)

healthcare_received$region_birth <- 
  ifelse(healthcare_received$region_birth %in% c("Myanmar", 
                                                 "Cyprus", 
                                                 "India", 
                                                 "Bahrain", 
                                                 "Oman", 
                                                 "Turkey", 
                                                 "Palestine", 
                                                 "Israel"), 
         "Asia", 
         ifelse(healthcare_received$region_birth %in% c("Serbia",
                                                        "Poland", 
                                                        "France"), 
                "Europe", 
                ifelse(healthcare_received$region_birth %in% c("Barbados", 
                                                               "Brazil", 
                                                               "Nicaragua"), 
                       "Americas", NA)))

healthcare_received$healthcare <- 
  ifelse(healthcare_received$healthcare %in% c("Myanmar", 
                                               "Cyprus", 
                                               "India", 
                                               "Bahrain", 
                                               "Oman", 
                                               "Turkey", 
                                               "PalestineWestBank", 
                                               "Israel"), 
         "Asia", 
         ifelse(healthcare_received$healthcare %in% c("Serbia",
                                                      "Poland", 
                                                      "France"), 
                "Europe", 
                ifelse(healthcare_received$healthcare %in% c("Barbados", 
                                                             "Brazil", 
                                                             "Nicaragua"), 
                       "Americas", NA)))


cob_input_table <- 
  as.data.frame(table(healthcare_received$region_birth, 
                      healthcare_received$healthcare))

cob_input_table$region_birth <- cob_input_table$Var1

cob_input_table$healthcare <- cob_input_table$Var2

cob_input_table$Count <- cob_input_table$Freq

## ---- aggregate-health-related-info

# convert conditions to high level ICD codes
health_conditions <- data.frame(condition = conditions_diagnosed)

health_conditions$condition_level_one <- 
  ifelse(health_conditions$condition %in% c("asthma", "chronicbronchitis"), "J", 
         ifelse(health_conditions$condition == "allergies", "T", 
                ifelse(health_conditions$condition %in% c("scoliosis", "Herniateddisc"), "M", 
                       ifelse(health_conditions$condition == "policysticovariansyndrome", "E", 
                              ifelse(health_conditions$condition %in% c("generalisedanxietydisorder", 
                                                                        "manic-depressivedisorder", 
                                                                        "ADHD", 
                                                                        "Panicattackdisorder", 
                                                                        "chronicdepression"), "F", 
                                     "D")))))

health_conditions$count <- 
  ifelse(health_conditions$condition_level_one == "J", 
         table(health_conditions$condition_level_one)["J"], 
         ifelse(health_conditions$condition_level_one == "T", 
                table(health_conditions$condition_level_one)["T"], 
                ifelse(health_conditions$condition_level_one == "M", 
                       table(health_conditions$condition_level_one)["M"], 
                       ifelse(health_conditions$condition_level_one == "E", 
                              table(health_conditions$condition_level_one)["E"], 
                              ifelse(health_conditions$condition_level_one == "F", 
                                     table(health_conditions$condition_level_one)["F"], 
                                     table(health_conditions$condition_level_one)["D"])))))


## ---- aggregate-waiting-room-survey

# aggregate visit variables
visit_input <- as.data.frame(table(waiting_room_visit$arrange_visit, 
                                   waiting_room_visit$self_visit))

visit_input$arrange_visit <- visit_input$Var1

visit_input$self_visit <- visit_input$Var2

visit_input$Count <- visit_input$Freq

# aggregate people interacted with
people_interaction_input <- data.frame(co_creator = wri_respondents_match, 
                                       people_interaction = waiting_room_interactions)

people_interaction_input$co_creator <- 
  ifelse(
    people_interaction_input$co_creator == "CC1", 
    "CC01", 
    ifelse(
      people_interaction_input$co_creator == "CC2", 
      "CC02", 
      ifelse(
        people_interaction_input$co_creator == "CC3", 
        "CC03", 
        ifelse(
          people_interaction_input$co_creator == "CC4", 
          "CC04", 
          ifelse(
            people_interaction_input$co_creator == "CC5", 
            "CC05", 
            ifelse(
              people_interaction_input$co_creator == "CC6", 
              "CC06", 
              ifelse(
                people_interaction_input$co_creator == "CC7", 
                "CC07", 
                ifelse(
                  people_interaction_input$co_creator == "CC8", 
                  "CC08", 
                  ifelse(
                    people_interaction_input$co_creator == "CC9", 
                    "CC09", 
                    people_interaction_input$co_creator)))))))))

# aggregate waiting time and waiting location
waiting_time_location <- as.data.frame(table(waiting_room_exp$waiting_time_standardised, 
                                             waiting_room_exp$wait_location))

waiting_time_location$wait_time <- waiting_time_location$Var1

waiting_time_location$wait_location <- waiting_time_location$Var2

waiting_time_location$Count <- waiting_time_location$Freq

# aggregate waiting room and waiting location
waiting_room_location <- as.data.frame(table(waiting_room_exp$clinic_waiting_room, 
                                             waiting_room_exp$wait_location))

waiting_room_location$waiting_room <- waiting_room_location$Var1

waiting_room_location$wait_location <- waiting_room_location$Var2

waiting_room_location$Count <- waiting_room_location$Freq

# aggregate mobile data responses and cell phone
mobile_devices_data <- as.data.frame(table(waiting_room_exp$mobile_devices, 
                                           waiting_room_exp$mobile_data))

mobile_devices_data$mobile_devices <- mobile_devices_data$Var1

mobile_devices_data$mobile_data <- mobile_devices_data$Var2

mobile_devices_data$Count <- mobile_devices_data$Freq