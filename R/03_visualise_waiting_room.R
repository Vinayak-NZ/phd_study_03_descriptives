## ---- visualise-waiting-room-survey

# aggregate visit variables
waiting_room_visit <- waiting_room_exp

waiting_room_visit$arrange_visit <- 
  factor(waiting_room_visit$arrange_visit, levels = c("Calling ahead and making an appointment ", 
                                                      "Walking into the clinic and waiting  ", 
                                                      "Other"))

visit_input <- as.data.frame(table(waiting_room_visit$arrange_visit, 
                                   waiting_room_visit$self_visit))

visit_input$arrange_visit <- visit_input$Var1

visit_input$self_visit <- visit_input$Var2

visit_input$Count <- visit_input$Freq

visit.labels <- c("Calling ahead and\nmaking an appointment",
               "Walking into clinic \nand waiting", 
               "Other")

# plot heatmap of approach to visit and self visit
ggplot(visit_input, 
       aes(arrange_visit, 
           self_visit, 
           fill = Count)) +
  geom_tile(color = "#FFFFFF",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = Count), color = "#FFFFFF", size = 4) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#e18b22",
                       mid = "#46e7fd",
                       high = "#4739a2", 
                       labels = c("0", "2", "4", "6", "8")) +
  labs(title = paste0("Experiences of collaborators"), 
       subtitle = "Heatmap of typical visit to the primary care physician",
       caption = "Data source: Patient preparedness tool") +
  xlab("Arranging visit") + 
  ylab("Visiting physician by themselves") +
  scale_x_discrete(labels = visit.labels) +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", linewidth = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

# extract people interacted with
waiting_room_visit$people_interaction <- gsub(" ", "", 
                                              waiting_room_visit$people_interaction, 
                                              fixed = TRUE)

waiting_room_list <- regmatches(waiting_room_visit$people_interaction, 
                           gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                                    waiting_room_visit$people_interaction, 
                                    perl=TRUE))

waiting_room_list_edited <- lapply(waiting_room_list, strsplit, ",")

waiting_room_interactions <- unlist(waiting_room_list_edited)

# extract respondents for people interacted with
wri_number_responses_list <- lapply(waiting_room_list_edited, 
                                   function(x) {length(unlist(x))})

wri_number_responses <- unlist(wri_number_responses_list)

wri_respondents_match <- c()

for(i in 1:nrow(waiting_room_exp)){
  
  x <- rep(paste0("CC", i), wri_number_responses[[i]])
  
  wri_respondents_match <- c(wri_respondents_match, x)
  
}

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

# plot people interacted with
ggplot(people_interaction_input, aes(people_interaction, co_creator)) + 
  geom_point(size = 5,colour = "#4739a2") + 
  labs(title = paste0("Experiences of collaborators"), 
       subtitle = "Scatterplot of interactions during typical visit in waiting room",
       caption = "Data source: Patient preparedness tool") +
  xlab("People interacted with in waiting room") + 
  ylab("Collaborator") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", size = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

# standardise waiting time
waiting_room_exp$waiting_time_standardised <- 
  ifelse(grepl("20 minutes| 15| 5", waiting_room_exp$clinic_wait), "< 30", 
         ifelse(grepl("30", waiting_room_exp$clinic_wait), ">= 30", NA))

# impute waiting time
waiting_room_exp$waiting_time_standardised <- 
  ifelse(!is.na(waiting_room_exp$waiting_time_standardised), 
         waiting_room_exp$waiting_time_standardised, 
         ifelse(waiting_room_exp$clinic_busy == "Very busy", ">= 30", "< 30"))

# aggregate waiting time and waiting location
waiting_time_location <- as.data.frame(table(waiting_room_exp$waiting_time_standardised, 
                                             waiting_room_exp$wait_location))

waiting_time_location$wait_time <- waiting_time_location$Var1

waiting_time_location$wait_location <- waiting_time_location$Var2

waiting_time_location$Count <- waiting_time_location$Freq

wait.time.labels <- c("< 30",
                          "> = 30")

wait.location.labels <- c("Inside the \nclinic",
                  "Outside the \nclinic", 
                  "Other")

# plot heatmap of approach to visit and self visit
ggplot(waiting_time_location, 
       aes(wait_time, 
           wait_location, 
           fill = Count)) +
  geom_tile(color = "#FFFFFF",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = Count), color = "#FFFFFF", size = 4) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#e18b22",
                       mid = "#46e7fd",
                       high = "#4739a2") +
  labs(title = paste0("Experiences of collaborators"), 
       subtitle = "Heatmap of typical waiting time and location for primary care visit",
       caption = "Data source: Patient preparedness tool") +
  xlab("Typical waiting time") + 
  ylab("Typical waiting location") +
  scale_x_discrete(labels = wait.time.labels) +
  scale_y_discrete(labels = wait.location.labels) +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", linewidth = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

# aggregate waiting room and waiting location
waiting_room_location <- as.data.frame(table(waiting_room_exp$clinic_waiting_room, 
                                             waiting_room_exp$wait_location))

waiting_room_location$waiting_room <- waiting_room_location$Var1

waiting_room_location$wait_location <- waiting_room_location$Var2

waiting_room_location$Count <- waiting_room_location$Freq

wait.location.labels <- c("Inside the \nclinic",
                          "Outside the \nclinic", 
                          "Other")

# plot heatmap of waiting room and waiting location
ggplot(waiting_room_location, 
       aes(waiting_room, 
           wait_location, 
           fill = Count)) +
  geom_tile(color = "#FFFFFF",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = Count), color = "#FFFFFF", size = 4) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#e18b22",
                       mid = "#46e7fd",
                       high = "#4739a2", 
                       labels = c("0", "2", "4", "6", "8")) +
  labs(title = paste0("Experiences of collaborators"), 
       subtitle = "Heatmap of clinic waiting environment and waiting location",
       caption = "Data source: Patient preparedness tool") +
  xlab("Does clinic have a waiting room?") + 
  ylab("Typical waiting location") +
  scale_y_discrete(labels = wait.location.labels) +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", linewidth = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

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

# aggregate mobile data responses and cell phone
mobile_devices_data <- as.data.frame(table(waiting_room_exp$mobile_devices, 
                                           waiting_room_exp$mobile_data))

mobile_devices_data$mobile_devices <- mobile_devices_data$Var1

mobile_devices_data$mobile_data <- mobile_devices_data$Var2

mobile_devices_data$Count <- mobile_devices_data$Freq

# plot heatmap of waiting room and waiting location
ggplot(mobile_devices_data, 
       aes(mobile_devices, 
           mobile_data, 
           fill = Count)) +
  geom_tile(color = "#FFFFFF",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = Count), color = "#FFFFFF", size = 4) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#e18b22",
                       mid = "#46e7fd",
                       high = "#4739a2") +
  labs(title = paste0("Experiences of collaborators"), 
       subtitle = "Heatmap of mobile connection inside clinic",
       caption = "Data source: Patient preparedness tool") +
  xlab("Does clinic allow \nmobile phones to be used?") + 
  ylab("Strength of mobile data \nconnection inside clinic") +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", linewidth = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )
