## ---- visualise-visit-variables

visit.labels <- c("Calling ahead and\nmaking an appointment",
               "Walking into clinic \nand waiting", 
               "Other")

clinic_visit_plot <- 
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

ggsave("output/clinic_visit_plot.png", 
       plot = clinic_visit_plot)

## ---- visualise-people-interactions

waiting_room_interactions <- 
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

ggsave("output/waiting_room_interactions.png", 
       plot = waiting_room_interactions)

## ---- visualise-wait-time-location

wait.time.labels <- c("< 30",
                          "> = 30")

wait.location.labels <- c("Inside the \nclinic",
                  "Outside the \nclinic", 
                  "Other")

wait_time_location_plot <- 
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

ggsave("output/wait_time_location_plot.png", 
       plot = wait_time_location_plot)

## ---- visualise-room-location

wait.location.labels <- c("Inside the \nclinic",
                          "Outside the \nclinic", 
                          "Other")

wait_room_location_plot <- 
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

ggsave("output/wait_room_location_plot.png", 
       plot = wait_room_location_plot)

## ---- visualise-mobile-devices-data

mobile_data_devices_plot <- 
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

ggsave("output/mobile_data_devices_plot.png", 
       plot = mobile_data_devices_plot)