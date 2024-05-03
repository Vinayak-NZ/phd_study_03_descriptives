## ---- plot-regions-resided-in

region_resided_plot <- 
  ggplot(region_input, aes(region, co_creator)) + 
  geom_point(size = 5,colour = "#4739a2") + 
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Scatterplot of regions resided in prior to Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("Region") + 
  ylab("Collaborator") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", linewidth = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/region_resided_plot.png", 
       plot = region_resided_plot)

## ---- plot-regions-birth-region-healthcare

region_birth_healthcare_plot <- 
  ggplot(cob_input_table, 
       aes(region_birth, 
           healthcare, 
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
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Heatmap of regions received care in preior to Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("Region of birth") + 
  ylab("Region received health care in") +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", size = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/region_birth_healthcare_plot.png", 
       plot = region_birth_healthcare_plot)

## ---- plot-months-since-arrival

months_arrival_plot <- 
  ggplot(demographics, aes(x = month_approximation)) +
  geom_histogram(bins = 7, fill = "#4739a2") +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Histogram of months since arrival in Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("Time since arrival (Months)") + 
  ylab("Count") + 
  xlim(0, 60) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/months_arrival_plot.png", 
       plot = months_arrival_plot)
