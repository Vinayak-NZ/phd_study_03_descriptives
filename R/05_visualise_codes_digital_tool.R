## ---- visualise-codes-digital-tool

# inclusion-needs
dt_inclusion <- codes_sources[[11]]

dt_inclusion$name_labels <- 
  ifelse(dt_inclusion$name == "Include other", 
         "Other", 
         ifelse(dt_inclusion$name == "Include empowerment", 
                "Empowerment", 
                ifelse(dt_inclusion$name == "Include education", 
                       "Education", 
                       ifelse(dt_inclusion$name == "Include search", 
                              "Search", 
                              ifelse(dt_inclusion$name == "Include accessibility", 
                                     "Accessibility", 
                                     ifelse(dt_inclusion$name == "Include communication", 
                                            "Communication", 
                                            ifelse(dt_inclusion$name == "Include monitoring", 
                                                   "Monitoring", 
                                                   ifelse(dt_inclusion$name == "Include video", 
                                                          "Video", 
                                                          ifelse(dt_inclusion$name == "Include text", 
                                                                 "Text", 
                                                                 ifelse(dt_inclusion$name == "Include native-app", 
                                                                        "Native-app", 
                                                                        ifelse(dt_inclusion$name == "Include web-app", 
                                                                               "Web-app", "Interactivity")))))))))))

dt_inclusion$name_labels = factor(dt_inclusion$name_labels, 
                              levels = c("Empowerment", 
                                         "Education", 
                                         "Search", 
                                         "Accessibility", 
                                         "Communication", 
                                         "Monitoring", 
                                         "Video", 
                                         "Text", 
                                         "Native-app", 
                                         "Web-app", 
                                         "Interactivity", 
                                         "Other"))

ggplot(dt_inclusion, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  coord_flip() +
  labs(title = paste0("Digital health solutions"), 
       subtitle = "Bar chart of features to include in the digital health tool",
       caption = "Data source: COT project") +
  xlab("Features") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

# exclusion-needs
dt_exclusion <- codes_sources[[12]]

dt_exclusion$name_labels <- 
  ifelse(dt_exclusion$name == "Exclude other", 
         "Other", 
         ifelse(dt_exclusion$name == "Exclude empowerment", 
                "Empowerment", 
                ifelse(dt_exclusion$name == "Exclude education", 
                       "Education", 
                       ifelse(dt_exclusion$name == "Exclude search", 
                              "Search", 
                              ifelse(dt_exclusion$name == "Exclude accessibility", 
                                     "Accessibility", 
                                     ifelse(dt_exclusion$name == "Exclude communication", 
                                            "Communication", 
                                            ifelse(dt_exclusion$name == "Exclude monitoring", 
                                                   "Monitoring", 
                                                   ifelse(dt_exclusion$name == "Exclude video", 
                                                          "Video", 
                                                          ifelse(dt_exclusion$name == "Exclude text", 
                                                                 "Text", 
                                                                 ifelse(dt_exclusion$name == "Exclude native-app", 
                                                                        "Native-app", 
                                                                        ifelse(dt_exclusion$name == "Exclude web-app", 
                                                                               "Web-app", "Interactivity")))))))))))

dt_exclusion$name_labels = factor(dt_exclusion$name_labels, 
                                  levels = c("Empowerment", 
                                             "Education", 
                                             "Search", 
                                             "Accessibility", 
                                             "Communication", 
                                             "Monitoring", 
                                             "Video", 
                                             "Text", 
                                             "Native-app", 
                                             "Web-app", 
                                             "Interactivity", 
                                             "Other"))

ggplot(dt_exclusion, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  coord_flip() +
  labs(title = paste0("Digital health tool"), 
       subtitle = "Bar chart of features to exclude in the digital health tool",
       caption = "Data source: COT project") +
  xlab("Features") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

# tool-utility
tool_utility <- codes_sources[[15]]


tool_utility$name_labels <- 
  ifelse(tool_utility$name == "UTO", 
         "Other", tool_utility$name)

tool_utility$name_labels = factor(tool_utility$name_labels, 
                                  levels = c("No use", 
                                             "One time use", 
                                             "Important consultations", 
                                             "All consultations", 
                                             "Conditional use",
                                             "Other"))

ggplot(tool_utility, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Digital health tool"), 
       subtitle = "Bar chart of utility of tool",
       caption = "Data source: COT project") +
  xlab("Expected usage") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 
