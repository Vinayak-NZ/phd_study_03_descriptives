## ---- visualise-codes-preparation

# responsibility-assumptions
responsibility <- codes_sources[[3]]

responsibility$name_labels <- 
  ifelse(responsibility$name == "Primarily doctor", 
         "Doctor", 
         ifelse(responsibility$name == "Primarily patient", 
                "Patient", 
                ifelse(responsibility$name == "Primarily system/ governance/ institutional responsibility", 
                       "System", 
                       ifelse(responsibility$name == "Shared responsibility not further specified", 
                              "Shared", "Other"))))

ggplot(responsibility, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of assumptions on responsibility",
       caption = "Data source: COT project") +
  xlab("Assigning of responsibility") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

# ggsave("output/comm_hwc_only_plot.png", 
#        plot = comm_hwc_only_plot)


# preparation-timing
preparation_timing <- codes_sources[[4]]

preparation_timing$name_labels = ifelse(preparation_timing$name == "PTO", 
                                    "Other", preparation_timing$name)

ggplot(preparation_timing, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of when people prepare",
       caption = "Data source: COT project") +
  xlab("Preparation timing") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

# preparation-location
preparation_location <- codes_sources[[5]]

preparation_location$name_labels = factor(preparation_location$name, 
                                          levels = c("At home", "On the way", "Other"))

ggplot(preparation_location, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of where people prepare",
       caption = "Data source: COT project") +
  xlab("Preparation location") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 



# preparation-method
preparation_method <- codes_sources[[6]]

preparation_method$name_labels <- ifelse(preparation_method$name == "PMO", 
                                         "Other", 
                                         preparation_method$name)

preparation_method$name_labels = factor(preparation_method$name_labels, 
                                          levels = c("Taking notes", 
                                                     "Mental review", 
                                                     "Language translation", 
                                                     "Research", 
                                                     "No preparation", 
                                                     "Other"))

ggplot(preparation_method, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of when people prepare",
       caption = "Data source: COT project") +
  xlab("Preparation method") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 


# preparation-reason
preparation_reason <- codes_sources[[7]]

preparation_reason$name_labels <- ifelse(preparation_reason$name == "PRO", 
                                         "Other", 
                                         preparation_reason$name)

preparation_reason$name_labels = factor(preparation_reason$name_labels, 
                                        levels = c("Prompt doctor", 
                                                   "Efficient communication", 
                                                   "Psychological safety", 
                                                   "Other"))

ggplot(preparation_reason, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of why people prepare",
       caption = "Data source: COT project") +
  xlab("Preparation reason") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

# preparation-assistance
preparation_assistance <- codes_sources[[8]]

preparation_assistance$name_labels <- ifelse(preparation_assistance$name == "PAO", 
                                         "Other", 
                                         preparation_assistance$name)

preparation_assistance$name_labels = factor(preparation_assistance$name_labels, 
                                        levels = c("Friends", 
                                                   "Partner or family", 
                                                   "No assistance", 
                                                   "Other"))

ggplot(preparation_assistance, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of who people get assistance from",
       caption = "Data source: COT project") +
  xlab("Preparation assistance") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 
