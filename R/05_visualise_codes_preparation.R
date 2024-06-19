## ---- visualise-codes-preparation

# preparation-timing
preparation_timing <- codes_sources[[4]]

preparation_timing$name_labels = ifelse(preparation_timing$name == "PTO", 
                                    "Other", preparation_timing$name)

plot_prep_timing <- 
  ggplot(preparation_timing, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of when people prepare",
       caption = "Data source: Patient preparedness tool") +
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

ggsave("output/plot_prep_timing.png", 
       plot = plot_prep_timing)

# preparation-location
preparation_location <- codes_sources[[5]]

preparation_location$name_labels = factor(preparation_location$name, 
                                          levels = c("At home", 
                                                     "On the way", 
                                                     "In the clinic", 
                                                     "Other"))

plot_prep_location <- 
  ggplot(preparation_location, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of where people prepare",
       caption = "Data source: Patient preparedness tool") +
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

ggsave("output/plot_prep_location.png", 
       plot = plot_prep_location)

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

plot_prep_method <- 
  ggplot(preparation_method, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of when people prepare",
       caption = "Data source: Patient preparedness tool") +
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

ggsave("output/plot_prep_method.png", 
       plot = plot_prep_method)

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

plot_prep_reason <- 
  ggplot(preparation_reason, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of why people prepare",
       caption = "Data source: Patient preparedness tool") +
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

ggsave("output/plot_prep_reason.png", 
       plot = plot_prep_reason)

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

plot_prep_assistance <- 
  ggplot(preparation_assistance, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Preparation to see the doctor"), 
       subtitle = "Bar chart of who people get assistance from",
       caption = "Data source: Patient preparedness tool") +
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

ggsave("output/plot_prep_assistance.png", 
       plot = plot_prep_assistance)