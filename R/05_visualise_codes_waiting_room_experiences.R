## ---- visualise-codes-wre

# emotions
emotions <- codes_sources[[9]]

emotions$name_labels <- ifelse(emotions$name == "EO", 
                                             "Other", 
                               emotions$name)

emotions$name_labels = factor(emotions$name_labels, 
                              levels = c("Fear or anxiety", 
                                         "Anger or frustration", 
                                         "Sadness", 
                                         "Bored", 
                                         "Positive emotions", 
                                         "Other"))

ggplot(emotions, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Waiting to see the doctor"), 
       subtitle = "Bar chart of emotions whilst waiting",
       caption = "Data source: COT project") +
  xlab("Emotions") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

# thoughts
thoughts <- codes_sources[[10]]

thoughts$name_labels <- ifelse(thoughts$name == "TO", 
                               "Other", thoughts$name)

thoughts$name_labels = factor(thoughts$name_labels, 
                              levels = c("Anticipation or expectations", 
                                         "Planning", 
                                         "Time of appointment", 
                                         "Waiting area observations", 
                                         "Work or study", 
                                         "Other"))

ggplot(thoughts, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  coord_flip() +
  labs(title = paste0("Waiting to see the doctor"), 
       subtitle = "Bar chart of emotions whilst waiting",
       caption = "Data source: COT project") +
  xlab("Emotions") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 
