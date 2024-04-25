## ---- visualise-communication-survey

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
                                                          "Agree", "Strongly agree"))))

communication_exp$comm_pat <- ifelse(communication_exp$patient_only == 1, 
                                     "Strongly disagree", 
                                     ifelse(communication_exp$patient_only == 2, 
                                            "Disagree", 
                                            ifelse(communication_exp$patient_only == 3, 
                                                   "Neutral", 
                                                   ifelse(communication_exp$patient_only == 4, 
                                                          "Agree", "Strongly agree"))))

communication_exp$both <- ifelse(communication_exp$both == 1, 
                                     "Strongly disagree", 
                                     ifelse(communication_exp$both == 2, 
                                            "Disagree", 
                                            ifelse(communication_exp$both == 3, 
                                                   "Neutral", 
                                                   ifelse(communication_exp$both == 4, 
                                                          "Agree", "Strongly agree"))))

# specify order for plotting
likert_standardised_order <- c('Strongly disagree', 
                               'Disagree', 
                               'Neutral', 
                               'Agree', 
                               'Strongly agree')

# bar graph of health care worker only
ggplot(data = communication_exp, 
       aes(x = factor(comm_hcw, level = likert_standardised_order), 
           fill = comm_hcw)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = rep("#4739a2", length(likert_standardised_order))) +
  labs(title = paste0("Responsibility of effective communication"), 
       subtitle = "Bar chart of responses on whether the healthcare worker is only responsible",
       caption = "Data source: Patient preparedness tool") +
  xlab("Response categories") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none")

# bar graph of patient only
ggplot(data = communication_exp, 
       aes(x = factor(comm_pat, level = likert_standardised_order), 
           fill = comm_pat)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = rep("#4739a2", length(likert_standardised_order))) +
  labs(title = paste0("Responsibility of effective communication"), 
       subtitle = "Bar chart of responses on whether the patient is only responsible",
       caption = "Data source: Patient preparedness tool") +
  xlab("Response categories") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none")

# bar graph of both
ggplot(data = communication_exp, 
       aes(x = factor(both, level = likert_standardised_order), 
           fill = both)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = rep("#4739a2", length(likert_standardised_order))) +
  labs(title = paste0("Responsibility of effective communication"), 
       subtitle = "Bar chart of responses on whether both are responsible",
       caption = "Data source: Patient preparedness tool") +
  xlab("Response categories") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none")

# bar graph of misunderstood
misunderstood_order <- c('Not sure', 
                         'No', 
                         'Yes')

ggplot(data = communication_exp, 
       aes(x = factor(misunderstood, level = misunderstood_order), fill = misunderstood)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = rep("#4739a2", length(misunderstood_order))) +
  labs(title = paste0("Communication experiences with doctors"), 
       subtitle = "Bar chart of responses relating to feeling misunderstood",
       caption = "Data source: Patient preparedness tool") +
  xlab("Felt misunderstood") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none")

# bar graph of listening gaps
listening_gaps_order <- c('Not sure ', 
                         'No', 
                         'Yes')

ggplot(data = communication_exp[!is.na(communication_exp$listened), ], 
       aes(x = factor(listened, level = listening_gaps_order), fill = listened)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = rep("#4739a2", length(misunderstood_order))) +
  labs(title = paste0("Communication experiences with doctors"), 
       subtitle = "Bar chart of responses relating to feeling not being listened to",
       caption = "Data source: Patient preparedness tool") +
  xlab("Felt not listened to by doctor") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none")
