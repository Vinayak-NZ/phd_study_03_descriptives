## ---- visualise-hwc-only

comm_hwc_only_plot <- 
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

ggsave("output/comm_hwc_only_plot.png", 
       plot = comm_hwc_only_plot)

## ---- visualise-patient-only

comm_patient_only_plot <- 
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

ggsave("output/comm_patient_only_plot.png", 
       plot = comm_patient_only_plot)

## ---- visualise-both-comm

comm_both_plot <- 
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

ggsave("output/comm_both_plot.png", 
       plot = comm_both_plot)

## ---- visualise-comm-misunderstood

comm_misunderstood_plot <- 
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

ggsave("output/comm_misunderstood_plot.png", 
       plot = comm_misunderstood_plot)

## ---- visualise-comm-listened

comm_listened_plot <- 
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

ggsave("output/comm_listened_plot.png", 
       plot = comm_listened_plot)