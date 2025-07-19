## ---- visualise-responsibility

commnication_exp_subset <- 
  communication_exp[, c("id", "comm_hcw", "comm_pat", "both")]

setDT(commnication_exp_subset)

commnication_exp_subset_long <- 
  melt(test, id.vars = "id", 
       measure.vars = c("comm_hcw", "comm_pat", "both"), 
       variable.name = "responsibility", 
       value.name = "valence")

commnication_exp_subset_long$valence <- 
  factor(commnication_exp_subset_long$valence, levels = c('Strongly agree', 
                                     'Agree', 
                                     'Neutral', 
                                     'Disagree', 
                                     'Strongly disagree'))
comm_resp_all_plot <- 
ggplot(commnication_exp_subset_long, 
       aes(x = responsibility, 
           fill = valence)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#000000", 
                                 "#4739a2", 
                                 "#46e7fd", 
                                 "#fefe62", 
                                 "#e18b22")) + 
  scale_x_discrete(labels= c("Healthcare worker only", 
                             "Patient only", 
                             "Both")) + 
  scale_y_continuous(limits = c(0, 12), 
                     breaks = c(0, 3, 6, 9, 12)) +
  labs(title = paste0("Responsibility of effective communication"), 
       subtitle = "Bar chart of responses on who is responsible for effective communication",
       caption = "Data source: Patient preparedness tool", 
       fill = "Likert scale") +
  xlab("Responsibility") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"))

ggsave("output/comm_resp_all_plot.png", 
       plot = comm_resp_all_plot)

## ---- visualise-comm-misunderstood-listened

misunderstood_listened_plot <- 
  ggplot(communication_exp_input, 
         aes(misunderstood, 
             listened, 
             fill = Count)) +
  geom_tile(color = "#FFFFFF",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = Count), color = "#FFFFFF", size = 4) +
  coord_fixed() +
  scale_fill_gradient2(low = "#e18b22",
                       mid = "#46e7fd",
                       high = "#4739a2") +
  labs(title = paste0("Communication experiences with doctors"), 
       subtitle = "Heatmap of feeling misunderstood and not being listened to by the physician",
       caption = "Data source: Patient preparedness tool") +
  xlab("Felt misunderstood") + 
  ylab("Listened to by physician") +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", size = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/misunderstood_listened_plot.png", 
       plot = misunderstood_listened_plot)
