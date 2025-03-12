## ---- visualise-codes-teambaby

# teambaby-retain
tb_retain <- codes_sources[[13]]

tb_retain$name_labels <- 
  ifelse(tb_retain$name == "RTO", "Other", 
         ifelse(tb_retain$name == "Functionality R", 
                "Functionality", 
                ifelse(tb_retain$name == "Content R", 
                       "Content", 
                       ifelse(tb_retain$name == "Design R", 
                              "Design", 
                              ifelse(tb_retain$name == "Handling data R", 
                                     "Data", "Web-app")))))

tb_retain$name_labels = factor(tb_retain$name_labels, 
                                      levels = c("Functionality", 
                                                 "Content", 
                                                 "Design", 
                                                 "Data", 
                                                 "Web-app",
                                                 "Other"))

plot_tb_retain <- 
  ggplot(tb_retain, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 20) +
  labs(title = paste0("Suitability of DHI A"), 
       subtitle = "Bar chart of elements of DHI A to retain",
       caption = "Data source: Patient preparedness tool") +
  xlab("DHI A feature") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/plot_tb_retain.png", 
       plot = plot_tb_retain)

# teambaby-modify
tb_modify <- codes_sources[[14]]

tb_modify$name_labels <- 
  ifelse(tb_modify$name == "MTO", "Other", 
         ifelse(tb_modify$name == "Functionality M", 
                "Functionality", 
                ifelse(tb_modify$name == "Content M", 
                       "Content", 
                       ifelse(tb_modify$name == "Design M", 
                              "Design", 
                              ifelse(tb_modify$name == "Handling data M", 
                                     "Data", "Web-app")))))

tb_modify$name_labels = factor(tb_modify$name_labels, 
                               levels = c("Functionality", 
                                          "Content", 
                                          "Design", 
                                          "Data", 
                                          "Web-app",
                                          "Other"))

plot_tb_modify <- 
  ggplot(tb_modify, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 50) +
  labs(title = paste0("Suitability of DHI A"), 
       subtitle = "Bar chart of elements of DHI A to modify",
       caption = "Data source: Patient preparedness tool") +
  xlab("DHI A feature") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/plot_tb_modify.png", 
       plot = plot_tb_modify)
