## ---- visualise-health-underlying-conditions-visits

health_condition_visit_plot <- 
  ggplot(visits_condition_input_table, 
         aes(health_condition, 
             visits, 
             fill = Count)) +
  geom_tile(color = "#FFFFFF",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = Count), color = "#FFFFFF", size = 4) +
  scale_x_discrete(labels = c("yes" = "Yes", "no" = "No")) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#e18b22",
                       mid = "#46e7fd",
                       high = "#4739a2", 
                       labels = c("0", "2", "4", "6", "8")) +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Heatmap of doctor visits and underlying conditions",
       caption = "Data source: Patient preparedness tool") +
  xlab("Diagnosed with illness/ disorder") + 
  ylab("Number of visits") +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", size = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/health_condition_visit_plot.png", 
       plot = health_condition_visit_plot)

## ---- visualise-icd-conditions

labels.icd <- c(D = 'D = Disorders of blood or blood forming organs', 
            E = 'E = Endocrine, nutritional or metabolic diseases', 
            F = 'F = Mental or behavioural disorders', 
            J = 'J = Respiratory system disorders', 
            M = 'M = Diseases of musculoskeletal system and connective tisue', 
            T = 'T = Injuries, poisoning and other consequences of external causes')

diagnosed_icd_plot <- 
  ggplot(data = health_conditions, 
       aes(x = reorder(condition_level_one, count), fill = condition_level_one)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = rep("#4739a2", length(labels.icd)), labels = labels.icd) +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Bar chart of conditions reported in group",
       caption = "Data source: Patient preparedness tool", 
       fill = "ICD codes") +
  xlab("International Classification \nof Disease (ICD) codes") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(color = NA), 
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt"))

ggsave("output/diagnosed_icd_plot.png", 
       plot = diagnosed_icd_plot)