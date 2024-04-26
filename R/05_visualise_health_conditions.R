## ---- visualise-health-underlying-conditions

health_related_information$count <- 
  ifelse(health_related_information$pre_existing_conditions == "yes", 
         table(health_related_information$pre_existing_conditions)["yes"], 
         table(health_related_information$pre_existing_conditions)["no"])

underlying_condition_plot <- 
  ggplot(data = health_related_information, 
       aes(x = reorder(pre_existing_conditions, count), 
           fill = pre_existing_conditions)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = c("#46e7fd", "#4739a2")) +
  scale_x_discrete(labels = c("yes" = "Yes", "no" = "No")) +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Bar chart of pre-existing/ underlying conditions within group",
       caption = "Data source: Patient preparedness tool") +
  xlab("Pre-existing/ underlying conditions") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none") 

ggsave("output/underlying_condition_plot.png", 
       plot = underlying_condition_plot)

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
  xlab("International Classification of Disease (ICD) codes") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt"))

ggsave("output/diagnosed_icd_plot.png", 
       plot = diagnosed_icd_plot)

## ---- visualise-doctor-visits

doctor_visit_plot <- 
  ggplot(data = health_related_information, 
       aes(x = factor(visits_standardised, level = visits_standardised_order), 
           fill = visits_standardised)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = rep("#4739a2", length(table(health_related_information$visits_standardised)))) +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Bar chart of conditions reported in group",
       caption = "Data source: Patient preparedness tool", 
       fill = "ICD codes") +
  xlab("International Classification of Disease (ICD) codes") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none")

ggsave("output/doctor_visit_plot.png", 
       plot = doctor_visit_plot)