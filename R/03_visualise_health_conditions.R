## ---- pre-existing-health-conditions

# bar graph of conditions

health_related_information$count <- 
  ifelse(health_related_information$pre_existing_conditions == "yes", 
         table(health_related_information$pre_existing_conditions)["yes"], 
         table(health_related_information$pre_existing_conditions)["no"])

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

# extract specific conditions
health_related_information$pre_existing_conditions_list <- gsub(" ", "", 
                                              health_related_information$pre_existing_conditions_list, 
                                       fixed = TRUE)

conditions_list <- regmatches(health_related_information$pre_existing_conditions_list, 
                           gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                                    health_related_information$pre_existing_conditions_list, 
                                    perl=TRUE))

conditions_list_edited <- lapply(conditions_list, strsplit, ",")

conditions_diagnosed <- unlist(conditions_list_edited)

# convert conditions to high level ICD codes
health_conditions <- data.frame(condition = conditions_diagnosed)

health_conditions$condition_level_one <- 
  ifelse(health_conditions$condition %in% c("asthma", "chronicbronchitis"), "J", 
         ifelse(health_conditions$condition == "allergies", "T", 
         ifelse(health_conditions$condition %in% c("scoliosis", "Herniateddisc"), "M", 
         ifelse(health_conditions$condition == "policysticovariansyndrome", "E", 
         ifelse(health_conditions$condition %in% c("generalisedanxietydisorder", 
                                                   "manic-depressivedisorder", 
                                                   "ADHD", 
                                                   "Panicattackdisorder", 
                                                   "chronicdepression"), "F", 
         "D")))))

health_conditions$count <- 
  ifelse(health_conditions$condition_level_one == "J", 
         table(health_conditions$condition_level_one)["J"], 
         ifelse(health_conditions$condition_level_one == "T", 
                table(health_conditions$condition_level_one)["T"], 
                ifelse(health_conditions$condition_level_one == "M", 
                       table(health_conditions$condition_level_one)["M"], 
                       ifelse(health_conditions$condition_level_one == "E", 
                              table(health_conditions$condition_level_one)["E"], 
                              ifelse(health_conditions$condition_level_one == "F", 
                                     table(health_conditions$condition_level_one)["F"], 
                                     table(health_conditions$condition_level_one)["D"])))))


labels <- c(D = 'D = Disorders of blood or blood forming organs', 
            E = 'E = Endocrine, nutritional or metabolic diseases', 
            F = 'F = Mental or behavioural disorders', 
            J = 'J = Respiratory system disorders', 
            M = 'M = Diseases of musculoskeletal system and connective tisue', 
            T = 'T = Injuries, poisoning and other consequences of external causes')

# bar graph of icd classifications for those with conditions
ggplot(data = health_conditions, 
       aes(x = reorder(condition_level_one, count), fill = condition_level_one)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = rep("#4739a2", length(labels)), labels = labels) +
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

# edit values relating to visits to doctor

health_related_information$visits_standardised <- 
  ifelse(health_related_information$visits %in% c("not frequently cause the response wasn't up to the mark", 
                                                  "1", "1 to 2", "2", "3", "4", "3 to 4"), "< 5", 
         ifelse(health_related_information$visits %in% c("5", "6", "5 to 6"), "5 - 10", "> 10"))

# bar graph of visits
visits_standardised_order <- c('< 5', '5 - 10', '> 10')

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
