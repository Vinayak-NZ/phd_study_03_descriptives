## ---- visualise-codes-needs

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

plot_responsibility <- 
  ggplot(responsibility, 
         aes(reorder(name_labels, 
                     name_labels, 
                     function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  ylim(0, 20) +
  labs(title = paste0("Needs of patients"), 
       subtitle = "Bar chart of assumptions on responsibility",
       caption = "Data source: Patient preparedness tool") +
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

ggsave("output/plot_responsibility.png", 
       plot = plot_responsibility)

# needs-complaints
needs_complaints <- codes_sources[[1]]

needs_complaints$name_labels <- 
  ifelse(needs_complaints$name == "NCO", "Other", 
         ifelse(needs_complaints$name == "Engagement and inquiry", 
                "Engagement", 
                ifelse(needs_complaints$name == "Limited listening and time", 
                       "Listening and time", needs_complaints$name)))

needs_complaints$name_labels = factor(needs_complaints$name_labels, 
                                        levels = c("Sufficient information", 
                                                   "Clear information", 
                                                   "Language", 
                                                   "Health literacy", 
                                                   "Courtesy", 
                                                   "Dignity in care", 
                                                   "Engagement", 
                                                   "Feeling pressured", 
                                                   "Stigmatisation", 
                                                   "Confidentiality", 
                                                   "Tailored", 
                                                   "Listening and time", 
                                                   "Racist attitudes", 
                                                   "Other"))

plot_needs <- 
  ggplot(needs_complaints, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 50) +
  coord_flip() +
  labs(title = paste0("Needs of patients"), 
       subtitle = "Bar chart of needs and complaints in a healthcare setting",
       caption = "Data source: Patient preparedness tool") +
  xlab("Need/ complaint") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/plot_needs.png", 
       plot = plot_needs)

# system-challenges
system_challenges <- codes_sources[[2]]

system_challenges$name_labels <- 
  ifelse(system_challenges$name == "SCO", "Other", system_challenges$name)

system_challenges$name_labels = factor(system_challenges$name_labels, 
                                      levels = c("Wait time", 
                                                 "Booking an appointment", 
                                                 "Digitisation", 
                                                 "Prescriptions", 
                                                 "Insurance", 
                                                 "Insufficient care", 
                                                 "Test results", 
                                                 "Navigating system", 
                                                 "Other"))

plot_sys_challenges <- 
  ggplot(system_challenges, 
       aes(reorder(name_labels, 
                   name_labels, 
                   function(x)-length(x)))) + 
  geom_bar(fill = "#4739a2") + 
  scale_x_discrete(drop=FALSE) +
  ylim(0, 30) +
  coord_flip() +
  labs(title = paste0("Needs of patients"), 
       subtitle = "Bar chart of healthcare system challenges in Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("System challenge") + 
  ylab("Frequency of codes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/plot_sys_challenges.png", 
       plot = plot_sys_challenges)