## ---- visualise-demographics-age
subset_data_age <- demographics[demographics$age > 18, ] 

subset_data_age <- subset_data_age[!is.na(subset_data_age$age), ]

subset_data_age$age <- as.numeric(subset_data_age$age)

socio_demo_age <- 
  ggplot(subset_data_age, aes(x = age)) +
  geom_histogram(bins = 15, fill = "#4739a2") +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Histogram of age distribution in workshop",
       caption = "Data source: Patient preparedness tool") +
  xlab("Age") + 
  ylab("Count") + 
  xlim(16, 30) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/socio_demo_age.png", 
       plot = socio_demo_age)

## ---- visualise-demographics-sex
subset_data_sex <- demographics

subset_data_sex$sex <- ifelse(subset_data_sex$sex == "female", "Female", 
                              ifelse(subset_data_sex$sex == "male", "Male", 
                                     "Diverse"))

subset_data_sex$count <- ifelse(subset_data_sex$sex == "Female", table(subset_data_sex$sex)["Female"], 
                                ifelse(subset_data_sex$sex == "Male", table(subset_data_sex$sex)["Male"], 
                                       table(subset_data_sex$sex)["Diverse"]))

socio_demo_sex <- 
  ggplot(data = subset_data_sex, aes(x = reorder(sex, count), fill = sex)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = c("#46e7fd", "#e18b22", "#4739a2")) +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Bar chart of sex distribution in workshop",
       caption = "Data source: Patient preparedness tool") +
  xlab("Sex") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.position = "none") 

ggsave("output/socio_demo_sex.png", 
       plot = socio_demo_sex)

## ---- visualise-demographics-major
subset_data_major <- demographics

subset_data_major$major <- ifelse(subset_data_major$major == "computer science", "CS", 
                              ifelse(subset_data_major$major == "chemistry and biotechnology", "CB", 
                                     ifelse(subset_data_major$major == "integrated social and cognitive psychology", "ISCP", 
                                            ifelse(subset_data_major$major == "medicinal chemistry and chemical biology", "MCCB", 
                                                   ifelse(subset_data_major$major == "international business administration", "IBA", 
                                                          ifelse(subset_data_major$major == "global economics and management", "GEM", 
                                                                 "ISCP"))))))

subset_data_major$count <- ifelse(subset_data_major$major == "CS", table(subset_data_major$major)["CS"], 
                                ifelse(subset_data_major$major == "CB", table(subset_data_major$major)["CB"], 
                                       ifelse(subset_data_major$major == "ISCP", table(subset_data_major$major)["ISCP"], 
                                              ifelse(subset_data_major$major == "MCCB", table(subset_data_major$major)["MCCB"], 
                                                     ifelse(subset_data_major$major == "IBA", table(subset_data_major$major)["IBA"], 
                                                            table(subset_data_major$major)["GEM"])))))

labels.major <- c(CS = 'CS = Computer science', 
            CB = 'CB = Chemistry and biotechnology', 
            ISCP = 'ISCP = Integrated social and cognitive psychology', 
            MCCB = 'MCCB = Medicinal chemistry and chemical biology', 
            IBA = 'IBA = International business administration', 
            GEM = 'GEM = Global economics and management')

socio_demo_major <- 
  ggplot(data = subset_data_major, aes(reorder(x = major, count), fill = major)) + 
  geom_bar(key_glyph = draw_key_blank) + 
  scale_fill_manual(values = rep("#4739a2", length(labels.major)), labels = labels.major) +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Bar chart of major enrolled in",
       caption = "Data source: Patient preparedness Tool", 
       fill = "Major") +
  xlab("Major") + 
  ylab("Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/socio_demo_major.png", 
       plot = socio_demo_major)
