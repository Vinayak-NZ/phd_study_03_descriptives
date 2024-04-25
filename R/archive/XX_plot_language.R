## ---- plot-language-proficiency

# extract languages spoken
demographics$language <- gsub(" ", "", 
                              demographics$language, 
                              fixed = TRUE)

language_list <- regmatches(demographics$language, 
                            gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                                     demographics$language, 
                                     perl=TRUE))

language_list_edited <- lapply(language_list, strsplit, ",")

languages_spoken <- unlist(language_list_edited)

# extract language proficiency
demographics$language_proficiency <- gsub(" ", "", 
                                          demographics$language_proficiency, 
                                          fixed = TRUE)

language_proficiency_list <- regmatches(demographics$language_proficiency, 
                            gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                                     demographics$language_proficiency, 
                                     perl=TRUE))

language_proficiency_list_edited <- lapply(language_proficiency_list, strsplit, ",")

languages_spoken_proficiency <- unlist(language_proficiency_list_edited)

# aggregate data
languages_input <- data.frame(language = languages_spoken, 
                        language_proficiency = languages_spoken_proficiency)

languages_input_table <- 
  as.data.frame(table(languages_input$language, 
                      languages_input$language_proficiency))

languages_input_table$language <- languages_input_table$Var1

languages_input_table$language_proficiency <- languages_input_table$Var2

languages_input_table$Count <- languages_input_table$Freq

languages_input_table <- languages_input_table[languages_input_table$Count > 0, ]

# aggregate data
ggplot(languages_input_table, aes(language, language_proficiency)) + 
  geom_point(aes(size = Count), colour = "#4739a2") + 
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Scatterplot of language proficiency in workshop",
       caption = "Data source: Patient preparedness tool") +
  xlab("Language") + 
  ylab("Proficiency") + 
  scale_size_continuous(range = c(1, 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
    )