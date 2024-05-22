## ---- visualise-experience

# enjoyment
w1_enjoyment <- mean(workshop_one_exp$Enjoyment)
w2_enjoyment <- mean(workshop_two_exp$Enjoyment)
w3_enjoyment <- mean(workshop_three_exp$Enjoyment)
w4_enjoyment <- mean(workshop_four_exp$Enjoyment)

# participation
w1_participation <- mean(workshop_one_exp$Participation)
w2_participation <- mean(workshop_two_exp$Participation)
w3_participation <- mean(workshop_three_exp$Participation)
w4_participation <- mean(workshop_four_exp$Participation)

# knowledge
w1_knowledge <- mean(workshop_one_exp$Knowledge)
w2_knowledge <- mean(workshop_two_exp$Knowledge)
w3_knowledge <- mean(workshop_three_exp$Knowledge)
w4_knowledge <- mean(workshop_four_exp$Knowledge)

# equality
w1_equality <- mean(workshop_one_exp$Equality)
w2_equality <- mean(workshop_two_exp$Equality)
w3_equality <- mean(workshop_three_exp$Equality)
w4_equality <- mean(workshop_four_exp$Equality)

# create a dataframe
experience_likert <- data.frame(
  workshop = c(1, 2, 3, 4), 
  enjoyment = c(w1_enjoyment, 
                w2_enjoyment, 
                w3_enjoyment, 
                w4_enjoyment), 
  participation = c(w1_participation, 
                    w2_participation, 
                    w3_participation, 
                    w4_participation), 
  knowledge = c(w1_knowledge, 
                w2_knowledge, 
                w3_knowledge, 
                w4_knowledge), 
  equality = c(w1_equality, 
               w2_equality, 
               w3_equality, 
               w4_equality))

# transform data
experience_likert_long <- melt(setDT(experience_likert), 
                               id.vars = "workshop", 
                               variable.name = "feature")

# plot scores
ggplot(experience_likert_long, aes(x = workshop, 
                                   y = value, 
                                   color = feature)) +
  geom_line() + 
  labs(title = paste0("Co-creator experiences"), 
       subtitle = "Linechart of mean scores across workshops",
       caption = "Data source: COT project") +
  xlab("Workshop number") + 
  ylab("Scores") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )
