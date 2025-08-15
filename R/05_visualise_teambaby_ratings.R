## ---- tb-ratings

# time-1-average-scores
t1_lesson <- list()

tb_ratings[["T1"]] <- 0

for (i in 1:9){
  
  t1_lesson[[i]] <- as.numeric(
    unlist(strsplit(gsub("[\\{\\}]", "", tb_ratings[["t1"]][i]), ",")))

  tb_ratings[["T1"]][i] <- mean(t1_lesson[[i]])
  
}

# time-2-average-scores
t2_lesson <- list()

tb_ratings[["T2"]] <- 0

for (i in 1:9){
  
  t2_lesson[[i]] <- as.numeric(
    unlist(strsplit(gsub("[\\{\\}]", "", tb_ratings[["t2"]][i]), ",")))
  
  tb_ratings[["T2"]][i] <- mean(t2_lesson[[i]])
  
}

## ---- visualising-tb-ratings

tb_ratings_visualisation_input <- tb_ratings[, c("id", "T1", "T2")]

tb_ratings_visualisation_input$lesson <- 
  ifelse(
    tb_ratings_visualisation_input$id == "l1", 
    "Lesson 1", 
    ifelse(
      tb_ratings_visualisation_input$id == "l2", 
      "Lesson 2", 
      ifelse(
        tb_ratings_visualisation_input$id == "l3", 
        "Lesson 3", 
        ifelse(
          tb_ratings_visualisation_input$id == "l4", 
          "Lesson 4", 
          ifelse(
            tb_ratings_visualisation_input$id == "l5", 
            "Lesson 5", 
            ifelse(
              tb_ratings_visualisation_input$id == "l6", 
              "Lesson 6", 
              ifelse(
                tb_ratings_visualisation_input$id == "l7", 
                "Lesson 7", 
                ifelse(
                  tb_ratings_visualisation_input$id == "l8", 
                  "Lesson 8", "Lesson 9"))))))))

tb_ratings_visualisation_input <- 
  tb_ratings_visualisation_input[, c("lesson", "T1", "T2")]

setDT(tb_ratings_visualisation_input)

tb_ratings_visualisation_input <- 
  melt(tb_ratings_visualisation_input, 
       id.vars = "lesson", 
       variable.name = "time")

ggplot(tb_ratings_visualisation_input, 
       aes(fill = time, 
           y = value, 
           x = lesson)) + 
  geom_bar(position="dodge", 
           stat="identity") +
  scale_fill_manual(values = c("#e18b22", "#4739a2")) +
  labs(title = paste0("Ratings of DiPPT"), 
       subtitle = "Bar chart of average ratings for each component",
       caption = "Data source: Patient preparedness tool", 
       fill = "Time") +
  xlab("Lesson") + 
  ylab("Average Rating") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) 

