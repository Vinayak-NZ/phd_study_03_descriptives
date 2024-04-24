## ---- plot-regions-resided

# extract country resided in
demographics$countries_resided <- gsub(" ", "", 
                                       demographics$countries_resided, 
                                       fixed = TRUE)

country_list <- regmatches(demographics$countries_resided, 
                           gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                                    demographics$countries_resided, 
                                    perl=TRUE))

country_list_edited <- lapply(country_list, strsplit, ",")

countries_resided <- unlist(country_list_edited)

# extract respondents for countries resided in
cr_number_responses_list <- lapply(country_list_edited, 
                                function(x) {length(unlist(x))})

cr_number_responses <- unlist(cr_number_responses_list)

cr_respondents_match <- c()

for(i in 1:nrow(demographics)){
  
  x <- rep(paste0("CC", i), test_two[[i]])
  
  cr_respondents_match <- c(cr_respondents_match, x)
  
}

# extract countries received health care in
demographics$countries_health_care <- gsub(" ", "", 
                                           demographics$countries_health_care, 
                                           fixed = TRUE)

country_health_care_list <- 
  regmatches(demographics$countries_health_care, 
             gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                      demographics$countries_health_care, 
                      perl=TRUE))

country_health_care_list_edited <- lapply(country_health_care_list, 
                                          strsplit, ",")

country_health_care_received <- unlist(country_health_care_list_edited)

# extract respondents for countries received healthcare in
ch_number_responses_list <- lapply(country_health_care_list_edited, 
                                function(x) {length(unlist(x))})

ch_number_responses <- unlist(ch_number_responses_list)

ch_respondents_match <- c()

for(i in 1:nrow(demographics)){
  
  x <- rep(paste0("CC", i), test_two[[i]])
  
  ch_respondents_match <- c(ch_respondents_match, x)
  
}

# aggregate data for regions resided in
region_input <- data.frame(region = countries_resided)

region_input$region <- ifelse(region_input$region %in% c("Myanmar", 
                                                         "Cyprus", 
                                                         "India", 
                                                         "Bahrain", 
                                                         "Oman", 
                                                         "Turkey", 
                                                         "PalestineWestBank"), 
                              "Asia", 
                              ifelse(region_input$region %in% c("Serbia",
                                                                "Poland", 
                                                                "France"), 
                                     "Europe", 
                                     ifelse(region_input$region %in% c("Barbados", 
                                                                "Brazil", 
                                                                "Nicaragua"), 
                                            "Americas", NA)))

region_input$co_creator <- cr_respondents_match

region_input$co_creator <- 
  ifelse(
    region_input$co_creator == "CC1", 
         "CC01", 
         ifelse(
           region_input$co_creator == "CC2", 
                "CC02", 
                ifelse(
                  region_input$co_creator == "CC3", 
                       "CC03", 
                       ifelse(
                         region_input$co_creator == "CC4", 
                              "CC04", 
                              ifelse(
                                region_input$co_creator == "CC5", 
                                     "CC05", 
                                     ifelse(
                                       region_input$co_creator == "CC6", 
                                            "CC06", 
                                            ifelse(
                                              region_input$co_creator == "CC7", 
                                                   "CC07", 
                                                   ifelse(
                                                     region_input$co_creator == "CC8", 
                                                          "CC08", 
                                                          ifelse(
                                                            region_input$co_creator == "CC9", 
                                                                 "CC09", 
                                                                 region_input$co_creator)))))))))


region_input <- region_input[!is.na(region_input$region), ]

# plot regions resided in
ggplot(region_input, aes(region, co_creator)) + 
  geom_point(size = 5,colour = "#4739a2") + 
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Scatterplot of regions resided in prior to Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("Region") + 
  ylab("Collaborator") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", size = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )




# plot region of birth vs region received care in using heatmap

# 



# plot regions resided in
ggplot(region_input, aes(region, co_creator)) + 
  geom_point(size = 5,colour = "#4739a2") + 
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Scatterplot of regions resided in prior to Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("Region") + 
  ylab("Collaborator") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", size = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )















country_input_table <- 
  as.data.frame(table(region_input$co_creator, 
                      region_input$region))








country_input_table$co_creator <- country_input_table$Var1

country_input_table$region <- country_input_table$Var2

country_input_table$Count <- country_input_table$Freq

country_input_table <- country_input_table[country_input_table$Count > 0, ]

ggplot(country_input_table, aes(co_creator, region, fill = Count)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed()



country_input <- data.frame(co_creator = test_par, 
                            country_resided = countries_resided)


country_input_table <- 
  as.data.frame(table(country_input$co_creator, 
                      country_input$country_resided))

country_input_table$co_creator <- country_input_table$Var1

country_input_table$country_resided <- country_input_table$Var2

country_input_table$Count <- country_input_table$Freq

country_input_table <- country_input_table[country_input_table$Count > 0, ]

ggplot(country_input_table, aes(co_creator, countries_resided, fill = Count)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed()




length(country_list_edited[[1]])

length(unlist(country_list_edited[1]))



demographics$countries_health_care <- gsub(" ", "", demographics$countries_health_care, fixed = TRUE)

country_health_care_list <- regmatches(demographics$countries_health_care, 
                                        gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                                                 demographics$countries_health_care, 
                                                 perl=TRUE))

country_health_care_list_edited <- lapply(country_health_care_list, strsplit, ",")

country_health_care_received <- unlist(country_health_care_list_edited)


country_input <- data.frame(country_resided = countries_resided, 
                              country_health_care = country_health_care_received)


country_input_table <- 
  as.data.frame(table(country_input$country_resided, 
                           country_input$country_health_care))

country_input_table$countries_resided <- country_input_table$Var1

country_input_table$country_health_care_received <- country_input_table$Var2

country_input_table$Count <- country_input_table$Freq

country_input_table <- country_input_table[country_input_table$Count > 0, ]

ggplot(country_input_table, aes(countries_resided, country_health_care_received, fill = Count)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed()


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