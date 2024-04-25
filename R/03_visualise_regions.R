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
  
  x <- rep(paste0("CC", i), cr_number_responses[[i]])
  
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
  
  x <- rep(paste0("CC", i), ch_number_responses[[i]])
  
  ch_respondents_match <- c(ch_respondents_match, x)
  
}

# extract country of birth for countries resided in
cob_number_responses_list <- lapply(country_health_care_list_edited, 
                                   function(x) {length(unlist(x))})

cob_number_responses <- unlist(cob_number_responses_list)

cob_respondents_match <- c()

for(i in 1:nrow(demographics)){
  
  x <- rep(demographics[i, "country_birth"], cob_number_responses[[i]])
  
  cob_respondents_match <- c(cob_respondents_match, x)
  
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

# aggregate data for country of birth and for country received health care in
healthcare_received <- data.frame(region_birth = cob_respondents_match, 
                                  healthcare = country_health_care_received)

healthcare_received$region_birth <- 
  ifelse(healthcare_received$region_birth %in% c("Myanmar", 
                                                 "Cyprus", 
                                                 "India", 
                                                 "Bahrain", 
                                                 "Oman", 
                                                 "Turkey", 
                                                 "Palestine", 
                                                 "Israel"), 
         "Asia", 
         ifelse(healthcare_received$region_birth %in% c("Serbia",
                                                        "Poland", 
                                                        "France"), 
                "Europe", 
                ifelse(healthcare_received$region_birth %in% c("Barbados", 
                                                               "Brazil", 
                                                               "Nicaragua"), 
                       "Americas", NA)))

healthcare_received$healthcare <- 
  ifelse(healthcare_received$healthcare %in% c("Myanmar", 
                                                 "Cyprus", 
                                                 "India", 
                                                 "Bahrain", 
                                                 "Oman", 
                                                 "Turkey", 
                                                 "PalestineWestBank", 
                                                 "Israel"), 
         "Asia", 
         ifelse(healthcare_received$healthcare %in% c("Serbia",
                                                        "Poland", 
                                                        "France"), 
                "Europe", 
                ifelse(healthcare_received$healthcare %in% c("Barbados", 
                                                               "Brazil", 
                                                               "Nicaragua"), 
                       "Americas", NA)))


cob_input_table <- 
  as.data.frame(table(healthcare_received$region_birth, 
                      healthcare_received$healthcare))

cob_input_table$region_birth <- cob_input_table$Var1

cob_input_table$healthcare <- cob_input_table$Var2

cob_input_table$Count <- cob_input_table$Freq

# plot region of birth vs region received care in using heatmap


ggplot(cob_input_table, 
       aes(region_birth, 
           healthcare, 
           fill = Count)) +
  geom_tile(color = "#FFFFFF",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = Count), color = "#FFFFFF", size = 4) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#e18b22",
                       mid = "#46e7fd",
                       high = "#4739a2", 
                       labels = c("0", "2", "4", "6", "8")) +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Heatmap of regions received care in preior to Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("Region of birth") + 
  ylab("Region received health care in") +
  guides(fill = guide_colourbar(title = "Count",
                                barwidth = 0.5,
                                barheight = 20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.grid = element_line(color = "#e18b22", size = 0.2, linetype = 2),
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

## ---- plot-months-since-arrival

demographics$date_workshop <- 
  as.Date("2024-02-06")

demographics$year_arrival_date_estimate <- 
  as.Date(paste0(demographics$year_arrival,"-", "09-01"))

demographics$weeks_arrival <- 
  difftime(demographics$date_workshop, 
           demographics$year_arrival_date_estimate, 
           units = "weeks")

demographics$month_approximation <- 
  (demographics$weeks_arrival/4)

ggplot(demographics, aes(x = month_approximation)) +
  geom_histogram(bins = 7, fill = "#4739a2") +
  labs(title = paste0("Characteristics of collaborators"), 
       subtitle = "Histogram of months since arrival in Germany",
       caption = "Data source: Patient preparedness tool") +
  xlab("Time since arrival (Months)") + 
  ylab("Count") + 
  xlim(0, 60) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )