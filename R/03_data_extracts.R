## ---- extract-demographics

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

## ---- extract-health-info

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

## ---- extract-waiting-room-survey

# extract people interacted with
waiting_room_visit$people_interaction <- gsub(" ", "", 
                                              waiting_room_visit$people_interaction, 
                                              fixed = TRUE)

waiting_room_list <- regmatches(waiting_room_visit$people_interaction, 
                                gregexpr("(?<=\\{)[^{}]+(?=\\})", 
                                         waiting_room_visit$people_interaction, 
                                         perl=TRUE))

waiting_room_list_edited <- lapply(waiting_room_list, strsplit, ",")

waiting_room_interactions <- unlist(waiting_room_list_edited)

# extract respondents for people interacted with
wri_number_responses_list <- lapply(waiting_room_list_edited, 
                                    function(x) {length(unlist(x))})

wri_number_responses <- unlist(wri_number_responses_list)

wri_respondents_match <- c()

for(i in 1:nrow(waiting_room_exp)){
  
  x <- rep(paste0("CC", i), wri_number_responses[[i]])
  
  wri_respondents_match <- c(wri_respondents_match, x)
  
}