## ---- process-data

demographics <- 
  data.frame(lapply(demographics, 
                    function(x) {
                      gsub("Native", "C2", x)
                      }
                    ))

demographics <- 
  data.frame(lapply(demographics, 
                    function(x) {
                      gsub("B1.2", "B1", x)
                    }
  ))

demographics <- 
  data.frame(lapply(demographics, 
                    function(x) {
                      gsub("NA", "C1", x)
                    }
  ))

# add observation to demographics based on auxiliary information
data_edit_demographics <- data.frame(id = "d12", 
                                            age = NA, 
                                            sex = "female", 
                                            major = "computer science", 
                                            language = "{English}", 
                                            language_proficiency = "{C2}", 
                                            countries_resided = "{Ghana, Germany}", 
                                            countries_health_care = "{Ghana, Germany}", 
                                            country_birth = "Ghana", 
                                            year_arrival = 2022)

demographics_processed <- rbind(demographics, data_edit_demographics)

# add observation to health related info based on auxiliary information
data_edit_health_related_info <- data.frame(id = "h12", 
                                            pre_existing_conditions = "yes", 
                                            pre_existing_conditions_list = "{asthma}", 
                                            visits = NA)

health_related_information_processed <- rbind(health_related_information, data_edit_health_related_info)
