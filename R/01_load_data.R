## ---- data-load

demographics <- read.csv("input/workshop_04_demographics.csv", sep = ";")
health_related_information <- read.csv("input/workshop_04_health_info.csv", sep = ";")

workshop_one_exp <- read.csv("input/workshop_01_co_creator_experience_survey.csv", sep = ",")
workshop_two_exp <- read.csv("input/workshop_02_co_creator_experience_survey.csv", sep = ",")
workshop_three_exp <- read.csv("input/workshop_03_co_creator_experience_survey.csv", sep = ",")
workshop_four_exp <- read.csv("input/workshop_04_co_creator_experience_survey.csv", sep = ",")

communication_exp <- read.csv("input/workshop_01_communication_survey.csv", sep = ";")
waiting_room_exp <- read.csv("input/workshop_02_waiting_room_survey.csv", sep = ";")
