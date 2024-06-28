## ---- data-load

demographics <- read.csv("input/workshop_04_demographics.csv", sep = ";")
health_related_information <- read.csv("input/workshop_04_health_info.csv", sep = ";")

workshop_one_exp <- read.csv("input/workshop_01_co_creator_experience_survey.csv", sep = ",")
workshop_two_exp <- read.csv("input/workshop_02_co_creator_experience_survey.csv", sep = ",")
workshop_three_exp <- read.csv("input/workshop_03_co_creator_experience_survey.csv", sep = ",")
workshop_four_exp <- read.csv("input/workshop_04_co_creator_experience_survey.csv", sep = ",")

communication_exp <- read.csv("input/workshop_01_communication_survey.csv", sep = ";")
waiting_room_exp <- read.csv("input/workshop_02_waiting_room_survey.csv", sep = ";")

needs_complaints <- read.csv("input/needs_complaints.csv", sep = "\t")
system_challenges <- read.csv("input/system_challenges.csv", sep = "\t")
responsibility_assumptions <- read.csv("input/responsibility_assumptions.csv", sep = "\t")
preparation_timing <- read.csv("input/preparation_timing.csv", sep = "\t")
preparation_location <- read.csv("input/preparation_location.csv", sep = "\t")
preparation_method <- read.csv("input/preparation_method.csv", sep = "\t")
preparation_reason <- read.csv("input/preparation_reason.csv", sep = "\t")
preparation_assistance <- read.csv("input/preparation_assistance.csv", sep = "\t")
emotions <- read.csv("input/emotions.csv", sep = "\t")
thoughts <- read.csv("input/thoughts.csv", sep = "\t")
digital_tool_inclusion <- read.csv("input/digital_tool_inclusion.csv", sep = "\t")
digital_tool_exclusion <- read.csv("input/digital_tool_exclusion.csv", sep = "\t")
retaining_teambaby <- read.csv("input/retaining_teambaby.csv", sep = "\t")
modifying_teambaby <- read.csv("input/modifying_teambaby.csv", sep = "\t")
utility <- read.csv("input/utility.csv", sep = "\t")
image_codes_all <- read.csv("input/image_codes_all.csv", sep = "\t")

code_frequencies <- read.csv("input/code_frequencies.csv", sep = ",")
