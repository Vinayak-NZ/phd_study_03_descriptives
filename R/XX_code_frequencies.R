## ---- subset-code-frequencies

# remove-parent-frequencies
code_frequencies <- 
  code_frequencies[!(code_frequencies$code_tree == code_frequencies$parent_name), ]

# subset-text-extracts
frequency_parent_codes <- 
  list("Needs and complaints", 
       "System challenges", 
       "Assumptions about responsibility", 
       "Preparation timing", 
       "Preparation location", 
       "Preparation method", 
       "Preparation reason", 
       "Preparation assistance", 
       "Emotions whilst waiting for the doctor", 
       "Thoughts whilst waiting for the doctor", 
       "Needs for a digital tool",
       "Feature inclusion", 
       "Feature exclusion", 
       "Retaining TeamBaby", 
       "Modifying TeamBaby", 
       "Utility of tool")

text_codes_sub <- lapply(frequency_parent_codes, 
                         subset_code_frequencies, 
                         code_frequencies)
