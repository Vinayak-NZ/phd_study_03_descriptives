## ---- format-codes

# subset-text-extracts
text_codes <- list(needs_complaints, 
                   system_challenges, 
                   responsibility_assumptions, 
                   preparation_timing, 
                   preparation_location, 
                   preparation_method, 
                   preparation_reason, 
                   preparation_assistance, 
                   emotions, 
                   thoughts, 
                   digital_tool_inclusion, 
                   digital_tool_exclusion, 
                   retaining_teambaby, 
                   modifying_teambaby, 
                   utility)

text_codes_sub <- lapply(text_codes, subset_qualcoder_text)

# subset-image-extracts
image_codes_sub <- subset_qualcoder_image(image_codes_all)

# collate-across-sources
codes_sources <- lapply(text_codes, collate_qualcoder_output, image_codes_all)
