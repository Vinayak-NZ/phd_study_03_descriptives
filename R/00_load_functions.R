## ---- function-load

# subset-qualcoder-text-output
subset_qualcoder_text <- function(data){
  
  output <- data[, c("caseid", "fid", "cid", "catid", "name", "seltext")]
  
  return(output)
  
}

# subset-qualcoder-image-output
subset_qualcoder_image <- function(data){
  
  output <- data[, c("catid", "cid", "name")]
  
  return(output)
  
}

# collote-qualcoder-output
collate_qualcoder_output <- function(text_extracts, image_extracts){
  
  subset_text_extracts <- text_extracts[, c("catid", "cid", "name")]
  
  catid_marker <- subset_text_extracts[1, "catid"]
  
  subset_image_extracts <- image_extracts[image_extracts$catid == catid_marker, 
                                          c("catid", "cid", "name")]
  
  output <- rbind(subset_text_extracts, subset_image_extracts)
  
  return(output)
  
}
