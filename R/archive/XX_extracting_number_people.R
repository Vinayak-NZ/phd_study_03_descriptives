test <- lapply(country_list_edited, function(x) {length(unlist(x))})

test_two <- unlist(test)

test_par <- c()

for(i in 1:nrow(demographics)){
  
  x <- rep(paste0("CC", i), test_two[[i]])
  
  test_par <- c(test_par, x)
  
}