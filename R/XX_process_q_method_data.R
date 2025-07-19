
validate_needs_include <- 
  validate_needs[validate_needs[["original_sample"]] == 0, ]

validate_needs_exclude <- 
  validate_needs_include[!validate_needs_include[["id"]] %in% c("CV09", "CV11"), ]

validate_q_method <- 
  validate_needs_exclude[, c("id", "qm_1", "qm_2", "qm_3", "qm_4", "qm_5", "qm_6", "qm_7")]

setDT(validate_q_method)

test <- melt(validate_q_method, 
             id.vars = c("id"),
             measure.vars = c("qm_1", "qm_2", "qm_3", "qm_4", "qm_5", "qm_6", "qm_7"))

test2 <- test[test[["variable"]] == "qm_1", ]

c_vars <- c("C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", 
            "C11", "C12", "C13", "C14")

qm_replace <- c(-3, -2, -1, 0, 1, 2, 3)

for (i in c_vars) {
  
  test2[[i]] <- ifelse(test2[["value"]] == i, -3, 0)
  
}


test_all <- list()

for (i in 1:7) {
  
  test_all[[i]] <- test[test[["variable"]] == paste0("qm_", i), ]

  for (j in c_vars){
    
    q_val <- qm_replace[i]
    
    test_all[[i]][[j]] <- ifelse(grepl(j, test_all[[i]][["value"]]), q_val, NA)
    
  }
  
}

test3 <- do.call(rbind, test_all) 

test3 <- as.data.frame(test3)

test3_all <- list()

for (i in 1:14){
  
  test3_all[[i]] <- 
    test3[grepl(c_vars[i], test3[["value"]]), c("id", c_vars[i])]
  
}

test4 <- 
  Reduce(function(x, y) merge(x, y, all = TRUE, by = "id"), test3_all)

library(qmethod)

# Run the full model with max factors
test4[is.na(test4)] <- -1
test4 <- test4[!duplicated(test4$id), ]
test5 <- test4[!(test4[["id"]] %in% c("CV03", "CV05")), ]
test5 <- test4[, !(names(test4)=="id")]

result_all <- qmethod(test5, nfactors = 14)

# Extract eigenvalues
eigenvals <- result_all$eig.val

# Scree plot
plot(eigenvals, type = "b", pch = 19,
     xlab = "Number of Factors", ylab = "Eigenvalue",
     main = "Scree Plot")
abline(h = 1, col = "red", lty = 2) 