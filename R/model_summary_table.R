model_summary_table <- function(model) {
  parametric <- data.frame(summary(model)[22])
  parametric <- rownames_to_column(parametric, "Term")
  colnames(parametric) <- c("1", "2", "3", "4", "5")
  parametric <- rbind(c("Parametric term", "Estimate", "Std. Error", "Z-Value", "P-Value"), parametric)
  smooths <- data.frame(summary(model)[24])
  smooths <- rownames_to_column(smooths, "Term")
  colnames(smooths) <- c("1", "2", "3", "4", "5")
  smooths <- rbind(c("Smooth terms", "EDF", "Ref DF", "Chi.sq", "P-Value"), smooths)
  model.terms <- rbind(parametric, smooths)
  rows = c(2, 4:11)
  for (i in rows) {
    model.terms[i, 2:5] <- round(as.numeric(model.terms[i, 2:5]), 3)
  }
  model_terms <- model.terms %>% 
    mutate(`5` = case_when(
      `5` == "0" ~ "<0.001",
      TRUE ~ `5`
    ))
  return(model_terms)
}

