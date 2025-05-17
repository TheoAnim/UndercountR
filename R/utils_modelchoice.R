model_choice <- function(df, thresh) {
  # Ensure the tibble has the expected structure
  if (!all(c("model_names", "DIC") %in% colnames(df))) {
    stop("The tibble must have columns 'model_names' and 'DIC'")
  }
  min_dic <- min(df$DIC)
  
  #models within 2 units of the min DIC
  candidate_models <- df$model_names[df$DIC - min_dic <= thresh]
  # If there are competing models, choose based on parsimony
  if (length(candidate_models) > 1 &
      "poisson" %in% candidate_models) {
    # If poisson is among candidates, choose it (most parsimonious)
    "poisson"
  }
  # If both zip and negbinom are candidates (equally complex), return the one with lower DIC
  else {
    candidate_dics <- df$DIC[df$model_names %in% candidate_models]
    candidate_names <- df$model_names[df$model_names %in% candidate_models]
    candidate_names[which.min(candidate_dics)]
  }
}
