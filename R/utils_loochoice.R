loo_choice <- function(df, thresh) {
  # Ensure the tibble has the expected structure
  if (!all(c("model_names", "loo") %in% colnames(df))) {
    stop("The tibble must have columns 'model_names' and 'loo'")
  }
  min_loo <- min(df$loo)

  #models within 2 units of the min loo
  candidate_models <- df$model_names[df$loo - min_loo <= thresh]
  # If there are competing models, choose based on parsimony
  if (length(candidate_models) > 1 &
      "poisson" %in% candidate_models) {
    # If poisson is among candidates, choose it (most parsimonious)
    "poisson"
  }
  # If both zip and negbinom are candidates (equally complex), return the one with lower loo
  else {
    candidate_loos <- df$loo[df$model_names %in% candidate_models]
    candidate_names <- df$model_names[df$model_names %in% candidate_models]
    candidate_names[which.min(candidate_loos)]
  }
}
