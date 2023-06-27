# Functions for post-stratifying by party
party_mrp_preds <- function(models, 
                            post_strat, 
                            weights, 
                            ndraws,
                            .cores = 1,
                            ...) {
  
  # Coerce the post-stratification table to a data table
  post_strat <- data.table(post_strat)
  
  # Set names to trials
  setnames(post_strat, old = "n", new = "trials")
  
  # Construct a vector of draw weights
  weighted_draws <- round_largest_remainder(weights * ndraws)
  
  # Generate the posterior predictions
  out <- lapply(seq_along(models), function(x) {
    
    # Generate posterior predictions using the weighted draws
    draws <- posterior_predict(
      models[[x]],
      ndraws = weighted_draws[x],
      newdata = post_strat,
      allow_new_levels = TRUE,
      sample_new_levels = "gaussian",
      cores = .cores,
      ...
    )
    
    # Get the predictions returned by brms in long form
    draws <- apply(draws, 1, function(x){
      as.vector(t(x))
    })
    
    # Return the predicted draws
    return(draws)
  })
  
  # Bind the predictions into a single matrix
  out <- do.call("cbind", out)
  
  # Number of levels in the response
  K <- c("Independent", "Other/Don't Know", "Republican", "Democrat")
  
  # Expand the post-stratification table by each level
  post_strat_party <- post_strat[
    rep(seq_len(nrow(post_strat)), each = length(K)), 
  ]
  
  # Add a column for party to the data
  post_strat_party[, partisan := rep(K, nrow(post_strat))]
  
  # Calculate the posterior median and prediction intervals since this
  # is more computationally efficient than working with the full draws matrix
  out <- apply(out, 1, function(x) {
    data.table(
      .pred_med = median(x),
      .pred_lo = quantile(x, probs = cred[1]),
      .pred_hi = quantile(x, probs = cred[2])
    )})
  
  # Bind the rows into a single list
  out <- rbindlist(out)
  
  # Bind the post-strat table and predictions together
  out <- cbind(post_strat_party, out)
  
  # Return the post-stratification table with party included
  return(out)
}



# Functions for post-stratifying by linked fate
lfate_mrp_preds <- function(model, 
                            post_strat, 
                            dimension, 
                            ndraws,
                            .cores = 1,
                            ...) {
  
  # Set the names of the predicitons from the partisan population estimates
  pred_names <- c(".pred_med", ".pred_25PI", ".pred_75PI")
  
  # Round the number of trials to ensure integer values
  post_strat[, (pred_names) := lapply(.SD, round),
             .SDcols = pred_names]
  
  # Recode the partisanship vector
  post_strat[, partisan := fcase(
    partisan == "indep", "Independent",
    partisan == "otdk", "Other/Don't Know",
    partisan == "repub", "Republican",
    partisan == "demo", "Democrat"
  )]
  
  # Generate data tables for each of the trial estimates
  out <- lapply(seq_along(pred_names), function(x) {
    
    # Subset just the required columns to reduce memory requirements
    cols <- c("trials", "ancestry", "female", "age_cat", "citizen", "educ", 
              "region", "pct_college_z", "partisan", pred_names[x])
    
    # Subset the relevant columns
    out = post_strat[, ..cols]
    
    # Set names for the median predictions
    setnames(
      out, 
      old = c("trials", pred_names[x]), 
      new = c("pop_totals", "trials")
    )
  })
  
  # Generate posterior predictions
  draws <- lapply(out, function(x) {
    
    # Generate the posterior predicitons, this takes a long time so if possible
    # set .cores as high as possible. Memory using 8 cores peaks at around 24GB
    draws <- posterior_predict(
      model,
      newdata = x,
      ndraws = ndraws,
      allow_new_levels = TRUE,
      sample_new_levels = "gaussian",
      cores = .cores,
      ...
    )
    
    # Get the predictions returned by brms in long form
    draws <- matrix_to_vector(draws)
    
    # Return the vector of draws
    return(draws)
  })
  
  # Expand the data table to length row*category*draw
  out = out[[1]][rep(seq_len(nrow(post_strat)), each = 5*ndraws), ]
  
  # Generate row ids, draw ids
  out[, ':=' (.row = rep(seq_len(nrow(post_strat)), each = 5*ndraws),
              .draw = rep(seq(1, 6e3, 1), length.out = nrow(out)))]
  
  # Generate the linked fate variable by row
  out[, linked_fate := rep(1:5, each = ndraws), by = ".row"]
  
  # Add the draws to the data table
  out[, ':=' (trials = draws[[1]],
              trials_lo = draws[[2]],
              trials_hi = draws[[3]],
              dimension = dimension)]
  
  # Return the updated post-stratification frame
  return(out)
}

matrix_to_vector <- function(draws) {
  
  # Check that the matrix has the correct dimensions
  stopifnot(length(dim(draws)) == 3)
  
  # Turn the prediction matrix into a single long vector structured as
  # row-category-draw; trying really hard to avoid a tidybayes dependency
  draws <- apply(draws, 2, function(x){
    as.vector(x)
  })
  
  draws <- as.vector(draws)
  
  # Return the row-category-draw vector
  return(draws)
}

# Function from the brms package to ensure weighted draws sum to n
round_largest_remainder <- function(x) {
  x <- as.numeric(x)
  total <- round(sum(x))
  out <- floor(x)
  diff <- x - out
  J <- order(diff, decreasing = TRUE)
  I <- seq_len(total - floor(sum(out)))
  out[J[I]] <- out[J[I]] + 1
  return(out)
}
