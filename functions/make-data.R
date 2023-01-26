#' Function for Pre-Processing the Data for the Partisanship Models
#' 
#' @import dplyr
#' @importFrom fastDummies dummy_cols
#'
#' @param cmps_data A data frame containing the pre-processed 2020 CMPS 
#' data subset to be used
#' 
#' @param .vars A character vector containing the names of the 
#' columns in the data frame to be used in the model
#' 
#' @param post_strat A data frame containing the post-stratification
#' table with the group-level predictors to merge into the aggregated
#' survey data
#' 
#' @param ... Reserved for future development, currently unused
#'
#' @return A pre-processed tibble containing the data to be passed to 
#' `brms::brm`
#' 
#' @export make_partisan_data
#'
make_partisan_data <- function(cmps_data, .vars, post_strat, ...) {
  
  # Extract a subset of the covariates for the model
  df.subset <- cmps_data[colnames(cmps_data) %in% .vars]
  
  # Exclude missing data in the CMPS
  df.complete <- df.subset[complete.cases(df.subset), ]
  
  # Convert partisan to dummies
  df.complete <- fastDummies::dummy_cols(
    df.complete, 
    select_columns = "partisan"
    ) %>%
    # Rename the dummied columns for simplicity later
    rename(
      indep = partisan_Independent, 
      repub = partisan_Republican,
      demo = partisan_Democrat,
      otdk = `partisan_Other/Don't Know`
    )
  
  # Aggregate the data to multinomial counts
  df.aggregate <- df.complete %>%
    # Group the data by covariates
    group_by(female, age_cat, educ, citizen, ancestry) %>%
    # Aggregate the cell counts
    summarise(across(otdk:indep, ~ sum(.x, na.rm = TRUE))) %>%
    # Ungroup the data
    ungroup() %>%
    # Model-specific variables
    mutate(
      # Create a size variable for the trials
      trials = indep + otdk + repub + demo,
      # Create a matrix of each column for the multinomial trials
      partisan = cbind(indep, otdk, repub, demo)
    ) %>%
    # Merge in group-level predictors
    left_join(
      post_strat %>%
        select(ancestry, region, pct_college_z) %>%
        distinct(),
      by = "ancestry"
    )
  
  # Apply sum contrast coding to gender and citizen
  df.aggregate <- within(df.aggregate, {
    contrasts(female) <- contr.sum(levels(female))/2
    contrasts(citizen) <- contr.sum(levels(citizen))/2
  })
  
  # Return the processed data
  return(df.aggregate)
}

#' Function for Pre-Processing the Data for the Linked Fate Models
#' 
#' @import dplyr
#' @importFrom fastDummies dummy_cols
#'
#' @param cmps_data A data frame containing the pre-processed 2020 CMPS 
#' data subset to be used
#' 
#' @param .vars A character vector containing the names of the 
#' columns in the data frame to be used in the model
#' 
#' @param post_strat A data frame containing the post-stratification
#' table with the group-level predictors to merge into the aggregated
#' survey data
#' 
#' @param resp A character vector containing the name of the response 
#' variable for the linked fate models.
#' 
#' @param ... Reserved for future development, currently unused
#'
#' @return A pre-processed tibble containing the data to be passed to 
#' `brms::brm`
#' 
#' @export make_lfate_data
#' 

# A Function for building the linked fate data
make_lfate_data <- function(cmps_data, .vars, post_strat, resp, ...) {
  
  # Store the name of the response variable
  resp_var <- enquo(resp)
  
  # Extract a subset of the covariates for the model
  df.subset <- cmps_data[colnames(cmps_data) %in% .vars]
  
  # Exclude missing data in the CMPS
  df.complete <- df.subset[complete.cases(df.subset), ]
  
  # Transform the linked fate variable in resp to integer
  df.aggregate <- df.complete %>% 
    mutate(!! resp_var := as.integer(!! resp_var)) %>% 
    # Convert the linked fate column to seperate dummies
    fastDummies::dummy_cols(select_columns = .vars[length(.vars)]) %>% 
    # Group the data by covariates
    group_by(female, age_cat, educ, citizen, ancestry, partisan) %>% 
    # Aggregate the cell counts
    summarise(across(matches("_lfate_[1-5]"), ~ sum(.x, na.rm = TRUE))) %>% 
    # Ungroup the data
    ungroup() %>% 
    # Model-specific variables
    transmute(
      # Create a size variable for the trials
      trials = rowSums(select(., matches("_lfate_[1-5]"))),
      # Outcome needs to be a matrix with a column for each response
      !! resp_var := cbind(select(., matches("_lfate_[1-5]"))),
      across(female:partisan, ~ .x)
    ) %>% 
    # Merge in group-level predictors
    left_join(
      post_strat %>% 
        select(ancestry, region, pct_college_z) %>% 
        distinct(),
      by = "ancestry"
    )

  # Apply sum contrast coding to gender and citizen
  df.aggregate <- within(df.aggregate, {
    contrasts(female) <- contr.sum(levels(female))/2
    contrasts(citizen) <- contr.sum(levels(citizen))/2
  })
  
  # Return the processed data
  return(df.aggregate)
}


