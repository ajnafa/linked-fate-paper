#-------------------Non-Census MrP Models for Partisanship----------------------
#-Author: A. Jordan Nafa--------------------------------Created: July 23, 2022-#
#-R Version: 4.2.1------------------------------Last Revised: January 23, 2023-#

# Load the necessary libraries
pacman::p_load(
  "tidyverse", # Suite of packages for data management 
  "arrow", # Apache Arrow data management and file storage
  "brms", # Bayesian regression models with Stan
  "tidybayes", # Functions for wrangling posteriors tidy-style
  "future",
  "furrr",
  "fastDummies", # Create dummy variables from a factor
  install = FALSE
)

#------------------------------------------------------------------------------#
#-------------------------------Data Preparation--------------------------------
#------------------------------------------------------------------------------#

# Read in the Post-Stratification Data
post_strat <- map(
  .x = c(
    "data/census/latino/latino_poststrat_table.gz.parquet",
    "data/census/asian/aapi_poststrat_table.gz.parquet"
  ),
  ~ read_parquet(.x) %>% 
    # Set % college at the median and multiple by 10
    mutate(pct_college_z = (pct_college - median(pct_college))*10)
)

# Read in the processed CMPS subset data
cmps_data <- map(
  .x = c(
    "data/cmps/Latino_CMPS_2020_Data.gz.parquet",
    "data/cmps/AAPI_CMPS_2020_Data.gz.parquet"
  ),
  ~ read_parquet(.x)
)

# Build the data for the partisanship models
partisan_data <- map2(
  .x = cmps_data,
  .y = post_strat,
  ~ make_partisan_data(
    .x, 
    .vars = c("female", "ancestry", "age_cat", "educ", "citizen", "partisan"),
    .y
    )
)

# Set names for the data frames
names(partisan_data) <- c("Latino", "Asian")

#------------------------------------------------------------------------------#
#-------------------------------Model Preparation-------------------------------
#------------------------------------------------------------------------------#

# Base Model and Interactions for Partisanship
partisan_rhs <- c(
  "", # Partisanship Baseline Model
  " + (1 | age_cat:educ)", # Age x Education
  " + (1 | ancestry:educ)", # Ancestry x Education
  " + (1 | female:educ)",  # Gender x Education
  " + (1 | female:age_cat)", # Gender x Age
  " + (1 | female:ancestry)", # Gender x Ancestry
  " + (1 | ancestry:age_cat)", # Ancestry x Age
  " + (1 | age_cat:educ) + (1 | female:age_cat)"
)

# Return each combination of lhs and rhs as a list
partisan_forms <- formula_builder(
  lhs = "partisan | trials(trials) ~ female + citizen + (1 | age_cat) + 
  (1 | educ) + (1 | ancestry) + pct_college_z + region",
  rhs = partisan_rhs
)

# Specify some weakly informative priors for the model parameters
partisan_mlogit_priors <- 
  prior(normal(0, 1), class = "b", dpar = "mudemo") +
  prior(student_t(10, 0, 1), class = "Intercept", dpar = "mudemo") +
  prior(exponential(0.5), class = "sd", dpar = "mudemo") +
  prior(normal(0, 1), class = "b", dpar = "muotdk") +
  prior(student_t(10, 0, 1), class = "Intercept", dpar = "muotdk") +
  prior(exponential(0.5), class = "sd", dpar = "muotdk") +
  prior(normal(0, 1), class = "b", dpar = "murepub") +
  prior(student_t(10, 0, 1), class = "Intercept", dpar = "murepub") +
  prior(exponential(0.5), class = "sd", dpar = "murepub")

#------------------------------------------------------------------------------#
#--------------------Model Estimation for the Latino Sample---------------------
#------------------------------------------------------------------------------#

# File paths to save the models to
latino_partisan_files <- paste0(
  partisan_dir,
  "latino/partisan_mrp_latino_",
  c(
    "base",
    "age_x_educ",
    "ancestry_x_educ",
    "female_x_educ",
    "female_x_age",
    "female_x_ancestry",
    "ancestry_x_age",
    "age_x_educ_x_female"
  )
)

# Use future to parallelize the models
plan(tweak(multisession, workers = 3))

# Fit the latino partisanship models
latino_partisan_mlogit_ls <- future_map2(
  .x = partisan_forms,
  .y = latino_partisan_files,
  ~ brm(
    formula = .x,
    prior = partisan_mlogit_priors,
    family = multinomial(link = "logit", refcat = "indep"),
    data = partisan_data$Latino,
    cores = 12, 
    chains = 6, 
    iter = 12000,
    warmup = 6000,
    thin = 6,
    refresh = 100,
    seed = 12345,
    control = list(
      adapt_delta = 0.99, 
      max_treedepth  = 15,
      step_size = 0.02
    ),
    save_pars = save_pars(all = TRUE),
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    file = .y
  ),
  .options = furrr_options(
    scheduling = 1,
    seed = TRUE,
    prefix = "prefix"
  ),
  .progress = TRUE
)

#------------------------------------------------------------------------------#
#-----------------Cross Validation for the Latino Sample Models-----------------
#------------------------------------------------------------------------------#

# Set the seed to ensure this is reproducible
set.seed(12345)

# Define the folds for the splits
kfolds <- loo::kfold_split_grouped(
  K = 10, 
  partisan_data$Latino$ancestry
  )

# Use future to parallelize cross validation
plan(multisession(workers = 10))

# K-fold cross validation by ancestry
latino_partisan_mlogit_ls <- map(
  .x = latino_partisan_mlogit_ls,
  ~ add_criterion(
    .x,
    criterion = "kfold",
    K = 10,
    folds = kfolds,
    save_fits = TRUE,
    chains = 4, 
    iter = 12000,
    warmup = 6000,
    thin = 4,
    seed = 12345,
    control = list(
      adapt_delta = 0.99, 
      max_treedepth  = 15,
      step_size = 0.02
    ),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian"
  )
)

# Close Future Sessions
plan(sequential)

# Add PSIS LOO-CV
latino_partisan_mlogit_ls <- map(
  .x = latino_partisan_mlogit_ls,
  ~ add_criterion(
    .x,
    criterion = "loo"
  ))

#------------------------------------------------------------------------------#
#---------------------Model Estimation for the Asian Sample---------------------
#------------------------------------------------------------------------------#

# File paths to save the models to
asian_partisan_files <- paste0(
  partisan_dir,
  "asian/partisan_mrp_asian_",
  c(
    "base",
    "age_x_educ",
    "ancestry_x_educ",
    "female_x_educ",
    "female_x_age",
    "female_x_ancestry",
    "ancestry_x_age"
  )
)

# Use future to parallelize the models
plan(tweak(multisession, workers = 3))

# Fit the Asian partisanship models
asian_partisan_mlogit_ls <- future_map2(
  .x = partisan_forms,
  .y = asian_partisan_files,
  ~ brm(
    formula = .x,
    prior = partisan_mlogit_priors,
    family = multinomial(link = "logit", refcat = "indep"),
    data = partisan_data$Asian,
    cores = 12, 
    chains = 6, 
    iter = 12000,
    warmup = 6000,
    thin = 6,
    refresh = 100,
    seed = 12345,
    control = list(
      adapt_delta = 0.99, 
      max_treedepth  = 15,
      step_size = 0.02
    ),
    save_pars = save_pars(all = TRUE),
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    file = .y
  ),
  .options = furrr_options(
    scheduling = 1,
    seed = TRUE,
    prefix = "prefix"
  ),
  .progress = TRUE
)

#------------------------------------------------------------------------------#
#-----------------Cross Validation for the Asian Sample Models------------------
#------------------------------------------------------------------------------#

# Set the seed to ensure this is reproducible
set.seed(12345)

# Define the folds for the splits, need to use stratified here because of the
# other category in the region indicator
aapi_kfolds <- loo::kfold_split_stratified(
  K = 10, 
  partisan_data$Asian$ancestry
)

# Use future to parallelize cross validation
plan(multisession(workers = 10))

# K-fold cross validation by ancestry
asian_partisan_mlogit_ls <- map(
  .x = asian_partisan_mlogit_ls,
  ~ add_criterion(
    .x,
    criterion = "kfold",
    K = 10,
    folds = aapi_kfolds,
    save_fits = TRUE,
    chains = 4, 
    iter = 12000,
    warmup = 6000,
    thin = 4,
    seed = 12345,
    control = list(
      adapt_delta = 0.99, 
      max_treedepth  = 15,
      step_size = 0.02
    ),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian"
  )
)

# Close Future Sessions
plan(sequential)

# Add PSIS LOO-CV
asian_partisan_mlogit_ls <- map(
  .x = asian_partisan_mlogit_ls,
  ~ add_criterion(
    .x,
    criterion = "loo"
  ))
