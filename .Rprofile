# Set Session Options
options(
  digits = 6, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"],
  knitr.kable.NA = '',
  brms.backend = "cmdstanr",
  modelsummary_get = "broom"
)

# Load the helper functions
.helpers <- lapply(
  list.files(
    path = "functions/", 
    pattern = ".*R", 
    full.names = TRUE
    ), 
  source
  )

# Base directory to write the partisanship models to
partisan_dir <- "output/fits/models/partisan-mrp/"

# Base directory to write the linked fate models to
lfate_dir <- "output/fits/models/lfate-mrp/"

# Base directory to write the outcome models to
outcome_dir <- "output/fits/models/trump-support-mrp/"

# Base directory for the predictions/estimates
preds_dir <- "output/predictions/"

# Base Directory for the stan files
stan_dir <- "output/fits/stan-code/"

# Base Directory for the model diagnostics
diags_dir <- "output/diagnostics/"

# Base Directory for tables
tables_dir <- "output/tables/"

# Base Directory for standalone figures
figs_dir <- "output/figures/"
