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
  ~ source(.x)
  )
