#-----------------Census Data Pre-Processing for Latino Sample------------------
#-Author: A. Jordan Nafa--------------------------------Created: July 26, 2022-#
#-R Version: 4.2.1--------------------------------Last Modified: July 26, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "tidyverse",
  "sjlabelled",
  "arrow",
  "ipumsr",
  "datawizard",
  install = TRUE
)

#------------------------------------------------------------------------------#
#---------Processing the Census Data for the Post-Stratification Table----------
#------------------------------------------------------------------------------#

# Load the IPUMS data extract for the 2019 ACS 5-Year Data
acs_data_latino <- read_ipums_ddi("data/census/latino/usa_00005.xml") %>% 
  ## Read in the data
  read_ipums_micro() %>% 
  ## Drop unused value labels
  drop_labels()

# Subset the census data
data_mrp_latino <- acs_data_latino %>% 
  # Our data does not include any individuals under 18
  filter(AGE >= 18) %>%
  # Transmute a subset of the data extract
  transmute(
    # Household ID
    hh_id = SERIAL,
    # Household Weight
    hhweight = HHWT,
    # Person Weight
    pweight = PERWT,
    # Respondent's Sex (Reference = Male)
    female = factor(
      SEX,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    # Original Age Variable
    age = AGE,
    # Age Recoded into Ten Categories
    age_cat = factor(
      case_when(
        between(AGE, 18, 24) ~ 1,
        between(AGE, 25, 29) ~ 2,
        between(AGE, 30, 34) ~ 3,
        between(AGE, 35, 39) ~ 4,
        between(AGE, 40, 44) ~ 5,
        between(AGE, 45, 49) ~ 6,
        between(AGE, 50, 54) ~ 7,
        between(AGE, 55, 59) ~ 8,
        between(AGE, 60, 64) ~ 9,
        between(AGE, 65, 69) ~ 10,
        between(AGE, 70, 74) ~ 11,
        between(AGE, 75, 100) ~ 12
      ),
      levels = 1:12,
      labels = c(
        "18-24", "25-29", "30-34", "35-39", 
        "40-44", "45-49", "50-54", "55-59", 
        "60-64", "65-69", "70-74", "75+"
      )
    ),
    # Hispanic or Latino (Ref = Non-Hispanic)
    latino = factor(
      if_else(HISPAN %in% 1:4, 1, 0),
      levels = 0:1,
      labels = c("Not Hispanic or Latino", "Hispanic or Latino")
    ),
    # Racial Demographic Recoded
    race = factor(
      case_when(
        RACE == 1 ~ 0,
        RACE == 2 ~ 1,
        RACE == 3 ~ 2,
        RACE %in% 4:6 ~ 3,
        RACE == 7 ~ 4,
        RACE %in% 8:9 ~ 5
      ),
      levels = 0:5,
      labels = c(
        "White", 
        "Black/African American", 
        "American Indian or Alaska Native", 
        "Asian or Pacific Islander",
        "Other",
        "Two or More Races"
      )
    ),
    # Detailed Race
    race_detailed = factor(
      RACED,
      levels = get_values(RACED),
      labels = get_labels(RACED)
    ),
    # Citizenship Status
    citizen = factor(
      case_when(
        CITIZEN %in% 0:2 ~ 0,
        CITIZEN == 3 ~ 1
      ),
      levels = 0:1,
      labels = c("U.S. Citizen", "Non-Citizen")
    ),
    # Respondent educational attainment recoded to match CMPS
    educ = factor(
      case_when(
        between(EDUCD, 1, 61) ~ 1, # Some High School or Less
        between(EDUCD, 63, 64) ~ 2, # High School graduate or GED
        between(EDUCD, 65, 71) ~ 3, # Some College
        EDUCD == 81 ~ 4, # Associates, 2-year degree
        EDUCD == 101 ~ 5, # Bachelors, 4-year degree
        between(EDUCD, 114, 116) ~ 6 # Post-graduate degree
      ),
      levels = 1:6,
      labels = c(
        "Some High School or Less",
        "High School Graduate or GED",
        "Some College",
        "Associates, 2-year Degree",
        "Bachelors, 4-year Degree",
        "Post-Graduate Degree"
      )
    ),
    # At least a college graduate
    university = case_when(
      between(EDUCD, 101, 116) ~ 1,
      TRUE ~ 0
    ),
    # Ancestry (First Option)
    ancestry = factor(
      ANCESTR1,
      levels = get_values(ANCESTR1),
      labels = get_labels(ANCESTR1)
    ),
    # Birth Place
    birthplace = factor(
      BPL,
      levels = get_values(BPL),
      labels = get_labels(BPL)
    ),
    # Recoding the ancestry variable; using birthplace for "Not Reported"/Uncodable
    ancestry = case_when(
      ancestry == "Argentinean" | str_detect(birthplace, "Argentina") ~ "Argentinean",
      ancestry == "Bolivian" | str_detect(birthplace, "Bolivia") ~ "Bolivian",
      ancestry == "Chilean" | str_detect(birthplace, "Chile") ~ "Chilean",
      ancestry == "Colombian" | str_detect(birthplace, "Colombia") ~ "Colombian",
      ancestry == "Costa Rican" | str_detect(birthplace, "Costa Rica") ~ "Costa Rican", 
      ancestry == "Cuban" | str_detect(birthplace, "Cuba") ~ "Cuban",
      ancestry == "Dominican" | str_detect(birthplace, "Dominican Republic")  ~ "Dominican",
      ancestry == "Ecuadorian" | str_detect(birthplace, "Ecuador") ~ "Ecuadorian",
      ancestry == "Salvadoran" | str_detect(birthplace, "El Salvador") ~ "Salvadoran",
      ancestry == "Guatemalan" | str_detect(birthplace, "Guatemala") ~ "Guatemalan",
      ancestry == "Honduran" | str_detect(birthplace, "Honduras") ~ "Honduran",
      ancestry == "Nicaraguan" | str_detect(birthplace, "Nicaragua") ~ "Nicaraguan",
      ancestry == "Panamanian" | str_detect(birthplace, "Panama") ~ "Panamanian",
      ancestry == "Paraguayan" | str_detect(birthplace, "Paraguay") ~ "Paraguayan",
      ancestry == "Peruvian" | str_detect(birthplace, "Peru") ~ "Peruvian",
      ancestry == "Puerto Rican" | str_detect(birthplace, "Puerto Rico") ~ "Puerto Rican",
      ancestry == "Uruguayan" | str_detect(birthplace, "Uruguay") ~ "Uruguayan",
      ancestry == "Venezuelan" | str_detect(birthplace, "Venezuela")  ~ "Venezuelan",
      ancestry %in% c("Spanish", "Spanish American") | str_detect(birthplace, "Spain") ~ "Spanish",
      ancestry == "Mexican" | str_detect(birthplace, "Mexico") ~ "Mexican",
      ancestry == "Brazilian" | str_detect(birthplace, "Brazil") ~ "Brazilian",
      TRUE ~ "Other Latino Ancestry"
    ),
    # Total Family Income
    famincome = FTOTINC
  )

## Post-Stratification Table for Latino Subsample
latino_poststrat_base <- data_mrp_latino %>% 
  # Group the data, setting .drop = FALSE keeps cells with 0
  group_by(ancestry, female, age_cat, citizen, educ) %>% 
  # Construct cell counts for the post-stratification table
  summarise(n = sum(as.numeric(pweight))) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Geographic Regions
  mutate(region = case_when(
    ancestry %in% c("Mexican", "Salvadoran", "Honduran", "Nicaraguan", 
                    "Costa Rican", "Guatemalan", "Panamanian") ~ "Central America",
    ancestry %in% c("Colombian", "Brazilian", "Peruvian", "Argentinean", 
                    "Venezuelan", "Chilean", "Ecuadorian", "Uruguayan", 
                    "Paraguayan", "Bolivian") ~ "South America",
    ancestry %in% c("Dominican", "Jamaican", "Cuban", "Puerto Rican") ~ "Caribbean",
    TRUE ~ "Other"
  ))

## Data for group-level predictors
latino_group_preds <- data_mrp_latino %>% 
  # Group the data by ancestry
  group_by(ancestry) %>% 
  # Calculate weighted group-level predictors
  summarise(
    # Median Family Income
    med_income = weighted_median(famincome, pweight, na.rm = TRUE),
    # Median Age
    med_age = weighted_median(age, pweight, na.rm = TRUE),
    # Percentage College Educated
    pct_college = weighted_mean(university, pweight, na.rm = TRUE)
    ) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Log Median Income
  mutate(log_med_income = log(med_income))

# Add in the group-level predictors
latino_poststrat_full <- latino_poststrat_base %>% 
  left_join(latino_group_preds, by = "ancestry")

## Write the post-stratification table to the disk
write_parquet(
  latino_poststrat_full,
  "data/census/latino/latino_poststrat_table.gz.parquet",
  compression = "gzip",
  compression_level = 6
)

