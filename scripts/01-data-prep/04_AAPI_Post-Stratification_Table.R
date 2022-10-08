#-------------Census Data Pre-Processing for Asian American Sample--------------
#-Author: A. Jordan Nafa--------------------------------Created: July 26, 2022-#
#-R Version: 4.2.1--------------------------------Last Modified: July 26, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "tidyverse",
  "sjlabelled",
  "arrow",
  "ipumsr",
  "datawizard",
  install = FALSE
)

#------------------------------------------------------------------------------#
#---------Processing the Census Data for the Post-Stratification Table----------
#------------------------------------------------------------------------------#

# Load the IPUMS data extract for the 2019 ACS 5-Year Data
acs_data_aapi <- read_ipums_ddi("data/census/asian/usa_00006.xml") %>% 
  ## Read in the data
  read_ipums_micro() %>% 
  ## Drop unuused value labels
  drop_labels()

# Subset the census data
data_mrp_aapi <- acs_data_aapi %>%
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
    birth_place = factor(
      BPL,
      levels = get_values(BPL),
      labels = get_labels(BPL)
    ),
    # Recoding the ancestry variable using race_detailed for "Not Reported"
    ancestry = case_when(
      ancestry == "Chinese" | str_detect(race_detailed, "Chinese") ~ "Chinese",
      ancestry == "Taiwanese" | str_detect(race_detailed, "Taiwanese") ~ "Taiwanese", 
      ancestry == "Indian" | str_detect(race_detailed, "Indian") ~ "Asian Indian", 
      ancestry == "Filipino" | str_detect(race_detailed, "Filipino") ~ "Filipino",
      ancestry == "Korean" | str_detect(race_detailed, "Korean") ~ "Korean", 
      ancestry == "Vietnamese" | str_detect(race_detailed, "Vietnamese") ~ "Vietnamese",
      ancestry == "Japanese" | str_detect(race_detailed, "Japanese") ~ "Japanese", 
      ancestry == "Pakistani" | str_detect(race_detailed, "Pakistani") ~ "Pakistani",
      ancestry == "Hmong" | str_detect(race_detailed, "Hmong") ~ "Hmong",
      ancestry == "Thai" | str_detect(race_detailed, "Thai") ~ "Thai",
      ancestry == "Lao" | str_detect(race_detailed, "Laotian") ~ "Laotian",
      ancestry == "Iranian" | str_detect(birth_place, "Iran") ~ "Iranian",
      ancestry == "Bengali" | str_detect(race_detailed, "Bangladeshi") ~ "Bengali",
      ancestry == "Cambodian" | str_detect(race_detailed, "Cambodian") ~ "Cambodian",
      ancestry == "Sri Lankan" | str_detect(race_detailed, "Sri Lankan") ~ "Sri Lankan",
      ancestry %in% c("Samoan", "Guamanian", "Tongan") ~ "Samoa, Guam, Tonga", 
      ancestry == "Afghan" | str_detect(birth_place, "Afghanistan") ~ "Afghan",
      ancestry == "Indonesian" | str_detect(race_detailed, "Indonesian") ~ "Indonesian",
      ancestry == "Burmese" | str_detect(race_detailed, "Burmese") ~ "Burmese",
      ancestry == "Nepali" | str_detect(race_detailed, "Nepalese") ~ "Nepali",
      TRUE ~ "Other Asian Ancestry"
    ),
    # Total Family Income
    famincome = FTOTINC
  )

# Expanded Version of the Post-Stratification Table for Asian Subsample
aapi_poststrat_base <- data_mrp_aapi %>% 
  # Group the data, setting .drop = FALSE keeps cells with 0
  group_by(ancestry, female, age_cat, citizen, educ) %>% 
  # Construct cell counts for the post-stratification table
  summarise(n = sum(as.numeric(pweight))) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Asian Geographic Sub-Regions
  mutate(region = case_when(
    ancestry %in% c("Chinese", "Taiwanese", "Korean", 
                    "Japanese") ~ "Eastern Asia",
    ancestry %in% c("Asian Indian", "Pakistani", "Iranian", 
                    "Bengali", "Sri Lankan", "Afghan", "Nepali") ~ "Southern Asia",
    ancestry %in% c("Burmese", "Indonesian", "Cambodian", 
                    "Laotian", "Thai", "Hmong", "Vietnamese", 
                    "Filipino") ~ "South-Eastern Asia",
    TRUE ~ "Other"
  ))

# Data for group-level predictors
aapi_group_preds <- data_mrp_aapi %>% 
  # Group the data by ancestry
  group_by(ancestry) %>% 
  # Calculate weighted group-level predictors
  summarise(
    # Median Family Income
    med_income = weighted_median(famincome, pweight, na.rm = TRUE),
    # Median Family Income
    med_age = weighted_median(age, pweight, na.rm = TRUE),
    # Median Family Income
    pct_college = weighted_mean(university, pweight, na.rm = TRUE)
  ) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Log Median Income
  mutate(log_med_income = log(med_income))

# Add in the group-level predictors
aapi_poststrat_full <- aapi_poststrat_base %>% 
  left_join(aapi_group_preds, by = "ancestry")

## Write the post-stratification table to the disk
write_parquet(
  aapi_poststrat_full,
  "data/census/asian/aapi_poststrat_table.gz.parquet",
  compression = "gzip",
  compression_level = 6
)

