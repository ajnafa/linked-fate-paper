#---------------CMPS 2020 Data Pre-Processing: Latino Sub-Sample----------------
#-Author: A. Jordan Nafa--------------------------------Created: July 23, 2022-#
#-R Version: 4.2.1------------------------------Last Modified: October 6, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "sjlabelled",
  "tidyverse",
  "arrow",
  install = FALSE
)

#------------------------------------------------------------------------------#
#-------------------CMPS 2020 Data Cleaning: Latino Sample----------------------
#------------------------------------------------------------------------------#

# Read in the CMPS 2020 data
cmps_2020 <- haven::read_dta("data/cmps/CMPS_2020.dta")

## We'll start here by using regular expressions to recode the heritage 
## question so that it matches the categories in the census data.
cmps_2020_latino <- cmps_2020 %>%
  ## Filter Latino Respondents
  filter(Main_Eth_quota == 2) %>%
  ## Transmute an recode a subset of the data
  transmute(
    # Original Latino Heritage
    heritage = factor(
      S10,
      levels = get_values(S10),
      labels = get_labels(S10)
    ),
    # Recoding the open-ended heritage variables
    heritage_oe = str_to_lower(na_if(S10r22oe, "")),
    # Recoding the heritage variables
    ancestry = case_when(
      heritage == "Argentina" ~ "Argentinean",
      heritage == "Bolivia" | str_detect(heritage_oe, "boli") ~ "Bolivian",
      heritage == "Chile" ~ "Chilean",
      heritage == "Colombia" ~ "Colombian",
      heritage == "Costa Rica" ~ "Costa Rican", 
      heritage == "Cuba" | str_detect(heritage_oe, "cuba") ~ "Cuban",
      heritage == "Dominican Republic" | str_detect(heritage_oe, "dominican")  ~ "Dominican",
      heritage == "Ecuador" | str_detect(heritage_oe, "ecuador") ~ "Ecuadorian",
      heritage == "El Salvador" | str_detect(heritage_oe, "salvador") ~ "Salvadoran",
      heritage == "Guatemala" ~ "Guatemalan",
      heritage == "Honduras" | str_detect(heritage_oe, "honduras") ~ "Honduran",
      heritage == "Nicaragua" ~ "Nicaraguan",
      heritage == "Panama" ~ "Panamanian",
      heritage == "Paraguay" ~ "Paraguayan",
      heritage == "Peru" ~ "Peruvian",
      heritage == "Puerto Rico" | str_detect(heritage_oe, "puerto rico") ~ "Puerto Rican",
      heritage == "Uruguay" ~ "Uruguayan",
      heritage == "Venezuela" | str_detect(heritage_oe, "venezuela")  ~ "Venezuelan",
      heritage == "Spain / Spanish" | str_detect(heritage_oe, "spain") ~ "Spanish",
      heritage == "Mexico" | str_detect(heritage_oe, "xico") ~ "Mexican",
      heritage == "Brazil" ~ "Brazilian",
      TRUE ~ "Other Latino Ancestry"
    ),
    # Geographic Regions
    region = case_when(
      ancestry %in% c("Mexican", "Salvadoran", "Honduran", "Nicaraguan", 
                      "Costa Rican", "Guatemalan", "Panamanian") ~ "Central America",
      ancestry %in% c("Colombian", "Brazilian", "Peruvian", "Argentinean", 
                      "Venezuelan", "Chilean", "Ecuadorian", "Uruguayan", 
                      "Paraguayan", "Bolivian") ~ "South America",
      ancestry %in% c("Dominican", "Jamaican", "Cuban", "Puerto Rican") ~ "Caribbean",
      TRUE ~ "Other"
    ),
    # Respondent's Gender Self-Identification
    female = factor(
      S3b,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    # Respondent's year of birth
    birth_year = S5,
    # Respondent's Age in Years
    age = 2021 - S5,
    # Age recoded into ten categories
    age_cat = factor(
      case_when(
        between(age, 18, 24) ~ 1,
        between(age, 25, 29) ~ 2,
        between(age, 30, 34) ~ 3,
        between(age, 35, 39) ~ 4,
        between(age, 40, 44) ~ 5,
        between(age, 45, 49) ~ 6,
        between(age, 50, 54) ~ 7,
        between(age, 55, 59) ~ 8,
        between(age, 60, 64) ~ 9,
        between(age, 65, 69) ~ 10,
        between(age, 70, 74) ~ 11,
        between(age, 75, 100) ~ 12
      ),
      levels = 1:12,
      labels = c(
        "18-24", "25-29", "30-34", "35-39", 
        "40-44", "45-49", "50-54", "55-59", 
        "60-64", "65-69", "70-74", "75+"
      )
    ),
    # Respondent educational attainment
    educ = factor(
      case_when(
        S13 %in% 1:2 ~ 1,
        TRUE ~ S13 - 1
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
    # Respondent Citizenship
    citizen = factor(
      case_when(
        S7 %in% c(1, 3) | str_detect(Q807r8oe, "born|citizen") ~ 0,
        Q807 == 1 ~ 0, # Naturalized Citizen
        TRUE ~ 1
      ),
      levels = 0:1,
      labels = c("U.S. Citizen", "Non-Citizen")
    ),
    # Respondent Partisanship (Including Leaners)
    partisan = factor(
      case_when(
        Q21 == 4 | Q23 %in% c(4, 88) ~ 0, # Other/Don't Know
        Q21 == 1 | Q23 == 1 ~ 1, # Republican
        Q21 == 2 | Q23 == 2 ~ 2, # Democrat
        Q21 == 3 & Q23 == 3 ~ 3 # Pure Independent
      ),
      levels = 0:3,
      labels = c(
        "Other/Don't Know",
        "Republican",
        "Democrat",
        "Independent"
      )
    ),
    # Presidential Vote Choice
    pres_choice = factor(
      case_when(
        Q12 %in% 1:2 & Q14 %in% 3:6 ~ 1,
        Q12 %in% 1:2 & Q14 == 2 ~ 2,
        Q12 %in% 1:2 & Q14 == 1 ~ 3,
        TRUE ~ 0
      ),
      levels = 0:3,
      labels = c(
        "Did Not Vote",
        "Third-Party/Write-In",
        "Joe Biden",
        "Donald Trump"
      )
    ),
    # Presidential Vote Preference
    pres_pref = factor(
      case_when(
        Q14 %in% 3:6 ~ 1,
        Q14 == 2 ~ 2,
        Q14 == 1 ~ 3
      ),
      levels = 1:3,
      labels = c(
        "Third-Party/Write-In",
        "Joe Biden",
        "Donald Trump"
      )
    ),
    # Presidential Vote Choice Democrat
    dem_vote = case_when(
      pres_choice == "Joe Biden" ~ 1,
      TRUE ~ 0
    ),
    # Presidential Vote Choice Republican
    rep_vote = case_when(
      pres_choice == "Donald Trump" ~ 1,
      TRUE ~ 0
    ),
    # Presidential Vote Preference Republican
    trump_pref = case_when(
      pres_pref == "Donald Trump" ~ 1,
      TRUE ~ 0
    ),
    # Linked Fate Variables
    across(
      Q551_Q559r9:Q551_Q559r1,
      ~ factor(
        .x,
        levels = get_values(.x),
        labels = get_labels(.x)
      )
    ),
    # Original Survey Weights for Descriptive Statistics
    svy_weight = weight
    ) %>% 
  # Renaming linked fate variables
  rename(
    panethnic_lfate = Q551_Q559r2,
    gender_lfate = Q551_Q559r8,
    inter_lfate = Q551_Q559r9
  )

# Write the pre-processed 2020 CMPS subset to a file
write_parquet(
  cmps_2020_latino,
  "data/cmps/Latino_CMPS_2020_Data.gz.parquet",
  compression = "gzip",
  compression_level = 6
)
