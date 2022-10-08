#----------------CMPS 2020 Data Pre-Processing: Asian Sub-Sample----------------
#-Author: A. Jordan Nafa--------------------------------Created: July 23, 2022-#
#-R Version: 4.2.1------------------------------Last Modified: October 7, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "sjlabelled",
  "tidyverse",
  "arrow",
  install = FALSE
)

#------------------------------------------------------------------------------#
#--------------------CMPS 2020 Data Cleaning: Asian Sample----------------------
#------------------------------------------------------------------------------#

# Read in the CMPS 2020 data
cmps_2020 <- haven::read_dta("data/cmps/CMPS_2020.dta")

cmps_2020_asian <- cmps_2020 %>%
  ## Filter Latino Respondents
  filter(Main_Eth_quota %in% 4) %>%
  ## Transmute an recode a subset of the data
  transmute(
    # Original Hispanic/Latino Heritage
    heritage = factor(
      S8,
      levels = get_values(S8),
      labels = get_labels(S8)
    ),
    # Recoding the open-ended heritage variables
    heritage_oe = str_to_lower(na_if(S8r16oe, "")),
    # Recoding the heritage variables
    ancestry = case_when(
      heritage == "Chinese" | str_detect(heritage_oe, "chinese") ~ "Chinese",
      heritage == "Taiwanese" | str_detect(heritage_oe, "boli") ~ "Taiwanese", 
      heritage == "Indian" | str_detect(heritage_oe, "indian") ~ "Asian Indian", 
      heritage == "Filipino" | str_detect(heritage_oe, "filipino") ~ "Filipino",
      heritage == "Korean" | str_detect(heritage_oe, "korean") ~ "Korean", 
      heritage == "Vietnamese" | str_detect(heritage_oe, "vietnamese") ~ "Vietnamese",
      heritage == "Japanese" | str_detect(heritage_oe, "japan|okinawan") ~ "Japanese", 
      heritage == "Pakistani" ~ "Pakistani",
      heritage == "Hmong" | str_detect(heritage_oe, "hmong") ~ "Hmong",
      heritage == "Thai" ~ "Thai",
      heritage == "Lao" ~ "Laotian",
      heritage == "Iranian" ~ "Iranian",
      heritage == "Bangladeshi" | str_detect(heritage_oe, "bangladesh|bengali") ~ "Bengali",
      heritage == "Cambodian"  ~ "Cambodian",
      str_detect(heritage_oe, "sri lankan") ~ "Sri Lankan",
      heritage == "Pacific Islander (Samoa, Guam, Tonga\u0085)" ~ "Samoa, Guam, Tonga", 
      str_detect(heritage_oe, "afghan") ~ "Afghan",
      str_detect(heritage_oe, "indonesia") ~ "Indonesian",
      str_detect(heritage_oe, "burma|burmese|myanmar") ~ "Burmese",
      str_detect(heritage_oe, "n(ap|ep)al|newar") ~ "Nepali",
      TRUE ~ "Other Asian Ancestry"
    ),
    # Geographic Regions
    region = case_when(
      ancestry %in% c("Chinese", "Taiwanese", "Korean", "Japanese") ~ "Eastern Asia",
      ancestry %in% c("Asian Indian", "Pakistani", "Iranian", 
                      "Bengali", "Sri Lankan", "Afghan", "Nepali") ~ "Southern Asia",
      ancestry %in% c("Burmese", "Indonesian", "Cambodian", "Laotian", 
                      "Thai", "Hmong", "Vietnamese", "Filipino") ~ "South-Eastern Asia",
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
    panethnic_lfate = Q551_Q559r5,
    gender_lfate = Q551_Q559r8,
    inter_lfate = Q551_Q559r9
  )

# Write the pre-processed 2020 CMPS subset to a file
write_parquet(
  cmps_2020_asian,
  "data/cmps/AAPI_CMPS_2020_Data.gz.parquet",
  compression = "gzip",
  compression_level = 6
)
