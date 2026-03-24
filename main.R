library(tidyverse)
df=read.csv("instagram_users_lifestyle.csv")
library(dplyr)
library(stringr)

df <- df %>%
  mutate(
    gender_bin = case_when(
      str_to_lower(gender) == "male" ~ 1,
      str_to_lower(gender) != "male" ~ 0,
      TRUE ~ NA_real_
    ),
    education_clean = education_level %>%
      str_replace_all("’", "") %>%
      str_trim() %>%
      str_to_lower(),
    education_num = case_when(
      education_clean == "other" ~ 0,
      education_clean == "secondary" ~ 1,
      education_clean == "high school" ~ 2,
      education_clean == "some college" ~ 3,
      education_clean == "bachelor" ~ 4,
      education_clean == "bachelors" ~ 4,
      education_clean == "masters" ~ 5,
      education_clean == "phd" ~ 6,
      TRUE ~ NA_real_
    ),
    gender = NULL,
    education_level = NULL,
    education_clean = NULL
  )
df <- df %>%
  mutate(
    income_clean = income_level %>%
      str_to_lower() %>%
      str_trim(),
    
    income_num = case_when(
      income_clean == "low" ~ 0,
      income_clean == "lower-middle" ~ 1,
      income_clean == "middle" ~ 2,
      income_clean == "upper-middle" ~ 3,
      income_clean == "high" ~ 4,
      TRUE ~ NA_real_
    ),
    
    # delete old columns
    income_level = NULL,
    income_clean = NULL
  )
df <- df %>%
  mutate(
    two_factor_auth_enabled = case_when(
      str_to_lower(two_factor_auth_enabled) == "yes" ~ 1,
      str_to_lower(two_factor_auth_enabled) == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    biometric_login_used = case_when(
      str_to_lower(biometric_login_used) == "yes" ~ 1,
      str_to_lower(biometric_login_used) == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    uses_premium_features = case_when(
      str_to_lower(uses_premium_features) == "yes" ~ 1,
      str_to_lower(uses_premium_features) == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    has_children = case_when(
      str_to_lower(has_children) == "yes" ~ 1,
      str_to_lower(has_children) == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    subscription_status = case_when(
      str_to_lower(subscription_status) == "premium" ~ 1,
      str_to_lower(subscription_status) == "free" ~ 0,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(
    diet_quality = case_when(
      str_to_lower(diet_quality) == "very poor" ~ 0,
      str_to_lower(diet_quality) == "poor" ~ 1,
      str_to_lower(diet_quality) == "average" ~ 2,
      str_to_lower(diet_quality) == "good" ~ 3,
      str_to_lower(diet_quality) == "excellent" ~ 4,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(
    smoking = case_when(
      str_to_lower(smoking) == "yes" ~ 1,
      str_to_lower(smoking) != "yes" ~ 0,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(
    alcohol_frequency = case_when(
      str_to_lower(alcohol_frequency) == "never" ~ 0,
      str_to_lower(alcohol_frequency) == "rarely" ~ 1,
      str_to_lower(alcohol_frequency) == "weekly" ~ 2,
      str_to_lower(alcohol_frequency) == "several times a week" ~ 3,
      str_to_lower(alcohol_frequency) == "daily" ~ 4,
      TRUE ~ NA_real_
    )
  )


# Saving the cleaned df
saveRDS(df, "./clean_df")
