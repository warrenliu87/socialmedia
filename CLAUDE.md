# Social Media Analytics Project

## Overview
Esade university course project (Data Analytics with R, Term 2) analyzing Instagram user lifestyle data. The goal is to explore user behavior, build predictive models, and create visualizations from a large dataset (~619k rows, 52 columns).

## Data
- **Raw data**: `instagram_users_lifestyle.csv` (~170 MB, ~619k rows)
- **Cleaned data**: `clean_df.rds` (~37 MB, ~390k rows after cleaning) — this is the standard input for all branches

### Key variables in `clean_df.rds`
- **Target variable (current model)**: `uses_premium_features` (binary: 1=yes, 0=no, ~78% class 0)
- **Demographics**: `age`, `gender_bin`, `country`, `urban_rural`, `income_num`, `education_num`, `employment_status`, `relationship_status`, `has_children`
- **Health/lifestyle**: `exercise_hours_per_week`, `sleep_hours_per_night`, `diet_quality`, `smoking`, `alcohol_frequency`, `perceived_stress_score`, `self_reported_happiness`, `body_mass_index`, `blood_pressure_systolic/diastolic`, `daily_steps_count`, `weekly_work_hours`
- **Instagram activity**: `daily_active_minutes_instagram`, `sessions_per_day`, `posts_created_per_week`, `reels_watched_per_day`, `stories_viewed_per_day`, `likes_given_per_day`, `comments_written_per_day`, `dms_sent/received_per_week`, `ads_viewed/clicked_per_day`, `time_on_feed/explore/messages/reels_per_day`
- **Account info**: `followers_count`, `following_count`, `notification_response_rate`, `account_creation_year`, `content_type_preference`, `preferred_content_theme`, `privacy_setting_level`, `two_factor_auth_enabled`, `biometric_login_used`, `linked_accounts_count`, `subscription_status`, `user_engagement_score`
- **Encoded columns**: `gender_bin` (1=male), `education_num` (0-6), `income_num` (0-4), `diet_quality` (0-4), `smoking` (0/1), `alcohol_frequency` (0-4)
- **Non-predictive columns** (excluded from models): `user_id`, `last_login_date`

## Git Branch Structure
- **main**: Base branch
- **DataCleaning**: Data cleaning and encoding pipeline (creates `clean_df.rds`)
- **PredictionModel**: Logistic regression models (current work)
- **Visualizations**: Visualization scripts
- **UI**: UI-related scripts

All branches load `clean_df.rds` as their starting point.

## Current Model (`main.R` on PredictionModel branch)
- Logistic regression predicting `uses_premium_features`
- Uses a 30k row sample (full dataset is too slow for stepAIC; balanced training set ~33k rows)
- **Class imbalance handled**: minority class (1) is oversampled (with replacement) to match the majority class in training data
- **Optimal threshold**: ROC-based threshold selection (maximizes sensitivity + specificity) instead of fixed 0.5
- Two models: full glm (all predictors, 83 coefficients) and stepwise (AIC-selected, 42 coefficients)
- Evaluation metrics: accuracy, precision, recall, F1, AUC — all computed at both default (0.5) and optimal thresholds
- **Results**: AUC ~0.50 for both models — features have essentially no predictive power for `uses_premium_features`. The oversampling fixed the "all zeros" prediction problem, but the underlying signal is too weak.

## Known Issues
- **Weak signal for `uses_premium_features`**: AUC ~0.50 despite oversampling and threshold tuning. The available features do not meaningfully predict premium feature usage. Consider a different target variable or feature engineering if revisiting.
- **MASS/dplyr conflict**: `MASS::select` masks `dplyr::select`. Always use `dplyr::select()` explicitly when both packages are loaded.

## R Packages Used
- `tidyverse` (core data manipulation and plotting)
- `MASS` (stepwise selection via `stepAIC`)
- `pROC` (ROC analysis, optimal threshold selection, AUC computation)
- `dplyr`, `stringr` (used in DataCleaning branch)

## Commands
```bash
# Run the model script
Rscript main.R
```
