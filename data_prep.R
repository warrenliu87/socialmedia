# =============================================================================
# Data Preparation & Model Training
# Run this script ONCE to produce clean_df.rds and model_ctr.rds
# =============================================================================

library(tidyverse)
library(pROC)

# =============================================================================
# 1. DATA CLEANING
# =============================================================================

df <- read.csv("instagram_users_lifestyle.csv")

df <- df %>%
  mutate(
    gender_bin = case_when(
      str_to_lower(gender) == "male" ~ 1,
      str_to_lower(gender) != "male" ~ 0,
      TRUE ~ NA_real_
    ),
    education_clean = education_level %>%
      str_replace_all("'", "") %>%
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

saveRDS(df, "clean_df.rds")
cat("Saved clean_df.rds\n")

# =============================================================================
# 2. MODEL TRAINING
# =============================================================================

# Feature engineering: Ad CTR
df$ad_ctr <- df$ads_clicked_per_day / df$ads_viewed_per_day
df <- df[is.finite(df$ad_ctr) & !is.na(df$ad_ctr), ]

# Binarize at median
ctr_median <- median(df$ad_ctr)
df$high_ctr <- ifelse(df$ad_ctr > ctr_median, 1, 0)

cat("CTR median threshold:", round(ctr_median, 4), "\n")
cat("Class distribution:\n")
print(table(df$high_ctr))

# Prepare model data
df_model <- df %>%
  dplyr::select(-user_id, -last_login_date,
                -uses_premium_features, -subscription_status,
                -ad_ctr,
                -ads_clicked_per_day)

df_model <- na.omit(df_model)

# Convert character columns to factors
char_cols <- names(df_model)[sapply(df_model, is.character)]
df_model[char_cols] <- lapply(df_model[char_cols], as.factor)

# Train-test split (70/30)
set.seed(123)
train_idx <- sample(seq_len(nrow(df_model)), size = 0.7 * nrow(df_model))
train <- df_model[train_idx, ]
test  <- df_model[-train_idx, ]

cat("\nTrain:", nrow(train), "| Test:", nrow(test), "\n")
cat("Train class distribution:\n")
print(table(train$high_ctr))

# Oversample minority class if imbalanced
class_counts <- table(train$high_ctr)
if (min(class_counts) / max(class_counts) < 0.8) {
  majority_label <- names(which.max(class_counts))
  minority_label <- names(which.min(class_counts))

  train_majority <- train[train$high_ctr == as.numeric(majority_label), ]
  train_minority <- train[train$high_ctr == as.numeric(minority_label), ]

  set.seed(42)
  train_minority_up <- train_minority[sample(
    seq_len(nrow(train_minority)),
    size = nrow(train_majority),
    replace = TRUE
  ), ]
  train_balanced <- rbind(train_majority, train_minority_up)
  train_balanced <- train_balanced[sample(seq_len(nrow(train_balanced))), ]
  cat("Oversampled to balance classes:\n")
  print(table(train_balanced$high_ctr))
} else {
  train_balanced <- train
  cat("Classes roughly balanced, no oversampling needed.\n")
}

# Evaluation helper
evaluate_model <- function(probs, actual, model_name) {
  roc_obj <- roc(actual, probs, quiet = TRUE)
  auc_val <- auc(roc_obj)

  best <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
  opt_threshold <- best$threshold[1]

  pred_optimal <- ifelse(probs > opt_threshold, 1, 0)
  cm <- table(Predicted = pred_optimal, Actual = actual)

  tp <- if ("1" %in% rownames(cm) && "1" %in% colnames(cm)) cm["1", "1"] else 0
  fp <- if ("1" %in% rownames(cm) && "0" %in% colnames(cm)) cm["1", "0"] else 0
  fn <- if ("0" %in% rownames(cm) && "1" %in% colnames(cm)) cm["0", "1"] else 0
  tn <- if ("0" %in% rownames(cm) && "0" %in% colnames(cm)) cm["0", "0"] else 0

  accuracy  <- (tp + tn) / (tp + fp + fn + tn)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1        <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0

  cat(sprintf("\n===== %s (Threshold: %.4f) =====\n", model_name, opt_threshold))
  print(cm)
  cat("Accuracy: ", round(accuracy, 4), "\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall:   ", round(recall, 4), "\n")
  cat("F1 Score: ", round(f1, 4), "\n")
  cat("AUC:      ", round(auc_val, 4), "\n")

  return(list(auc = as.numeric(auc_val), accuracy = accuracy,
              precision = precision, recall = recall, f1 = f1,
              threshold = opt_threshold, roc = roc_obj))
}

# Train logistic regression
model_glm <- glm(high_ctr ~ ., data = train_balanced, family = binomial)
cat("\n===== Model Summary =====\n")
print(summary(model_glm))

# Evaluate on test set
probs_glm <- predict(model_glm, newdata = test, type = "response")
eval_glm <- evaluate_model(probs_glm, test$high_ctr, "Logistic Regression")

# Save trained model
saveRDS(model_glm, "model_ctr.rds")
cat("\nSaved model_ctr.rds\n")
