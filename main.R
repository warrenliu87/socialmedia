library(tidyverse)
library(pROC)

# =============================================================================
# Predictive Model: Ad Click-Through Rate (CTR)
# =============================================================================
# Target: high_ctr (binary) — whether a user's ad click-through rate
# (ads_clicked_per_day / ads_viewed_per_day) is above the median.
# =============================================================================

df <- read_rds("./clean_df.rds")

# --- Feature Engineering: Ad CTR ---
df$ad_ctr <- df$ads_clicked_per_day / df$ads_viewed_per_day

# Remove Inf (division by zero) and NAs
df <- df[is.finite(df$ad_ctr) & !is.na(df$ad_ctr), ]

# Binarize at median
ctr_median <- median(df$ad_ctr)
df$high_ctr <- ifelse(df$ad_ctr > ctr_median, 1, 0)

cat("CTR median threshold:", round(ctr_median, 4), "\n")
cat("Class distribution:\n")
print(table(df$high_ctr))

# --- Data Preparation ---
df_model <- df %>%
  dplyr::select(-user_id, -last_login_date,
                -uses_premium_features, -subscription_status,
                -ad_ctr,                        # remove continuous version of target
                -ads_clicked_per_day)            # remove direct numerator of ratio

df_model <- na.omit(df_model)

# Convert character columns to factors
char_cols <- names(df_model)[sapply(df_model, is.character)]
df_model[char_cols] <- lapply(df_model[char_cols], as.factor)

# Train-test split (70/30) on full data
set.seed(123)
train_idx <- sample(seq_len(nrow(df_model)), size = 0.7 * nrow(df_model))
train <- df_model[train_idx, ]
test  <- df_model[-train_idx, ]

cat("\n===== Train/Test Split =====\n")
cat("Train:", nrow(train), "| Test:", nrow(test), "\n")
cat("Train class distribution:\n")
print(table(train$high_ctr))
cat("Proportions:", round(prop.table(table(train$high_ctr)), 3), "\n")

# --- Oversample minority class if imbalanced ---
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
  cat("\nOversampled to balance classes:\n")
  print(table(train_balanced$high_ctr))
} else {
  train_balanced <- train
  cat("\nClasses roughly balanced, no oversampling needed.\n")
}

# =============================================================================
# Evaluation helper
# =============================================================================
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

# =============================================================================
# Logistic Regression
# =============================================================================
cat("\n\n========== LOGISTIC REGRESSION ==========\n")
model_glm <- glm(high_ctr ~ ., data = train_balanced, family = binomial)
cat("\n===== Model Summary =====\n")
print(summary(model_glm))

probs_glm <- predict(model_glm, newdata = test, type = "response")
eval_glm <- evaluate_model(probs_glm, test$high_ctr, "Logistic Regression")
