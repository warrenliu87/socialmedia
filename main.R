df=read.csv("instagram_users_lifestyle.csv")
threshold <- quantile(df$user_engagement_score, 0.75, na.rm = TRUE)

df <- df %>%
  mutate(high_engagement_user = ifelse(user_engagement_score >= threshold, 1, 0))
set.seed(123)

train_idx <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train <- df[train_idx, ]
test  <- df[-train_idx, ]
train <- na.omit(train)
test  <- na.omit(test)
model_logit <- glm(
  high_engagement_user ~ age + gender_bin + income_num + education_num +
    sleep_hours_per_night,
  data = train,
  family = binomial
)
pred_probs <- predict(model_logit, newdata = test, type = "response")
pred_class <- ifelse(pred_probs > 0.5, 1, 0)
table(Predicted = pred_class, Actual = test$high_engagement_user)
mean(pred_class == test$high_engagement_user)
library(pROC)

roc_obj <- roc(test$high_engagement_user, pred_probs)
auc(roc_obj)
plot(roc_obj)
