bank_data <- read.csv("C:/Users/elmar_e6gaevu/OneDrive/Desktop/bank-full.csv", sep = ";", stringsAsFactors = TRUE)

bank_data$y <- ifelse(bank_data$y == "yes", 1, 0)
bank_data$y <- as.factor(bank_data$y)

bank_data_clean <- subset(bank_data, select = -c(day, month, contact, duration))

set.seed(123)

sample_size <- floor(0.8 * nrow(bank_data_clean))
train_indices <- sample(seq_len(nrow(bank_data_clean)), size = sample_size)

train_set <- bank_data_clean[train_indices, ]
test_set <- bank_data_clean[-train_indices, ]

logit_model <- glm(y ~ ., data = train_set, family = "binomial")

summary(logit_model)

train_prob <- predict(logit_model, train_set, type = "response")
train_pred <- ifelse(train_prob > 0.5, 1, 0)
train_cm <- table(Actual = train_set$y, Predicted = train_pred)
train_accuracy <- sum(diag(train_cm)) / sum(train_cm)

test_prob <- predict(logit_model, test_set, type = "response")
test_pred <- ifelse(test_prob > 0.5, 1, 0)
test_cm <- table(Actual = test_set$y, Predicted = test_pred)
test_accuracy <- sum(diag(test_cm)) / sum(test_cm)

cat("Training Accuracy:", train_accuracy, "\n")
cat("Test Accuracy:", test_accuracy, "\n")

library(pROC)

roc_curve <- roc(test_set$y, test_prob)

plot(roc_curve, main = "ROC Curve for Bank Churn Model", 
     col = "blue", lwd = 2, print.auc = TRUE)

auc_value <- auc(roc_curve)
cat("Area Under Curve (AUC):", auc_value, "\n")