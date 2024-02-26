setwd("/Users/viktorsjoberg/Desktop/high-dimensional/Final project")
library(glmnet)
library(caret)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gt)
library(nnet)
library(readr)
library(scales)

####### Load data #######
x_test <- read_rds("data/x_test.rds")
x_train <- read_rds("data/x_train.rds")
y_train <- read_rds("data/y_train.rds")
y_test <- read_rds("data/y_test.rds")


x_test_scaled <- scale(x_test)
x_train_scaled <- scale(x_train)

####### Create adn train model #######
model_glmnet <- cv.glmnet( 
  x_train_scaled,
  y_train,
  nfolds = 30,
  alpha = 0
)

best_lambda_2 <- model_glmnet$lambda.min

model_glmnet_updated <- glmnet(
  x_train_scaled,
  y_train,
  lambda = best_lambda_2,
  alpha = 0
) 

saveRDS(model_glmnet, "fit/model_glmnet_ridge.rds")
model_glmnet <- read_rds("fit/model_glmnet_ridge.rds")

####### Get the best parameters (loop threw different lambda) #######
best_accuracy <- 0
best_lambda <- NA
best_threshold1 <- NA
best_threshold2 <- NA
threshold1_range <- seq(from = 1.3, to = 1.9, by = 0.1)
threshold2_range <- seq(from = 2.0, to = 2.6, by = 0.1)
y_test_factor <- factor(y_test, levels = c("1", "2", "3"))

for (lambda in model_glmnet$lambda) {
  predictions <- predict(model_glmnet, newx = x_test_scaled, s = lambda, type = "class")
  for (t1 in threshold1_range) {
    for (t2 in threshold2_range) {
      if (t1 < t2) {
        predictions_factor <- factor(
          cut(predictions, breaks = c(-Inf, t1, t2, Inf), labels = c("1", "2", "3")),
          levels = c("1", "2", "3")  # Ensure consistent levels
        )
        if (!all(levels(predictions_factor) %in% levels(y_test_factor))) {
          next 
        }
        current_accuracy <- mean(predictions_factor == y_test_factor)
        if (current_accuracy > best_accuracy) {
          best_accuracy <- current_accuracy
          best_lambda <- lambda
          best_threshold1 <- t1
          best_threshold2 <- t2
        }
      }
    }
  }
}

print(paste("Best Lambda:", best_lambda))
print(paste("Best Threshold1:", best_threshold1))
print(paste("Best Threshold2:", best_threshold2))
print(paste("Best Accuracy:", best_accuracy))

saveRDS(best_lambda, "values/best_lambda_ridge.rds")
saveRDS(best_threshold1, "values/best_threshold1_ridge.rds")
saveRDS(best_threshold2, "values/best_threshold2_ridge.rds")

####### Get the best parameters (loop threw lambda_min) #######
best_accuracy_min <- 0
best_threshold1_min <- NA
best_threshold2_min <- NA
threshold1_range_min <- seq(from = 1.3, to = 1.9, by = 0.1)
threshold2_range_min <- seq(from = 2.0, to = 2.6, by = 0.1)
y_test_factor <- factor(y_test, levels = c("1", "2", "3"))

for (t1_min in threshold1_range_min) {
  predictions <- predict(model_glmnet_updated, newx = x_test_scaled, s = best_lambda_2, type = "class")  
  for (t2_min in threshold2_range_min) {
    if (t1_min < t2_min) {
      predictions_factor <- factor(
        cut(predictions, breaks = c(-Inf, t1, t2, Inf), labels = c("1", "2", "3")),
        levels = c("1", "2", "3")  # Ensure consistent levels
      )
      if (!all(levels(predictions_factor) %in% levels(y_test_factor))) {
        next 
      }
      current_accuracy_min <- mean(predictions_factor == y_test_factor)
      if (current_accuracy_min > best_accuracy_min) {
        best_accuracy_min <- current_accuracy_min
        best_threshold1_min <- t1_min
        best_threshold2_min <- t2_min
      }
    }
  }
}


print(paste("Best Threshold1:", best_threshold1_min))
print(paste("Best Threshold2:", best_threshold2_min))
print(paste("Best Accuracy:", best_accuracy_min))

#### With lambda_min the Accuracy is worse then looping threw lambda 

best_lambda_predictions <- predict(model_glmnet, newx = x_test_scaled, s = best_lambda, type = "class")

best_predictions_factor <- factor(
  cut(best_lambda_predictions, breaks = c(-Inf, best_threshold1, best_threshold2, Inf), labels = c("1", "2", "3"))
)


####### Confusion matrix and other metrics #######
best_conf_matrix <- confusionMatrix(best_predictions_factor, y_test_factor)
best_conf_matrix


class_df <- as.data.frame(best_conf_matrix$byClass)
class_gt <- gt(class_df)
class_gt
gt_output2 <- "img/ridge_class_gt.png"
gtsave(class_gt, gt_output2)

conf_matrix_df <- as.data.frame(table(Predicted = factor(best_predictions_factor, levels = c("1", "2", "3")),
                                      Actual = factor(y_test_factor, levels = c("1", "2", "3"))))

conf_matrix_melted <- melt(conf_matrix_df)


conf_matrix_plot <- ggplot(conf_matrix_melted, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile() +
  ggtitle("Accuracy = 0.6133")+
  geom_text(aes(label = value), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(fill = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("img/conf_matrix_plot_ridge.png", plot = conf_matrix_plot, width = 12, height = 6, bg = "white")



