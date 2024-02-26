setwd("/Users/viktorsjoberg/Desktop/high-dimensional/Final project")
library(readr)
library(pROC)
library(glmnet)

y_test <- read_rds("data/y_test.rds")
x_test <- read_rds("data/x_test.rds")
best_lambda <- read_rds("values/best_lambda_ridge.rds")
model_glmnet <- read_rds("fit/model_glmnet_ridg.rds")
x_test_scaled <- scale(x_test)



###### AUC-ROC curve ###### 



predicted_probs <- predict(model_glmnet, newx = x_test_scaled, s = best_lambda, type = "response")
predicted_probs_matrix <- predicted_probs[, , 1]
str(predicted_probs_matrix)


# Compute the multi-class ROC
# Initialize lists to store ROC curves and AUC values
roc_curves <- list()
auc_values <- numeric(ncol(predicted_probs_matrix))

# Compute the ROC curves and AUC values
for(i in 1:ncol(predicted_probs_matrix)) {
  prob_for_class_i <- predicted_probs_matrix[, i]
  roc_curves[[i]] <- roc(response = factor(y_test, levels = c("1", "2", "3")), predictor = prob_for_class_i)
  auc_values[i] <- auc(roc_curves[[i]])
}

# Plot and save each ROC curve separately

# Save ROC curve for Class 1
png("img/class_1_ROC_curve_ridge.png")
plot(roc_curves[[1]], main = paste("Class 1 - AUC:", round(auc_values[1], 4)))
dev.off()

# Save ROC curve for Class 2
png("img/class_2_ROC_curve_ridge.png")
plot(roc_curves[[2]], main = paste("Class 2 - AUC:", round(auc_values[2], 4)))
dev.off()

# Save ROC curve for Class 3
png("img/class_3_ROC_curve_ridge.png")
plot(roc_curves[[3]], main = paste("Class 3 - AUC:", round(auc_values[3], 4)))
dev.off()



###### Regularization paths ####### 
plot(model_glmnet, xvar = "lambda", label = TRUE)

png("img/Regularization_paths_ridge.png")
plot(model_glmnet, xvar = "lambda", label = TRUE)
dev.off()

###### Feature selection ###### 
coefficients <- coef(model_glmnet, s = best_lambda)

# The coefficients for a multinomial model are stored in a list of matrices
# Convert each matrix to a dataframe

# Assuming you have a multinomial model with 3 classes
coefficients_class1 <- as.data.frame(as.matrix(coefficients[[1]]))
coefficients_class2 <- as.data.frame(as.matrix(coefficients[[2]]))
coefficients_class3 <- as.data.frame(as.matrix(coefficients[[3]]))

all_coef <- cbind(coefficients_class1, coefficients_class2,coefficients_class3)

# Create new column names
new_col_names <- c(paste("Class1"),
                   paste("Class2"),
                   paste("Class3"))

# Rename the columns
colnames(all_coef) <- new_col_names
print(all_coef)

###### precision recall curve ###### 

# Create empty list to store precision-recall data
pr_data_list <- list()

# Compute precision-recall data for each class
for (i in 1:3) {
  # Convert y_test to binary for the current class
  y_test_binary <- ifelse(y_test == i, 1, 0)
  
  # Calculate ROC curve
  roc_obj <- roc(y_test_binary, predicted_probs_matrix[, i])
  
  # Extract thresholds, sensitivities (recall), and specificities
  thresholds <- roc_obj$thresholds
  sensitivities <- roc_obj$sensitivities
  specificities <- roc_obj$specificities
  
  # Calculate precision: Precision = TP / (TP + FP)
  # Recall (sensitivity) = TP / (TP + FN)
  # Specificity = TN / (TN + FP)
  # Precision = Sensitivity / (Sensitivity + (1 - Specificity))
  precision <- sensitivities / (sensitivities + 1 - specificities)
  
  # Store the precision and recall data in a data frame
  pr_data_list[[i]] <- data.frame(
    Recall = sensitivities,
    Precision = precision,
    Class = as.factor(i)
  )
}

# Combine all classes into one data frame
pr_data <- do.call(rbind, pr_data_list)
pr_data_clean <- pr_data[!is.na(pr_data$Precision) & !is.na(pr_data$Recall), ]


# Plotting the precision-recall curves
precision_recall_curves_plot_ridge <- ggplot(pr_data_clean, aes(x = Recall, y = Precision, color = Class)) +
  geom_line() +
  labs(title = "Precision-Recall Curves", x = "Recall", y = "Precision") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave("img/precision_recall_curves_plot_ridge.png", plot = precision_recall_curves_plot_ridge, width = 12, height = 6, bg = "white")

