##############################################
##############################################
# Data Description
##############################################
##############################################

library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("student_health_data.csv")
data$Workload <- data$Study_Hours + data$Project_Hours

data$Gender <- as.factor(data$Gender)
data$Physical_Activity <- as.factor(data$Physical_Activity)
data$Sleep_Quality <- as.factor(data$Sleep_Quality)
data$Mood <- as.factor(data$Mood)
data$Health_Risk_Level <- as.factor(data$Health_Risk_Level)

# Objective Health Factors
# Demographics
# Age Distribution
age_plot <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")
print(age_plot)

# Gender Distribution
gender_plot <- ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")
print(gender_plot)

# Heart Rate Distribution
heart_rate_plot <- ggplot(data, aes(x = Heart_Rate)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Heart Rate Distribution", x = "Heart Rate", y = "Frequency") +
  scale_x_continuous(breaks = seq(min(data$Heart_Rate), max(data$Heart_Rate), by = 5))  # Set x-axis tick marks every 5 units
print(heart_rate_plot)

# Systolic Blood Pressure Distribution
bp_systolic_plot <- ggplot(data, aes(x = Blood_Pressure_Systolic)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Systolic Blood Pressure Distribution", x = "Systolic Blood Pressure", y = "Frequency") +
  scale_x_continuous(breaks = seq(min(data$Blood_Pressure_Systolic, na.rm = TRUE), 
                                  max(data$Blood_Pressure_Systolic, na.rm = TRUE), by = 5))  # Adjust tick marks
print(bp_systolic_plot)

# Diastolic Blood Pressure Distribution
bp_diastolic_plot <- ggplot(data, aes(x = Blood_Pressure_Diastolic)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Diastolic Blood Pressure Distribution", x = "Diastolic Blood Pressure", y = "Frequency") +
  scale_x_continuous(breaks = seq(min(data$Blood_Pressure_Diastolic, na.rm = TRUE), 
                                  max(data$Blood_Pressure_Diastolic, na.rm = TRUE), by = 5))  # Adjust tick marks
print(bp_diastolic_plot)

# Stress Level (Biosensor) Distribution
stress_biosensor_plot <- ggplot(data, aes(x = Stress_Level_Biosensor)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Stress Level (Biosensor) Distribution", x = "Stress Level (Biosensor)", y = "Frequency")
print(stress_biosensor_plot)

# Stress Level (Self-Report) Distribution
stress_self_report_plot <- ggplot(data, aes(x = Stress_Level_Self_Report)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(title = "Stress Level (Self-Report) Distribution", x = "Stress Level (Self-Report)", y = "Frequency")
print(stress_self_report_plot)

# Subjective Factors
# Physical Activity Levels
physical_activity_plot <- ggplot(data, aes(x = Physical_Activity)) +
  geom_bar(fill = "green") +
  labs(title = "Physical Activity Levels", x = "Physical Activity", y = "Count")
print(physical_activity_plot)

# Sleep Quality Distribution
sleep_quality_plot <- ggplot(data, aes(x = Sleep_Quality)) +
  geom_bar(fill = "purple") +
  labs(title = "Sleep Quality Distribution", x = "Sleep Quality", y = "Count")
print(sleep_quality_plot)

# Mood Distribution
mood_plot <- ggplot(data, aes(x = Mood)) +
  geom_bar(fill = "cyan") +
  labs(title = "Mood Distribution", x = "Mood", y = "Count")
print(mood_plot)

# Behavioral Factors
# Study Hours Distribution
study_hours_plot <- ggplot(data, aes(x = Study_Hours)) +
  geom_histogram(binwidth = 2, fill = "yellow", color = "black") +
  labs(title = "Study Hours Distribution", x = "Study Hours", y = "Frequency")
print(study_hours_plot)

# Project Hours Distribution
project_hours_plot <- ggplot(data, aes(x = Project_Hours)) +
  geom_histogram(binwidth = 2, fill = "brown", color = "black") +
  labs(title = "Project Hours Distribution", x = "Project Hours", y = "Frequency")
print(project_hours_plot)

# Workload Distribution (Workload = Study Hours + Project Hours)
library(ggplot2)

workload_plot <- ggplot(data, aes(x = Workload)) +
  geom_histogram(binwidth = 2, fill = "darkblue", color = "black") +
  labs(title = "Workload Distribution", x = "Workload (Study + Project Hours)", y = "Frequency") +
  theme_minimal()
print(workload_plot)

##############################################
##############################################
# Generalized Linear Regression
##############################################
##############################################

data <- read.csv("student_health_data.csv")
str(data)

# Change all text data into numerical data
data$Physical_Activity <- as.numeric(factor(data$Physical_Activity, levels = c("Low", "Moderate", "High")))
data$Sleep_Quality <- as.numeric(factor(data$Sleep_Quality, levels = c("Poor", "Moderate", "Good"))) 
data$Gender <- ifelse(data$Gender == "M", 1, 0)
data$Mood <- as.numeric(factor(data$Mood, levels = c( "Stressed","Neutral","Happy")))
data$Health_Risk_Level <- as.numeric(factor(data$Health_Risk_Level, levels = c("Low", "Moderate", "High"))) 


# For Health risk level, assign moderate & low risk level into 0, high risk level into 1
# Assign 1, 2 to 0 and 3 to 1 in a new column
# Add a new column Binary_Health_Risk to the original dataset 'data'
data$Binary_Health_Risk <- ifelse(data$Health_Risk_Level %in% c(1, 2), 0, 
                                  ifelse(data$Health_Risk_Level == 3, 1, NA))

# View the updated dataset
str(data)
print(data)


# Split the dataset into training and testing sets
training_data <- data[1:800, ]
testing_data <- data[801:1000, ]


# Train the initial model using glm
model <- glm(Binary_Health_Risk ~ Age + Gender + Heart_Rate + Blood_Pressure_Systolic + 
               Blood_Pressure_Diastolic + Stress_Level_Biosensor + Stress_Level_Self_Report + 
               Physical_Activity + Sleep_Quality + Mood + Study_Hours + Project_Hours, 
             data = training_data, family = binomial)
summary(model)


# Train the final model using glm
final_model <- glm(Binary_Health_Risk ~ Age + Stress_Level_Biosensor + Stress_Level_Self_Report + 
                     Physical_Activity + Sleep_Quality, data = training_data, family = binomial)
summary(final_model)


# Generate regression equation expression
coefficients <- coef(final_model)
logit_equation <- paste0("logit(mu) = ",
                         sprintf("%.3f", coefficients[1]), 
                         paste(sprintf(" + %.3f * %s", coefficients[-1],
                                       names(coefficients)[-1]), collapse = ""))
prob_equation <- paste0("mu = 1 / (1 + exp(-(", logit_equation, ")))")

cat("Logit_formula:\n", logit_equation, "\n")
cat("mu_formula:\n", prob_equation, "\n")


# Prediction
final_predictions <- predict(final_model, newdata = testing_data, type = "response")
predicted_classes <- ifelse(final_predictions > 0.5, 1, 0) 
actual_classes <- testing_data$Binary_Health_Risk

table(predicted_classes,actual_classes)
(167+15)/200
mean(predicted_classes == actual_classes)

##############################################
##############################################
# Decision Tree Model
##############################################
##############################################

install.packages("tree")
library(tree)
# Load the dataset (replace with the actual file path)
student_health_data <- read.csv("student_health_data.csv")

# View the structure and dimensions of the dataset
str(student_health_data)
dim(student_health_data)

# Check for missing values
sum(is.na(student_health_data))

### Classification Decision Tree
# Preprocessing
# Convert categorical variables to factors
student_health_data$Gender <- as.factor(student_health_data$Gender)
student_health_data$Physical_Activity <- as.factor(student_health_data$Physical_Activity)
student_health_data$Sleep_Quality <- as.factor(student_health_data$Sleep_Quality)
student_health_data$Mood <- as.factor(student_health_data$Mood)
student_health_data$Health_Risk_Level <- as.factor(student_health_data$Health_Risk_Level)

# Check the structure of the dataset
str(student_health_data)

##############################################
# Split Data into Training and Testing Sets
##############################################
set.seed(100) # For reproducibility
train_indices <- 1:800
train_data <- student_health_data[train_indices, ]
test_data <- student_health_data[-train_indices, ]

##############################################
# Build Classification Tree
##############################################
# Fit the tree
tree_model <- tree(Health_Risk_Level ~ . - Student_ID, data = train_data)
summary(tree_model)

# Visualize the tree
plot(tree_model)
text(tree_model, pretty = 0)

# Evaluate on Test Data
# Predict on test data
tree_predictions <- predict(tree_model, test_data, type = "class")

# Confusion Matrix and Accuracy
confusion_matrix <- table(tree_predictions, test_data$Health_Risk_Level)
print(confusion_matrix)
test_accuracy <- mean(tree_predictions == test_data$Health_Risk_Level)
print(paste("Test Accuracy:", round(test_accuracy, 4)))

##############################################
# Prune the Tree
##############################################
# Perform cross-validation to determine optimal tree size
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
plot(cv_tree$size, cv_tree$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

# Prune the tree
optimal_size <- which.min(cv_tree$dev)
optimal_size
pruned_tree <- prune.misclass(tree_model, best = cv_tree$size[optimal_size])

# Visualize the pruned tree
plot(pruned_tree)
text(pruned_tree, pretty = 0)

##############################################
# Evaluate Pruned Tree
##############################################
# Predict using the pruned tree
pruned_predictions <- predict(pruned_tree, test_data, type = "class")

# Confusion Matrix and Accuracy for Pruned Tree
pruned_confusion_matrix <- table(pruned_predictions, test_data$Health_Risk_Level)
print(pruned_confusion_matrix)
pruned_test_accuracy <- mean(pruned_predictions == test_data$Health_Risk_Level)
print(paste("Pruned Test Accuracy:", round(pruned_test_accuracy, 4)))

##############################################
# Compare Results
##############################################
print(paste("Original Test Accuracy:", round(test_accuracy, 4)))
print(paste("Pruned Test Accuracy:", round(pruned_test_accuracy, 4)))


