# Read the SMS Spam Collection dataset from a text file.
# The file is tab-separated, so we specify sep="\t".
# We also set stringsAsFactors = FALSE to keep text data as character strings.
# The quote="" argument ensures that quotes in the text are not treated as special characters.
sms <- read.table("SMSSpamCollection.txt", sep="\t", stringsAsFactors = FALSE, quote="")

# Assign column names: "Type" (spam or ham) and "Message" (SMS content).
names(sms) <- c("Type", "Message")

# Display the first few rows of the dataset to understand its structure.
head(sms)

# Count the number of spam and ham messages in the dataset.
table(sms$Type)

#display ten columns for the first ten rows:
sms_converted<-read.csv("SMS_converted.csv")
sms_converted[1:10, 1:10]

# Filter spam messages from the dataset.
# Select only rows where Type is "spam" and exclude the last column (Type).
spam <- sms_converted[sms_converted$Type == "spam", -ncol(sms_converted)]

# Convert term frequency into binary presence/absence.
# If a word appears in a message (count > 0), mark it as 1 (present), otherwise 0 (absent).
spam <- ifelse(spam > 0, 1, 0)

# Calculate word frequency across all spam messages.
# This is done by summing the occurrences of each word (column-wise) and
# dividing by the total number of spam messages.
spam_freq <- apply(spam, 2, sum) / nrow(spam)

# Sort the word frequencies in descending order.
spam_freq_ordered <- spam_freq[order(spam_freq, decreasing = TRUE)]

# Select the top 20 most frequent words in spam messages.
spam_top <- data.frame(freq = spam_freq_ordered[1:20])

# Load ggplot2 for visualization.
library(ggplot2)

# Create a bar plot of the top 20 words in spam messages.
ggplot(spam_top, aes(x = rownames(spam_top), y = freq)) +
  geom_bar(stat = "identity") + # Create a bar chart.
  theme(axis.text.x = element_text(angle = 90)) # Rotate x-axis labels for readability.

# REPEAT for "ham" type messages

# filter spam messages
ham<-sms_converted[sms_converted$Type=="ham",-ncol(sms_converted)]

# to calculate the frequency in the all documents we need to change the individual document
#frequency to the presence or absence of the word
ham<- ifelse(ham >0, 1, 0)
ham_freq<-apply(ham, 2, sum)/nrow(ham)

# sort vector with decreasing order
ham_freq_ordered<- ham_freq[order(ham_freq, decreasing =T)]

# filter top 20
ham_top <-data.frame(freq = ham_freq_ordered[1:20])
ggplot(ham_top, aes(x=rownames(ham_top), y=freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))

# Check to see any mussing vales
library(Amelia)
missmap(sms_converted)

# Data Conversion - naive Bayes classifier is typically trained on data 
# with categorical features

# Define a function to convert word counts into categorical values
convert_counts <- function(x) {
  
  # Convert numerical word count to binary presence/absence.
  # If x > 0 (word appears), set it to 1; otherwise, set it to 0.
  x <- ifelse(x > 0,1,0)
  
  # Convert the binary values into a factor with labels:
  # 0 → "No" (word is absent), 1 → "Yes" (word is present).
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  
  # Return the transformed factor variable.
  return(x)
}

# Apply the convert_counts function to all columns except the last one (Type).
# This converts word counts into categorical factors ("No" or "Yes").
sms_converted[, -ncol(sms_converted)] <- apply(sms_converted[, -ncol(sms_converted)],
                                               2, convert_counts)

# Convert the "Type" column (spam or ham) into a factor variable.
# This ensures that classification models treat it as a categorical variable.
sms_converted$Type <- as.factor(sms_converted$Type)

# Data Split
# split data set into 70% train and 30% test datasets.

# Load the caret package for data partitioning and machine learning utilities.
library(caret)

# Create a training and testing split (70% training, 30% testing).
# The createDataPartition function ensures a stratified split,
# preserving the proportion of spam and ham messages in both sets.
ind <- createDataPartition(sms_converted$Type, p = 0.7, list = FALSE)

# Subset the data into training and testing sets.
train <- sms_converted[ind, ] # 70% of the data
test <- sms_converted[-ind, ] # Remaining 30%

# Display the percentage distribution of spam and ham messages in the training set.
prop.table(table(train$Type)) * 100

# Display the percentage distribution of spam and ham messages in the testing set.
prop.table(table(test$Type)) * 100

# Training a model
# Apply the naive Bayes algorithm
library("e1071")

# The naiveBayes() function is performed in a single function call: 
# naiveBayes(train, class, laplace = 0)

# Train a Naive Bayes classifier.
# - `train[, -ncol(train)]` selects all columns except the last one (features only).
# - `train$Type` is the target variable (spam or ham).
sms_classifier <- naiveBayes(train[, -ncol(train)], train$Type)

# Print the trained Naive Bayes model to see learned probabilities.
sms_classifier

# To make prediction the predict() function is called:
# predict(m, test, type = ”class”)

# Make predictions on the training set using the trained Naive Bayes model.
# `type="class"` returns the predicted class labels ("spam" or "ham").
predicted <- predict(sms_classifier, train[, -ncol(train)], type = "class")

# Display the first 100 predicted class labels.
predicted[1:100]

# Make predictions on the training set but return raw probabilities instead.
# `type="raw"` returns the probability for each class (spam and ham).
predicted <- predict(sms_classifier, train[, -ncol(train)], type = "raw")

# Display the first 100 rows of predicted probabilities.
predicted[1:100]

# Using Caret

# Load the caret package for training models with cross-validation.
library(caret)

# Train a Naive Bayes model using caret's train function.
# - `train[, -ncol(train)]`: Use all columns except the last one (features).
# - `train$Type`: The target variable (spam or ham).
# - `method = "naive_bayes"`: Specifies using the Naive Bayes algorithm for training.
# - `trControl = trainControl(method="cv", number = 5)`: Use 5-fold cross-validation to evaluate model performance. 
# - `na.action = na.pass`: Handle missing values by passing them without removing the rows.
nb_fit1 <- train(train[, -ncol(train)], train$Type,
                 method = "naive_bayes",
                 trControl = trainControl(method = "cv", number = 5),
                 na.action = na.pass)

# Print the model details to see training results.
nb_fit1

# Load the caret package for training models with cross-validation.
library(caret)

# Train a Naive Bayes model using caret's train function with custom tuning parameters.
# - `train[,-ncol(train)]`: Use all columns except the last one (features).
# - `train$Type`: The target variable (spam or ham).
# - `method = "naive_bayes"`: Specifies using the Naive Bayes algorithm for training.
# - `trControl = trainControl(method="none")`: No resampling method is used (i.e., no cross-validation)# - `tuneGrid = data.frame(laplace=1, usekernel=T, adjust=0)`: Manually sets the tuning parameters:
# - `laplace = 1`: Applies Laplace smoothing (helps avoid zero probabilities for unseen words).
# - `usekernel = TRUE`: Applies kernel smoothing to the probability distributions.
# - `adjust = 0`: Keeps the default adjustment for the probability estimates.
nb_fit2 <- train(train[,-ncol(train)], train$Type,
                 method = "naive_bayes",
                 trControl = trainControl(method = "none"),
                 tuneGrid = data.frame(laplace = 1, usekernel = TRUE, adjust = 0))

# Print the trained model details to see results.
nb_fit2

#Model Evaluation
# Make predictions on the test set using the trained Naive Bayes model (sms_classifier)
sms_test_pred <- predict(sms_classifier, test)

# Generate a confusion matrix to evaluate the model's performance
# The confusion matrix compares the predicted class labels with the true class labels (test$Type)
cm <- confusionMatrix(sms_test_pred, test$Type)
# Print the confusion matrix to see the results
cm

# evaluate nb_fit1 model
sms_test_pred <- predict(nb_fit1, newdata=test[, -ncol(test)])
cm<-confusionMatrix(sms_test_pred, test$Type)
cm
# evaluate nb_fit2 model
sms_test_pred <- predict(nb_fit2, newdata=test[, -ncol(test)])
cm<-confusionMatrix(sms_test_pred, test$Type)
cm

# Model OPtimisation (Tuning)
# Train a Naive Bayes classifier
# Using the training data (train) and the target variable (train$Type)
sms_classifier2 <- naiveBayes(
  train[, -ncol(train)], # Select all columns except the last one (features)
  train$Type, # The target variable (the class: "ham" or "spam")
  laplace = 1 # Apply Laplace smoothing to handle zero probabilities
)

#Make predictions

# Predict the class labels for the test data using the trained Naive Bayes model
sms_test_pred2 <- predict(sms_classifier2, test)

# Apply the trained model (sms_classifier2) to the test data
# Calculate the confusion matrix by comparing the predicted values with the actual values
cm <- confusionMatrix(sms_test_pred2, test$Type)

# The 'confusionMatrix' function compares predictions with actual target values (test$Type)
# Display the confusion matrix and associated evaluation metrics
cm # Print the confusion matrix result to check performance

# Create a grid of tuning parameters for Naive Bayes
# This grid will be used to try different combinations of the:
# 'laplace', 'usekernel', and 'adjust' parameters
grid <- data.frame(expand.grid(laplace=c(0,1), usekernel = c(FALSE, TRUE), adjust=c(0,1.0)))

# Train the Naive Bayes model using the training data (train)
# We perform cross-validation (5-fold) and tune the model using the grid of parameters
nb_fit <- train(
  train[,-ncol(train)], # Select all columns except the last one (features)
  train$Type, # The target variable (the class: "ham" or "spam")
  method = "naive_bayes", # Specify the Naive Bayes method
  trControl = trainControl(method="cv", number=5),
  
  # Cross-validation with 5 folds to evaluate model performance
  tuneGrid = grid # Provide the tuning grid for hyperparameter tuning
)
# Display the results of the trained model, including the best combination of tuning parameters
nb_fit

#ROC Evlauation

# Load the pROC library for ROC curve analysis
library(pROC)

# Predict the class probabilities (not just the predicted class)
# for the test data using the model nb_fit2
# 'type="prob"' ensures that the output is the predicted probabilities for each class (ham and spam)
sms_test_pred_prob2 <- predict(nb_fit2, newdata=test[, -ncol(test)], type="prob")

# Predict the class probabilities (not just the predicted class)
# for the test data using the model nb_fit
# Again, 'type="prob"' returns the predicted probabilities for each class (ham and spam)
sms_test_pred_prob <- predict(nb_fit, newdata=test[, -ncol(test)], type="prob")

head(sms_test_pred_prob)

# Create an ROC curve for the model nb_fit2 (non-optimized model)
# test$Type is the actual class labels, sms_test_pred_prob2[,2]
# gives the predicted probabilities for the "spam" class
res.roc2 <- roc(test$Type, sms_test_pred_prob2[, 2])

# Create an ROC curve for the model nb_fit (optimized model)
# test$Type is the actual class labels, sms_test_pred_prob[,2]
# gives the predicted probabilities for the "spam" class
res.roc <- roc(test$Type, sms_test_pred_prob[, 2])

# Plot the ROC curve for the optimized model (nb_fit)
# The color is set to red
plot.roc(res.roc, col="red")

# Add the ROC curve for the non-optimized model (nb_fit2) to the same plot
# The color is set to dark green and 'add=TRUE' ensures that
# it is plotted on top of the previous ROC curve
plot.roc(res.roc2, col="darkgreen", add=TRUE)

# Add a legend to the bottom-right corner of the plot to label the curves
# The legend shows the AUC values for both models (non-optimized and optimized)
legend("bottomright", c(paste('auc_NB_nonopt', round(res.roc2$auc, 2)),
                        paste('auc_NB_opt', round(res.roc$auc, 2))),
       lty=c(1, 1), lwd=c(1, 1), col=c('red', 'darkgreen'), cex=0.7)

#Balancing Dataset

# Set up the training control object for cross-validation
# "cv" indicates cross-validation, "number=5" means 5-fold cross-validation,
# and "sampling='up'" indicates that the training data should be upsampled to address class imbalance.
ctr <- trainControl(method="cv", number=5, sampling="up")

# Train the Naive Bayes model using the training data with cross-validation and upsampling
# The method="naive_bayes" specifies that we are using the Naive Bayes classifier
# The trControl=ctr argument applies the previously defined training control
nb_fit <- train(train[, -ncol(train)], train$Type, method = "naive_bayes", trControl = ctr)

# Make predictions on the test set using the trained Naive Bayes model
# test[, -ncol(test)] excludes the last column (the target variable) from the test data
sms_test_pred <- predict(nb_fit, test[, -ncol(test)])

# Create a confusion matrix to evaluate the model's predictions
# Compare the predicted classes (sms_test_pred) with the actual classes (test$Type)
cm <- confusionMatrix(sms_test_pred, test$Type)

# Display the confusion matrix and other evaluation metrics
cm
