## ----message=FALSE, warning=FALSE----------------------------------------
################################
#  Install packages (if not installed)
################################
repos_path<- "http://cran.us.r-project.org"
if(!require(tidyverse)) install.packages("tidyverse", repos =repos_path)
if(!require(caret)) install.packages("caret", repos = repos_path)
if(!require(data.table)) install.packages("data.table", repos =repos_path)
if(!require(lubridate)) install.packages("lubridate", repos = repos_path)
if(!require(dplyr)) install.packages("dplyr", repos = repos_path)
if(!require(sjmisc)) install.packages("dplyr", repos = repos_path)
if(!require(scales)) install.packages("scales", repos = repos_path)
if(!require(caret)) install.packages("caret", repos = repos_path)
if(!require(gam)) install.packages("gam", repos = repos_path)

################################
# Load libraries
################################
library(lubridate)
library(tidyverse)
library(dplyr)
library(lubridate)
library(sjmisc)
library(scales)
library(caret)
library(gam)


## ----message=FALSE, warning=FALSE----------------------------------------
################################
# Downloading data
################################
# Indian Live Patient Records :
 # https://www.kaggle.com/uciml/indian-liver-patient-records/
 # https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian Liver Patient Dataset (ILPD).csv

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian Liver Patient Dataset (ILPD).csv"

# Download csv
liverData <- read.csv(url)

# Rename columns of csv (follow the webpage for naming convention)
colnames(liverData)<- c("Age","Gender","Total_Bilirubin","Direct_Bilirubin", "Alkaline_Phosphotase","Alamine_Aminotransferase","Aspartate_Aminotransferase",	"Total_Protiens","Albumin","Albumin_and_Globulin_Ratio","Dataset")

################################
# Creating training and validation sets
################################

# Validation set will be 10% of whole data
set.seed(1)
test_index <- createDataPartition(y = liverData$Dataset, times = 1, p = 0.1, list = FALSE)

training <- liverData[-test_index,]
validation <- liverData[test_index,]

 # Removing the objects from environment as no longer required
rm(liverData)



## ------------------------------------------------------------------------
# Looking at distributions of liver disease
# 1 indicates that the liver is damage. While 2 means that the liver is healthy 
print("Training Data")
table(training$Dataset)
print("Validation Data")
table(validation$Dataset)



## ------------------------------------------------------------------------
head(training)


## ------------------------------------------------------------------------
sprintf("Rows of training dataset = %d", nrow(training))
print("=========================")
summary(training)


## ------------------------------------------------------------------------
print("Validation Dataset")
summary(validation)


## ------------------------------------------------------------------------
# Replace null values with the mean of the variable
training$Albumin_and_Globulin_Ratio[is.na(training$Albumin_and_Globulin_Ratio)] <- mean(training$Albumin_and_Globulin_Ratio, na.rm=TRUE)


## ------------------------------------------------------------------------
# Adding a new column, which will contain the disease information
# M -> Malignant
# B -> Benign
training <- transform(training, LiverDisease= ifelse(Dataset==1, "M","B"))
validation <- transform(validation, LiverDisease= ifelse(Dataset==1, "M","B"))

# Deleting the column 'Dataset' as no longer required
training<-within(training, rm(Dataset))
validation<-within(validation, rm(Dataset))

# Displaying the first six rows
head(training)


## ------------------------------------------------------------------------
summary(training$LiverDisease)/nrow(training)*100.0


## ------------------------------------------------------------------------
sprintf("Minimum age = %d",min(training$Age))
sprintf("Maximum age = %d",max(training$Age))


## ------------------------------------------------------------------------
# Extracting frequency of patient ages
age_stats <-as.data.frame(table(training$Age))
names(age_stats)<- c("Age","Count")

# Remvoing the factor
age_stats$Age<-as.numeric(levels(age_stats$Age))

# Plotting distribution of ages
age_stats %>% ggplot(aes(Age, Count)) +
  geom_point(color="cadetblue") +
  scale_x_continuous(breaks = round(seq(min(age_stats$Age), 
                                        max(age_stats$Age), by = 6),1)) +
  ggtitle("Distribution of Patient Ages")



## ------------------------------------------------------------------------
# Plotting distributions of ages based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Age, color=LiverDisease)) +
  geom_point() +
  labs(y="Age", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of ages based on liver disease")


## ------------------------------------------------------------------------
# Getting summary of genders
summary(training$Gender)


## ------------------------------------------------------------------------
# Plotting distributions of Total_Bilirubin based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Total_Bilirubin, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Total_Bilirubin", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Total_Bilirubin based on liver disease")



## ------------------------------------------------------------------------
# Plotting distributions of Direct_Bilirubin based on liver disease
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Direct_Bilirubin, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Direct_Bilirubin", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Direct_Bilirubin based on liver disease")



## ------------------------------------------------------------------------
# Making a subset of data
subset_train <- training[c("Total_Bilirubin","Direct_Bilirubin","LiverDisease")]
# Converting disease variable to numeric format
subset_train <- transform(subset_train, LiverDisease= ifelse(subset_train$LiverDisease=="M", 1,0))
# Looking at the correlations
cor(subset_train) 


## ------------------------------------------------------------------------
# Plotting distributions of Alkaline Phosphotase based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Alkaline_Phosphotase, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Alkaline Phosphotase", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Alkaline Phosphotase based on liver disease")


## ------------------------------------------------------------------------
# Plotting distributions of Alamine Aminotransferase based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Alamine_Aminotransferase, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Alamine Aminotransferase", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Alamine Aminotransferase based on liver disease") 


## ------------------------------------------------------------------------
# Plotting distributions of Aspartate_Aminotransferase based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Aspartate_Aminotransferase, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Aspartate Aminotransferase", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Aspartate Aminotransferase based on liver disease") 


## ------------------------------------------------------------------------
# Making a subset of data
subset_train <- training[c("Alkaline_Phosphotase","Aspartate_Aminotransferase","LiverDisease")]

# Converting disease variable to numeric format
subset_train <- transform(subset_train, LiverDisease= ifelse(subset_train$LiverDisease=="M", 1,0))

# Looking at the coorelations
cor(subset_train) 


## ------------------------------------------------------------------------
# Plotting distributions of Total Protiens based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Total_Protiens, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Total Protiens", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Total Protiens based on liver diseases") 


## ------------------------------------------------------------------------
# Plotting distributions of Albumin based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Albumin, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Albumin", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Albumin based on liver disease") 


## ------------------------------------------------------------------------
# Plotting distributions of Albumin based on liver diseases
training %>% 
  ggplot(aes(as.numeric(row.names(training)),Albumin_and_Globulin_Ratio, color=LiverDisease)) +
  geom_boxplot() +
  labs(y="Albumin and Globulin Ratio", x = "Number of patients")+
  facet_wrap( ~ LiverDisease) +
  ggtitle("Distribution of Albumin and Globulin Ratio based on liver disease") 


## ------------------------------------------------------------------------
# Removing the variables 'Age' and 'Dataset' 
training<-within(training, rm(Age,Total_Protiens))
validation<-within(validation, rm(Age,Total_Protiens))



## ----warning=FALSE, message=FALSE----------------------------------------
# Defining a cross-validation (10 K folds )
control <- trainControl(method = "repeatedcv", number =10,repeats = 5)

# Train logistic regression model
train_glm <- train(LiverDisease ~., 
                   method = "glm",
                   preProc = c("zv","center", "scale"),
                   data = training,
                   trControl=control)

# Showing the accuracy
sprintf("The accuracy of GLM = %f",train_glm$results$Accuracy)

# Storing the results
model_results <- data_frame(method = "glm", Accuracy = train_glm$results$Accuracy)


## ----warning=FALSE,message=FALSE-----------------------------------------

set.seed(1)
# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats=5)

# Train knn model
train_knn <- train(LiverDisease~ .,
                   method = "knn", 
                   preProc = c("zv","center", "scale"),
                   data = training,
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = control)

# Plot the model and highlight the best result
ggplot(train_knn, highlight = TRUE) +
  ggtitle(paste("The best accuracy = ",round(max(train_knn$results$Accuracy),3)))

# Storing the results
model_results <- bind_rows(model_results,data_frame(method="knn",  
                                     Accuracy = max(train_knn$results$Accuracy) ))



## ---- warning=FALSE, message=FALSE---------------------------------------
set.seed(1)

# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats=5)

# Define tuning parameters
tune_grid <- expand.grid(span = seq(0.15, 0.65, len = 15), degree = seq(0,1,0.25))

# Train the model
train_loess <- train(LiverDisease~.,
                   method = "gamLoess",
                   preProc = c("zv","center", "scale"),
                   data = training,
                   tuneGrid= tune_grid, 
                   trControl = control)

# Plot the model and highlight the best result
ggplot(train_loess, highlight = TRUE) +
  ggtitle(paste("The best accuracy = ",round(max(train_loess$results$Accuracy),3)))


# Storing the results
model_results <- bind_rows(model_results,data_frame(method="loess",  
                                     Accuracy = max(train_loess$results$Accuracy) ))




## ---- warning=FALSE, message=FALSE---------------------------------------
set.seed(1)

# Define tuning parameters
tune_grid <- expand.grid(ncomp = seq(1,5, len = 10))

# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats=5)

# Train the model
train_pls <- train(LiverDisease~.,
                   method = "pls",
                   preProc = c("zv","center", "scale"),
                   data = training,
                   tuneGrid= tune_grid,
                   trControl = control)

# Plot the model and highlight the best result
ggplot(train_pls, highlight = TRUE) +
  ggtitle(paste("The best accuracy = ",round(max(train_pls$results$Accuracy),3)))


# Storing the results
model_results <- bind_rows(model_results,data_frame(method="pls",  
                                     Accuracy = max(train_pls$results$Accuracy) ))




## ---- warning=FALSE, message=FALSE---------------------------------------
set.seed(1)
# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number=10, repeats=5)

# Train the model
train_lda <- train(LiverDisease~., 
                   method = "lda",
                   preProc = c("zv","center", "scale"),
                   data = training,
                   trControl = control)

sprintf("The accuracy of lda = %f",max(train_lda$results$Accuracy))

# Storing the results
model_results <- bind_rows(model_results,data_frame(method="lda",  
                                     Accuracy = max(train_lda$results$Accuracy) ))




## ---- warning=FALSE, message=FALSE---------------------------------------
set.seed(1)
# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number=10, repeats=5)

# Train the model
train_qda <- train(LiverDisease~.,
                   method = "qda",
                   preProc = c("zv","center", "scale"),
                   data = training,
                   trControl = control)

sprintf("The accuracy of qda = %f",max(train_qda$results$Accuracy))

# Storing the results
model_results <- bind_rows(model_results,data_frame(method="qda",  
                                     Accuracy = max(train_qda$results$Accuracy) ))




## ---- warning=FALSE, message=FALSE---------------------------------------
set.seed(1)

# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats=5)

# Train the model
train_rpart <- train(LiverDisease~., 
                     method = "rpart",
                     preProc = c("zv","center", "scale"),
                     data = training,
                     trControl = control,
                     tuneGrid = data.frame(cp = seq(0, 0.08, len = 10)))

# Plot the model and highlight the best result
ggplot(train_rpart, highlight = TRUE) +
  ggtitle(paste("The best accuracy = ",round(max(train_rpart$results$Accuracy),3)))

# Storing the results
model_results <- bind_rows(model_results,data_frame(method="rpart",  
                                     Accuracy = max(train_rpart$results$Accuracy) ))




## ---- warning=FALSE, message=FALSE---------------------------------------
set.seed(1)

# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats=5)

# Train the model
train_rf <- train(LiverDisease~.,
                     method = "rf",
                     preProc = c("zv","center", "scale"),
                     data = training,
                     trControl = control,
                     tuneGrid = data.frame(mtry=seq(1,7)),
                     ntree=100)

# Plot the model and highlight the best result
ggplot(train_rf, highlight = TRUE) +
  ggtitle(paste("The best accuracy = ",round(max(train_rf$results$Accuracy),3)))

# Storing the results
model_results <- bind_rows(model_results,data_frame(method="rf",  
                                     Accuracy = max(train_rf$results$Accuracy) ))



## ---- warning=FALSE, message=FALSE---------------------------------------
set.seed(1)

# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Train the model
train_svm <- train(LiverDisease~.,
                   preProc = c("zv","center", "scale"),
                   data = training,
                   method = "svmLinear",
                   tune_grid= data.frame(tau=seq(1,10)),
                   trControl = control)

sprintf("The accuracy of svm = %f",max(train_svm$results$Accuracy))

# Storing the results
model_results <- bind_rows(model_results,data_frame(method="svm",  
                                     Accuracy = max(train_svm$results$Accuracy) ))



## ------------------------------------------------------------------------
set.seed(1)

# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Tuning the model
tune_grid  = expand.grid(method = c("Adaboost.M1", "Real adaboost"))
 
train_ada <- train(LiverDisease~.,
                   preProc = c("zv","center", "scale"),
                   data = training,
                   method = "ada",
                   trControl = control)

sprintf("The accuracy of adaboost = %f",max(train_ada$results$Accuracy))

# Stroing the results
model_results <- bind_rows(model_results,data_frame(method="ada",  
                                     Accuracy = max(train_ada$results$Accuracy) ))



## ------------------------------------------------------------------------
set.seed(1)

# Defining a cross validation (10 K folds )
control <- trainControl(method = "repeatedcv", number = 10, repeats=5)
# Train the model
train_rf_pca <- train(LiverDisease~.,
                     method = "rf",
                     preProc = c("zv","center", "scale"),
                     data = training,
                     trControl = control,
                     tuneGrid = data.frame(mtry=seq(1,7)),
                     preProcess=c("pca"),
                     ntree=100)

# Plot the model and highlight the best result
ggplot(train_rf_pca, highlight = TRUE) +
  ggtitle(paste("The best accuracy = ",round(max(train_rf_pca$results$Accuracy),3)))

# Storing the results
model_results <- bind_rows(model_results,data_frame(method="rf_pca",  
                                     Accuracy = max(train_rf_pca$results$Accuracy) ))





## ------------------------------------------------------------------------
model_results


## ------------------------------------------------------------------------
# collect resamples
results <- resamples(list(GLM=train_glm, 
                          KNN = train_knn,
                          LOESS=train_loess,
                          PLS= train_pls,
                          LDA = train_lda,
                          QDA = train_qda,
                          RPART = train_rpart,
                          RF = train_rf,
                          SVM = train_svm,
                          ADA= train_ada,
                          RF_PCA = train_rf_pca))
# boxplots of results
bwplot(results)


## ------------------------------------------------------------------------
# Function to display a confusion matrix
# Code Source:
# https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package

draw_confusion_matrix <- function(cm, title) {

  total <- sum(cm$table)
  res <- as.numeric(cm$table)

  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }

  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(title, cex.main=2)

  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)

  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
 
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}



## ------------------------------------------------------------------------
# Creating an empty data frame to hold the results of models 
# across validation dataset
ml_results<-data_frame()

# Function to compute all stats from models
evaluate_performance <- function(model_name, model, validation,model_results,title) 
{
  # Generating predictions
  predictions<-predict(model, validation)
  
   # Draw the confusion matrix
  cm<-confusionMatrix(predictions,validation$LiverDisease,positive="M")
  
  draw_confusion_matrix(cm,title)
 
  # Generate metrics
  sensitivty<-as.numeric(cm$byClass[1])
  specificity<-as.numeric(cm$byClass[2])
  precision<-as.numeric(cm$byClass[5])
  recall<-as.numeric(cm$byClass[6])
  f1_score<-as.numeric(cm$byClass[7])
  accuracy<-as.numeric(cm$overall[1])
  kappa <-as.numeric(cm$overall[2])
  
  # Store metrics to a data frame
  ml_results <- bind_rows(ml_results, data_frame(Models = model_name,
             Accuracy = accuracy,
             Precision= precision,
             Sensitivty=sensitivty,
             Specificity=specificity,
             F1_Score = f1_score,
             Kappa= kappa))
}

# Evaluating the performance of models
ml_results<-evaluate_performance("glm",train_glm,validation,ml_results,"Confusion Matrix - glm")


## ------------------------------------------------------------------------
# Evaluate knn model
ml_results<-evaluate_performance("knn",train_knn,validation,ml_results,"Confusion Matrix - knn")



## ------------------------------------------------------------------------
#Evaluate loess model
ml_results<-evaluate_performance("loess",train_loess,validation,ml_results,"Confusion Matrix - loess")


## ------------------------------------------------------------------------
ml_results<-evaluate_performance("pls",train_pls,validation,ml_results,"Confusion Matrix -pls")


## ------------------------------------------------------------------------
ml_results<-evaluate_performance("lda",train_lda,validation,ml_results, "Confusion Matrix - lda")


## ------------------------------------------------------------------------
ml_results<-evaluate_performance("rpart",train_rpart,validation,ml_results, "Confusion Matrix - rpart")


## ------------------------------------------------------------------------
ml_results<-evaluate_performance("rf",train_rf,validation,ml_results,"Confusion Matrix - rf")


## ------------------------------------------------------------------------
ml_results<-evaluate_performance("svmlinear",train_svm,validation,ml_results, "Confusion Matrix - svm")

## ------------------------------------------------------------------------
ml_results<-evaluate_performance("ada",train_ada,validation,ml_results,"Confusion Matrix - adaboost")

## ------------------------------------------------------------------------
ml_results<-evaluate_performance("rf_pca",train_rf_pca,validation,ml_results,"Confusion Matrix - rf pca")


## ---- message=FALSE, warning=FALSE---------------------------------------

# Generating prediction of all models
glm_predictions<-predict(train_glm, validation)
knn_predictions<-predict(train_knn, validation)
loess_predictions<-predict(train_loess, validation)
pls_predictions<-predict(train_pls, validation)
lda_predictions<-predict(train_lda, validation)
rpart_predictions<-predict(train_rpart, validation)
rf_predictions<-predict(train_rf, validation)
svmlinear_predictions<-predict(train_svm, validation)
ada_predictions<-predict(train_ada, validation)
rf_pca_predictions<-predict(train_rf_pca, validation)

# Generate outputs fpr ensemble model
ensemble_pred<-data.frame(glm_predictions, 
                          knn_predictions,
                          loess_predictions,
                          pls_predictions, 
                          lda_predictions,
                          rpart_predictions, 
                          rf_predictions, 
                          svmlinear_predictions,
                          ada_predictions,
                          rf_pca_predictions)

# If 50% of the predictions say disease then we pick it a disease
votes <- rowMeans(ensemble_pred=="M")
ensemble_predictions <- ifelse(votes > 0.5, "M", "B") %>% factor()

# Generate metrics
cm<-confusionMatrix(ensemble_predictions,validation$LiverDisease,positive="M")
draw_confusion_matrix(cm,"Confusion Matrix - ensemble")
sensitivty<-as.numeric(cm$byClass[1])
specificity<-as.numeric(cm$byClass[2])
precision<-as.numeric(cm$byClass[5])
recall<-as.numeric(cm$byClass[6])
f1_score<-as.numeric(cm$byClass[7])
accuracy<-as.numeric(cm$overall[1])
kappa <-as.numeric(cm$overall[2])

# Store metrics to a data frame
ml_results <- bind_rows(ml_results, data_frame(Models = "Ensemble",
             Accuracy = accuracy,
             Precision= precision,
             Sensitivty=sensitivty,
             Specificity=specificity,
             F1_Score = f1_score,
             Kappa= kappa))


## ------------------------------------------------------------------------
ml_results

