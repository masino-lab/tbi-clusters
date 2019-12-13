# Import dependencies
library(modelr, quietly = TRUE)
library(purrr, quietly = TRUE)
library(knn, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(caret, quietly = TRUE)

# set working directory
local_path = paste(path, "8_Supervised_Learning", sep="")
setwd(local_path)

# set seed
set.seed(3033)

# Load data
load(paste(path,'3_GLRM_Clustering/avg_clustering_results.RData', sep = ""))
load(paste(path,'3_GLRM_Clustering/avg_subset.RData', sep = ""))
load(paste(path,'1_Data_Cleaning/track_tbi_predictors.RData', sep = ""))
row.names(avg_x) = names(cluster_labels)


training_data = cbind(avg_x, cluster_labels)
# make sure target variable is a factor 
is.factor(training_data$cluster_labels)

 
#Split into training and validation set (10% of data)
intrain <- createDataPartition(y = cluster_labels, p= 0.8, list = FALSE)
training <- training_data[intrain,]
testing <- training_data[-intrain,]

dim(training); dim(testing)
anyNA(training_data)
summary(training_data)



trctrl <- trainControl(method = "cv", number = 10)
knn_fit <- train(cluster_labels ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

# how accurate is model?
test_pred = predict(knn_fit, newdata = testing)

conf = confusionMatrix(test_pred, testing$cluster_labels)

'''
Confusion Matrix and Statistics

Reference
Prediction  1  2  3
1 75  3  7
2  4 86  0
3  5  0 62

Overall Statistics

Accuracy : 0.9215          
95% CI : (0.8801, 0.9521)
No Information Rate : 0.3678          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.8815          
Mcnemar\'s Test P-Value : NA              

Statistics by Class:
  
  Class: 1 Class: 2 Class: 3
Sensitivity            0.8929   0.9663   0.8986
Specificity            0.9367   0.9739   0.9711
Pos Pred Value         0.8824   0.9556   0.9254
Neg Pred Value         0.9427   0.9803   0.9600
Prevalence             0.3471   0.3678   0.2851
Detection Rate         0.3099   0.3554   0.2562
Detection Prevalence   0.3512   0.3719   0.2769
Balanced Accuracy      0.9148   0.9701   0.9348
'''

print(knn_fit)

save(knn_fit, file = 'knn_fit.RData')


n = sum(conf$table) # number of instances
nc = nrow(conf$table) # number of classes
diag = diag(conf$table) # number of correctly classified instances per class 
rowsums = apply(conf$table, 1, sum) # number of instances per class
colsums = apply(conf$table, 2, sum) # num

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 


# Predict which phenotypes TRACK-TBI observations fit into
track_tbi_predicted_clusters = predict(knn_fit, newdata = track_tbi_predictors)
track_clusters = as.data.frame(track_tbi_predicted_clusters, row.names = rownames(track_tbi_predictors))

save(track_clusters, file = 'track_clusters.RData')


