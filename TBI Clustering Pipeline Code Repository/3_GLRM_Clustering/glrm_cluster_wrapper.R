# Import dependencies
library(h2o)
library(Rtsne)
library(cluster)
library(caret)
library(dplyr)

glrm_cluster_wrapper <-function(data, gamma_range = c(250:450), krange = c(3:6), loss_order= loss_order){
  # Set seed

  #format column names
  colnames(data) = tolower(gsub("[[:punct:]]", "", colnames(data)))
  # GLRM (Using H2o library)
  h2o.init(nthreads = -1, max_mem_size = "2G", enable_assertions = F)
  folds = createFolds(1:nrow(data), k = 5)
  
  labels_list = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  cols_list = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  bestGammas = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  bestK_list_training = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  maxSW_train = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  labels_list_test = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  test_maxSW = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  test_maxK = list(fold1=NA, fold2=NA, fold3=NA, fold4=NA, fold5=NA)
  
  count = 0
  for(fold in folds){
    count = count + 1
    print(count)
    
    test = data[fold,]
    train = data[-fold, ]
 
    gammaRange = gamma_range
    gammaList = c()
    bestKList = c()
    maxSWList = c()
    
    for(gamma in gammaRange){
      print(gamma)
      gammaList = c(gammaList, gamma)
      # Run model with training data
      
      #loss_order = c("Quadratic",rep("Hinge", 3),rep("Quadratic", 3),rep("Hinge",22),rep("Quadratic",18),rep("Hinge",10),rep("Quadratic",2),rep("Hinge",2),"Quadratic",rep("Hinge",47))
      
      glrm.mod = h2o.glrm(training_frame = as.h2o(train) ,transform="STANDARDIZE", k=2, loss_by_col = loss_order,
                          regularization_x = "L1", regularization_y = "L1", gamma_x = 0, gamma_y = gamma,
                          init="SVD", svd_method="GramSVD")
      # Select columns with non-zero weights
      nonzero_cols = colnames(glrm.mod@model$archetypes)[colSums(glrm.mod@model$archetypes) != 0]
      
      if(length(nonzero_cols) == 0){
        break
      }else{
        # Subset test dataframe with selected features
        subset = select(train, nonzero_cols)
      }
      
      # Make distance matrix
      
      asymms = c()
      for(col in 1:dim(subset)[2]){
        if(length(unique(subset[,col])) == 2){
          # Change boolean variable data type to factor 
          subset[,col] = ifelse(subset[,col]=="1", TRUE, FALSE)
          asymms = c(asymms, col)
          
        }
      }
      
      if(length(asymms) > 0) {
        gower_dist_matrix  <- as.matrix(daisy(subset, metric = "gower", warnBin = FALSE, type = list(asymm=(asymms))))
      } else {
        gower_dist_matrix  <- as.matrix(daisy(subset, metric = "gower", warnBin = FALSE))
      }
      
      # Find cluster number with highest silhouette width
      krange = krange
      sil_width = c()
      for(k in krange){
        pam_fit = pam(gower_dist_matrix, diss = TRUE, k = k)
        sil_width = c(sil_width, pam_fit$silinfo$avg.width)
      }
      
      maxK = krange[which.max(sil_width)]
      max_sil_width = max(sil_width)
      bestKList = c(bestKList, maxK)
      maxSWList = c(maxSWList, max_sil_width)
      
    }
    
    # To find features where K !=2
    best_gamma = gammaList[which.max(maxSWList[which(bestKList!=2)])]
    best_K = bestKList[which.max(maxSWList[which(bestKList!=2)])]
    maxSW_train[[count]] = max(maxSWList)
    glrm.mod = h2o.glrm(training_frame = as.h2o(train) ,transform="STANDARDIZE", k=2, loss_by_col = loss_order,
                        regularization_x = "L1", regularization_y = "L1", gamma_x = 0, gamma_y = best_gamma,
                        init="SVD", svd_method="GramSVD", seed=123)
    nonzero_cols = colnames(glrm.mod@model$archetypes)[colSums(glrm.mod@model$archetypes) != 0]
    
    train_subset = select(train, nonzero_cols)
    test_subset = select(test, nonzero_cols)
    
    gower_dist_matrix_train  <- as.matrix(daisy(train_subset, metric = "gower", warnBin = FALSE))
    pam_fit_train = pam(gower_dist_matrix_train, diss = TRUE, k=best_K)
    
    
    gower_dist_matrix_test  <- as.matrix(daisy(test_subset, metric = "gower", warnBin = FALSE))
    pam_fit_test = pam(gower_dist_matrix_test, diss = TRUE, k=best_K)
    
    test_maxK[[count]] = maxK
    test_maxSW[[count]] = max_sil_width
    labels_list[[count]] = factor(pam_fit_train$clustering)
    labels_list_test[[count]] = factor(pam_fit_test$clustering)
    
    cols_list[[count]] =  nonzero_cols
    bestGammas[[count]] = best_gamma
    bestK_list_training[[count]] = best_K
  }
  

  return(c(labels_list, cols_list,bestGammas, test_maxK, test_maxSW, labels_list_test, bestK_list_training, maxSW_train))
}   
    






  
    