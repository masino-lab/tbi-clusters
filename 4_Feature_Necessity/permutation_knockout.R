permutation_knockout = function(X, L1, k=3){
  #Shuffle each feature q times and calculate Jaccard coefficient and Similarity Matrix Index for each new clustering scheme. Gives q disIf Jaccard or Similarity Matrix Index are close to 1, that feature is not necessary for clustering. 
  
  names(L1) = rownames(X)
  
  d = dim(X)[2]
  q = 500
  feature_j_distributionsN = vector('list', length= d)
  clustsims_distributionsN =  vector('list', length= d)
  
  for(d_ in 1:d){
    print(d_)
    j = vector('list', length = q)
    sims = vector('list', length = q)
  
    
    for(q_ in 1:q){
      Xprime = X
      Xprime[,d_] = Xprime[sample(1:nrow(Xprime), replace=TRUE),d_]
      
      gower_dist_Xprime = as.matrix(daisy(Xprime, metric = "gower", warnType = FALSE))
      pam_fit_Xprime = pam(gower_dist_Xprime, diss = TRUE, k = k)
      L2 = factor(pam_fit_Xprime$clustering)
      
      j[q_] = clusteval::cluster_similarity(L1, L2, similarity = 'jaccard')
      sims[q_] = CompSimMat(SimMatrix(L1), SimMatrix(L2))
      
    }
  
    feature_j_distributionsN[[d_]] = as.numeric(j)
    clustsims_distributionsN[[d_]] = as.numeric(sims)
    
  }
  
  
  JmedsN = lapply(feature_j_distributionsN, median)
  SimmedsN = lapply(clustsims_distributionsN, median)
  MedJaccards_NEC = round(as.numeric(JmedsN), 2)
  MedSim_NEC = as.numeric(SimmedsN)
  
  return(list(JmedsN = JmedsN, SimmedsN = SimmedsN, MedJaccards_NEC = MedJaccards_NEC, MedSim_NEC = MedSim_NEC))
}





