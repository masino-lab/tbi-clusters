SimMatrix <- function(label_vector){
  #empty similary matrix n x n 
  sim_matrix = matrix(data=NA, nrow=length(label_vector), ncol=length(label_vector))
  rownames(sim_matrix) = names(label_vector)
  colnames(sim_matrix) = names(label_vector)
  
  #populate similarity matrix 
  for(row in 1:length(label_vector)){
    sim_matrix[row,] = label_vector[row] == label_vector
  }
  return(sim_matrix)
}


CompSimMat <- function(simmatrix1, simmatrix2){
  
  lowertri1 = simmatrix1[lower.tri(simmatrix1, diag = FALSE)]
  lowertri2 = simmatrix2[lower.tri(simmatrix2, diag = FALSE)]
  
  return(sum(lowertri1 == lowertri2) / length(lowertri1))
}

