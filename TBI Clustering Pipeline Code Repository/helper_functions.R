cluster_final = function(gower_matrix, krange = c(3:10)){
  sil_width = c()
  for(k in krange){
    pam_fit = pam(gower_matrix, diss = TRUE, k = k)
    sil_width = c(sil_width, pam_fit$silinfo$avg.width)
  }
  pam_object<- pam(gower_matrix, diss = TRUE, k = krange[which.max(sil_width)])
  print(krange[which.max(sil_width)])
  return(pam_object)
}

plot_tsne = function(gower_matrix, pam_object, perplexity = 50){
  # Generate T-SNE object
  tsne_obj <- Rtsne(gower_matrix, is_distance = TRUE, perplexity = perplexity)
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_object$clustering))
  
  # Plot T-SNE
  plot = ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster)) + labs(color = "Cluster") +theme(plot.title = element_text(size = 9))
  return(plot)
}

Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
