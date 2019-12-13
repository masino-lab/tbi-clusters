# Boxplot function

boxplotfeats = function(data, feature, cluster_labels){
  
  my_comparisons = list(c(1,2), c(1,3), c(2,3)) # Add pairwise comparisons p-value (Default = wilcox.test)
  #my_comparisons = list(c(1,2), c(1,3), c(2,3), c(1,4), c(2,4), c(3,4))
  sargs = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
  
  y = unlist(data[feature])
  plot = ggplot(as.data.frame(as.numeric(y)), aes_string(x = factor(cluster_labels), y = as.numeric(y), fill=factor(cluster_labels))) + geom_boxplot()  + stat_compare_means(method = "kruskal", label.y = 12) + 
    stat_compare_means(aes(label=..p.adjus..), comparisons = my_comparisons, symnum.args = sargs) +
    theme(legend.position="none")  + labs(x = "Patient Subtype", y= feature) + theme_bw() + theme(legend.position="none") + scale_fill_futurama()
  return(plot)
}
