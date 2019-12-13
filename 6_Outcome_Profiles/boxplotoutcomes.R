boxplotoutcomes = function(data, feature, cluster_labels, group_label, title, colors =c()){
  
  my_comparisons = list(c("A","B"), c("A","C"), c("B","C")) # Add pairwise comparisons p-value (Default = wilcox.test)
  sargs = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
  
  y = unlist(data[feature])
  plot = ggplot(as.data.frame(as.numeric(y)), aes_string(x = factor(cluster_labels), y = as.numeric(y), fill=factor(cluster_labels))) + geom_boxplot()  + ylim(0,12) +  stat_compare_means(method = "kruskal", label.y = 12) + 
    stat_compare_means(aes(label=..p.adjus..), comparisons = my_comparisons, symnum.args = sargs, label = "p.signif") +
    theme(legend.position="none")  + labs(x = group_label, y= title) + theme_bw() + theme(legend.position="none") + scale_fill_manual(values=colors)
  return(plot)
}


