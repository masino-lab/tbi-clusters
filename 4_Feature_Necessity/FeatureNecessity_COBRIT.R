# Import dependencies
library(ggplot2, quietly = TRUE)
library(plyr, quietly = TRUE)
library(ggsci, quietly = TRUE)
library(clusteval, quietly = TRUE)

source(paste(path, 'similarity_matrix_functions.R', sep=""))
source("permutation_knockout.R")


# Set working directory
local_path = paste(path, "4_Feature_Necessity", sep = "")
setwd(local_path)


# Load data 
load(paste(path,'3_GLRM_Clustering/avg_clustering_results.RData', sep = ""))
load(paste(path,'3_GLRM_Clustering/avg_subset.RData', sep = ""))
row.names(avg_x) = names(cluster_labels)



##### Necessity of Individual Features ##### 

# Permutation testing: shuffle each feature and measure how much the cluster membership changes
perm_KO_results = permutation_knockout(avg_x, cluster_labels, k=3)

save(perm_KO_results, file = 'perm_KO_results.RData')


#### Figure 2 ####

# Plotting function
PrettyColumnPlot <- function(g, title="", subtitle="", ylabel="", fillcolor, yintercept){
  g + geom_col(fill=fillcolor) + theme_classic() + labs(title=title, subtitle=subtitle,
                                                        y=ylabel, x = "Features") 
  + theme(axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), axis.text.x = element_text(size = 11, angle=90, vjust=0.5, 
                                                                                                                                                            hjust = 0.9)) + geom_hline(yintercept = yintercept, linetype="dotted")
}

## Figure 2A: Jaccards Coefficient 

# List of feature names
x = c('Glucose', 'Hematocrit', 'Prothrombin Time', 'INR', 'Platelets', 'Hemoglobin')

MedJaccards_NEC = perm_KO_results$MedJaccards_NEC

g <- ggplot(as.data.frame(MedJaccards_NEC), aes(x = x, y = MedJaccards_NEC))
title = "Feature Necessity"
subtitle = "Clustering Similarity when each feature is Independently Shuffled" 
ylabel = "Jaccard Similiarity Coffecient"
fillcolor = "#ca0020"
yintercept = 0.75

PrettyColumnPlot(g=g, ylabel=ylabel, fillcolor=fillcolor, yintercept = yintercept) + ylim(0,1)

## Figure 2B: Pairwise Similarity 

MedSim_NEC = perm_KO_results$MedSim_NEC * 100

g <- ggplot(as.data.frame(MedSim_NEC), aes(x = x, y = MedSim_NEC))
title = "Feature Necessity"
subtitle = "Clustering Similarity when each feature is Independently Shuffled" 
ylabel = "Pairwise Similiarity Index (%)"
fillcolor = "#0571b0"
yintercept = 90

PrettyColumnPlot(g=g, ylabel=ylabel, fillcolor=fillcolor, yintercept = yintercept) + ylim(0,100)
