# Import dependencies 
library(ggsci, quietly = TRUE)

# Set Working directory
local_path = paste(path, "7_GCS", sep="")
setwd(local_path)

# Load data 
load(paste(path,'3_GLRM_Clustering/avg_clustering_results.RData', sep = ""))
load(paste(path,'3_GLRM_Clustering/avg_subset.RData', sep = ""))
load(paste(path,'3_GLRM_Clustering/tsne_object.RData', sep = ""))
load(paste(path, '6_Outcome_Profiles/GOSE.RData', sep = ""))



# Import baseline GCS 
gcs_pupils = read.csv("~/Documents/CHOP/FITBIR/COBRIT/COBRIT_Baseline /GCS_Pupil.csv", header = TRUE,  na.strings="")
colnames(gcs_pupils) = as.character(unlist(gcs_pupils[1,]))
gcs_pupils = gcs_pupils[-1,] 

#### Clean GCS Dataframe ####

cols = c("Main.GUID", "Study site and day.EvaluationDayNum", "BesT GCS and Pupils.GCSTotalScore", "WorsT GCS and Pupils.GCSTotalScore")
gcs = as.data.frame(gcs_pupils[,cols])

#Filter for Day 1 only
gcs_day1 = gcs[gcs$`Study site and day.EvaluationDayNum` == 1,]

#Omit NA values
gcs_day1 = na.omit(gcs_day1)

# Match with GUIDs in the cobrit_full set 
gcs_day1_full = gcs_day1[gcs_day1$Main.GUID %in% names(cluster_labels),]
gcs_day1_full = gcs_day1_full[which(!duplicated(gcs_day1_full$Main.GUID)),]

# assign row names
rownames(gcs_day1_full) = gcs_day1_full$Main.GUID

# remove Main.GUID  and study day columns now that they are the row names
gcs_day1_full = gcs_day1_full[,-(1:2)]

# Average Best and Worst GCS Scores
gcs_day1_full$`BesT GCS and Pupils.GCSTotalScore` = as.numeric(gcs_day1_full$`BesT GCS and Pupils.GCSTotalScore`)
gcs_day1_full$`WorsT GCS and Pupils.GCSTotalScore` = as.numeric(gcs_day1_full$`WorsT GCS and Pupils.GCSTotalScore`)
gcs_day1_full$`Avg GCS` = rowMeans(gcs_day1_full[,1:2])

gcs_day1_full$`Avg GCS` = cut(gcs_day1_full$`Avg GCS`,c(0,7,12,15))
gcs_day1_full$`Avg GCS` = mapvalues(gcs_day1_full$`Avg GCS`, from = c("(0,7]", "(7,12]", "(12,15]"), to = c('Severe', 'Moderate', 'Mild'))

gcs_baseline = as.data.frame(gcs_day1_full$`Avg GCS`, row.names = row.names(gcs_day1_full))
gcs_baseline['AvgGCS'] = gcs_baseline['gcs_day1_full$`Avg GCS`']


cl_gcs = cluster_labels[names(cluster_labels) %in% rownames(gcs_day1_full)]

chisq.test(table(cl_gcs, gcs_day1_full$`Avg GCS`))

save(gcs_baseline, cl_gcs, file = "GCSBaseline.RData")




#### Figure 4 ####

## Figure 4A

# need to remove the two observations without GCS scores 
tsne_obj$Y = as.data.frame(tsne_obj$Y)[-c(which(!names(cluster_labels) %in% rownames(gcs_day1_full))),]

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = gcs_day1_full$`Avg GCS`)

# Plot T-SNE
plot = ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = gcs_day1_full$`Avg GCS`)) + labs(color = "Cluster") +theme(plot.title = element_text(size = 9)) + 
  scale_color_jama(labels = c('Severe (GCS < 8)', 'Moderate (GCS 8-12)', 'Mild (GCS 13-15)'))+ theme_bw() + theme(legend.title = element_blank()) 

## Figure 4B 

gcs_baseline$AvgGCS = revalue(gcs_baseline$AvgGCS, replace = c('Mild'="Mild\n (13-15)", 'Moderate'="Moderate\n (GCS 12-8)", 'Severe'="Severe\n (GCS < 8)"))
y = unlist(cluster_90d_outcomes$GOSE[-which(!row.names(cluster_90d_outcomes) %in% row.names(gcs_baseline))])

my_comparisons = list(c("Severe\n (GCS < 8)", "Moderate\n (GCS 12-8)"), c("Severe\n (GCS < 8)","Mild\n (13-15)"), c("Moderate\n (GCS 12-8)","Mild\n (13-15)")) # Add pairwise comparisons p-value (Default = wilcox.test)
sargs = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
ggplot(as.data.frame(as.numeric(y)), aes_string(x = factor(unlist(gcs_baseline$AvgGCS)), y = as.numeric(y), fill=factor(unlist(gcs_baseline$AvgGCS)))) + geom_boxplot()  + stat_compare_means(method = "kruskal", label.y = 12.5) +
  stat_compare_means(aes(label=..p.adjus..), comparisons = my_comparisons, symnum.args = sargs, label = "p.signif") +
  theme(legend.position="none") + labs(x = '', y= "GOS-E Score at 90 Days") + scale_fill_manual(labels=c("Severe\n (GCS < 8)", "Moderate\n (GCS 12-8)","Mild\n (13-15)"), values = c("#374E55FF", "#DF8F44FF", "#00A1D5FF")) + theme_bw() + theme(legend.position="none")


## Figure 4C

y = unlist(cluster_180d_outcomes$GOSE[-which(!row.names(cluster_180d_outcomes) %in% row.names(gcs_baseline))])
ggplot(as.data.frame(as.numeric(y)), aes_string(x = factor(unlist(gcs_baseline$AvgGCS)), y = as.numeric(y), fill=factor(unlist(gcs_baseline$AvgGCS)))) + geom_boxplot()  + stat_compare_means(method = "kruskal", label.y = 12.5) +
  stat_compare_means(aes(label=..p.adjus..), comparisons = my_comparisons, symnum.args = sargs, label = "p.signif") +
  theme(legend.position="none") + labs(x = '', y= "GOS-E Score at 180 Days") + scale_fill_manual(labels=c("Severe\n (GCS < 8)", "Moderate\n (GCS 12-8)","Complicated Mild\n (13-15)"), values = c("#374E55FF", "#DF8F44FF", "#00A1D5FF")) + theme_bw() + theme(legend.position="none")



#### Table 4 ####

row.names(avg_x) = names(cluster_labels)
avg_x_1211 = avg_x[which(row.names(avg_x) %in% row.names(gcs_baseline)),]
for(feature in colnames(avg_x_1211)){
  print(feature)
  print(median(avg_x[gcs_baseline$AvgGCS == 'Mild', feature]))
  print(IQR(avg_x[cluster_labels == 'Mild', feature]))
}
# do for 'Moderate' and 'Severe' as well

# Multinomial logistic regression vs GCS
gcs = relevel(gcs_baseline$AvgGCS, ref = 'Mild')
gcs_test <- multinom(gcs ~ glucose + hematocrit + prothrombintime + ptinternationalnormalizedratio + plateletsplt1000microliterlabtestresltval + hemoglobinhbgdllabtestresltval, data = avg_x_1211)
summary(test)
z <- summary(gcs_test)$coefficients/summary(gcs_test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(gcs_test))
exp(confint(gcs_test, level = 0.95))
gcs = gcs_day1_full$`Avg GCS`

