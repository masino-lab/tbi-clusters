# Import dependencies
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(plyr, quietly = TRUE)
library(ggsci, quietly = TRUE)
library(ggpubr, quietly = TRUE)
library(robustHD, quietly = TRUE)
library(crossmatch, quietly = TRUE)

# set working directory
local_path = paste(path, "9_TRACK_Cluster_Profiles", sep="")
setwd(local_path)

# Load data
load(paste(path,'3_GLRM_Clustering/avg_clustering_results.RData', sep = ""))
load(paste(path,'3_GLRM_Clustering/avg_subset.RData', sep = ""))
load(paste(path,'1_Data_Cleaning/track_tbi_predictors.RData', sep = ""))
load(paste(path, '8_Supervised_Learning/track_clusters.RData', sep = ""))
row.names(avg_x) = names(cluster_labels)

# import TRACKTBI GOS-E outcome data
track_gose = read.csv(paste(path, '0_Data_Files/TRACKTBI Outcomes/GOSE/query_result_GOSE_Standard_2018-05-05T11-29-05.csv', sep = ""))



#### Figure 5 ####

## Figure 5A 

# rearrange track_tbi_predictor columns to match cobrit columns by name 
track_df = track_tbi_predictors[,c('glucose', 'hematocrit', 'prothrombintime', 'ptinternationalnormalizedratio', 'plateletsplt1000microliterlabtestresltval', 'hemoglobinhbgdllabtestresltval')]
combo_df = rbind(avg_x, track_df)

combo_diss = as.matrix(daisy(combo_df, metric = 'gower'))
tr_clusters = as.character(mapvalues(as.factor(track_clusters$track_tbi_predicted_clusters), from = c(1,2,3), to = c('Tr1', 'Tr2', 'Tr3')))
cb_clusters = as.character(mapvalues(as.factor(cluster_labels), from = c(1,2,3), to = c('Cb1', 'Cb2', 'Cb3')))
combo_clusters = as.factor(c(cb_clusters, tr_clusters))

tsne_obj <- Rtsne(combo_diss, is_distance = TRUE, perplexity = 50)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = combo_clusters)

# Plot T-SNE
plot2 = ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) + labs(color = "Cluster") +theme(plot.title = element_text(size = 9)) + 
  scale_color_manual(labels = c('COBRIT Phen. A', 'COBRIT Phen. B', 'COBRIT Phen. C', 'TRACK-TBI Phen. A', 'TRACK-TBI Phen. B', 'TRACK-TBI Phen. C'), values=c("#008EA0FF", "#C71000FF", "#8A4198FF", "#FF95A8FF", "#84D7E1FF", "#FF6F00FF"))+ theme_bw() + theme(legend.title = element_blank()) 

save(plot2, file = 'overlap_tsne.RData')


# Cross-match test statistics 
combo_clusters = as.character(combo_clusters)
A_ind = which(combo_clusters %in% c('Cb1', 'Tr1'))
A_labels = as.numeric(mapvalues(combo_clusters[A_ind], from = c('Cb1', 'Tr1'), to = c(0,1)))
A_diss = as.matrix(daisy(combo_df[A_ind,]))
crossmatchtest(A_labels, A_diss)

B_ind =  which(combo_clusters %in% c('Cb2', 'Tr2'))
B_labels = as.numeric(mapvalues(combo_clusters[B_ind], from = c('Cb2', 'Tr2'), to = c(0,1)))
B_diss = as.matrix(daisy(combo_df[B_ind,]))
crossmatchtest(B_labels, B_diss)

C_ind =  which(combo_clusters %in% c('Cb3', 'Tr3'))
C_labels = as.numeric(mapvalues(combo_clusters[C_ind], from = c('Cb3', 'Tr3'), to = c(0,1)))
C_diss = as.matrix(daisy(combo_df[C_ind,]))
crossmatchtest(C_labels, C_diss)

## Figure 5B/C 

# Clean TRACK-TBI GOS-E data 

# subset columns of interest 
cols = c("GOSE_Standard.Main.GUID", "GOSE_Standard.Main.DaysSinceBaseline", "GOSE_Standard.Form.Completion.GlasgowOutcomeScalExtScore")
track_gose = track_gose[, cols]

# subset 90-D and 180-D GOS-E Scores
gose90 = track_gose %>% filter(GOSE_Standard.Main.DaysSinceBaseline == 90)
row.names(gose90) = gose90$GOSE_Standard.Main.GUID
gose90$GOSE_Standard.Main.DaysSinceBaseline = NULL
gose180 = track_gose %>% filter(GOSE_Standard.Main.DaysSinceBaseline == 180)
row.names(gose180) = gose180$GOSE_Standard.Main.GUID
gose180$GOSE_Standard.Main.DaysSinceBaseline = NULL

#### Merge cluster labels with gose scores

gose90$GOSE_Standard.Main.GUID = as.character(gose90$GOSE_Standard.Main.GUID)
track_clusters['GOSE_Standard.Main.GUID'] = row.names(track_clusters)

gose180$GOSE_Standard.Main.GUID = as.character(gose180$GOSE_Standard.Main.GUID)

jointbl90 = track_clusters %>% left_join(gose90, by = 'GOSE_Standard.Main.GUID')
jointbl180 = track_clusters %>% left_join(gose180, by = 'GOSE_Standard.Main.GUID')




dim(jointbl90 %>% filter(is.na(GOSE_Standard.Form.Completion.GlasgowOutcomeScalExtScore))) #85 missing GOS-E 90-D scores
dim(jointbl180 %>% filter(is.na(GOSE_Standard.Form.Completion.GlasgowOutcomeScalExtScore))) #109 missing GOS-E 180-D scores

jointbl90 = na.omit(jointbl90)
jointbl180 = na.omit(jointbl180)


# Figure 5B
boxplotfeats(jointbl90, "GOSE_Standard.Form.Completion.GlasgowOutcomeScalExtScore", jointbl90$track_tbi_predicted_clusters)+ scale_fill_manual(values= c("#FF95A8FF", "#84D7E1FF", "#FF6F00FF")) + labs(x = "TRACK-TBI Pilot Patient Phenotype", y= "GOS-E Score at 90 Days") 
# Figure 5C
boxplotfeats(jointbl180, "GOSE_Standard.Form.Completion.GlasgowOutcomeScalExtScore", jointbl180$track_tbi_predicted_clusters) + scale_fill_manual(values= c("#FF95A8FF", "#84D7E1FF", "#FF6F00FF")) + labs(x = "TRACK-TBI Pilot Patient Phenotype", y= "GOS-E Score at 180 Days") 



#### Phenotype 90-D GOSE Effect Size ####
jointbl90['GOSE'] = jointbl90$GOSE_Standard.Form.Completion.GlasgowOutcomeScalExtScore

phen1= jointbl90 %>% filter(track_tbi_predicted_clusters == 1) %>% na.omit()
phen2 = jointbl90 %>% filter(track_tbi_predicted_clusters == 2) %>% na.omit()
phen3 = jointbl90 %>% filter(track_tbi_predicted_clusters == 3) %>% na.omit()

cohen.d(phen1$GOSE, phen2$GOSE)
cohen.d(phen1$GOSE, phen3$GOSE)
cohen.d(phen2$GOSE, phen3$GOSE)

pnorm(0, mean(phen1$GOSE) - mean(phen3$GOSE), sqrt(sd(phen1$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)
pnorm(0, mean(phen2$GOSE) - mean(phen3$GOSE), sqrt(sd(phen2$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)


#### Phenotype 180-D GOSE Effect Size ####
jointbl180['GOSE'] = jointbl180$GOSE_Standard.Form.Completion.GlasgowOutcomeScalExtScore

phen1= jointbl180 %>% filter(track_tbi_predicted_clusters == 1) %>% na.omit()
phen2 = jointbl180 %>% filter(track_tbi_predicted_clusters == 2) %>% na.omit()
phen3 = jointbl180 %>% filter(track_tbi_predicted_clusters == 3) %>% na.omit()

cohen.d(phen1$GOSE, phen2$GOSE)
cohen.d(phen1$GOSE, phen3$GOSE)
cohen.d(phen2$GOSE, phen3$GOSE)

pnorm(0, mean(phen1$GOSE) - mean(phen3$GOSE), sqrt(sd(phen1$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)
pnorm(0, mean(phen2$GOSE) - mean(phen3$GOSE), sqrt(sd(phen2$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)

