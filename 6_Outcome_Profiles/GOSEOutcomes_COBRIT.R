# Import dependencies
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(plyr, quietly = TRUE)
library(ggsci, quietly = TRUE)
library(ggpubr, quietly = TRUE)
library(robustHD, quietly = TRUE)
library(effsize, quietly = TRUE)

# Set Working directory
local_path = paste(path, "6_Outcome_Profiles", sep="")
setwd(local_path)

# Source functions
source('boxplotoutcomes.R')

# Load data
load(paste(path,'3_GLRM_Clustering/avg_clustering_results.RData', sep = ""))
load(paste(path,'3_GLRM_Clustering/avg_subset.RData', sep = ""))
load(paste(path,'2_Data_Imputation/cobrit_imputeddfs.RData', sep = ""))
row.names(avg_x) = names(cluster_labels)


# Import outcomes data
gos_e = read.csv(paste(path, '0_Data_Files/COBRIT Outcomes/GOS-E/GOSE_Standard.csv', sep = ""), header=TRUE)
deaths = read.csv(paste(path, '0_Data_Files/COBRIT Outcomes/DeathReport_TBI_240/DeathReport_TBI_Validated.csv', sep = ""), header=TRUE, stringsAsFactors = FALSE)



#### Data Cleaning for Glasgow Outcome Scale Extended (GOS-E) ####

# Columns names were in the first row of dataframe -- correct for this
colnames(gos_e) = as.character(unlist(gos_e[1, ]))
gos_e = gos_e[-1,]

# Select relevant columns
keep_cols = c("Main.GUID", "Main.GeneralNotesTxt", "Form Completion.GlasgowOutcomeScalExtScore")
gos_e_rev = gos_e[,keep_cols]

# Filter for scores taken at 90 day follow-up
gos_e_rev_90 = gos_e_rev[grep("90", gos_e_rev$Main.GeneralNotesTxt),]
# Filter for scores taken at 180 day follow-up
gos_e_rev_180 = gos_e_rev[grep("180", gos_e_rev$Main.GeneralNotesTxt),]


# Remove Evaluation day column (column 2)
gos_e_rev_90 = gos_e_rev_90[,-2]
gos_e_rev_180 = gos_e_rev_180[,-2]

## Death Report
colnames(deaths) = deaths[1,]
death_ids = deaths[2:74,'Main.GUID']

# Combine GOS-E 90-day scores with deaths (GOS-E score of 1)
g1 = as.data.frame(as.factor(gos_e_rev_90$`Form Completion.GlasgowOutcomeScalExtScore`), row.names = as.character(gos_e_rev_90$Main.GUID))
g2 = droplevels(g1)
colnames(g2) = "GOSE"
subjects = names(cluster_labels)
GOSE =  rep(NA, length(subjects))
names(GOSE) = subjects

for(nam in names(GOSE)){
  if(nam %in% rownames(g2)){
    GOSE[nam] = g2$GOSE[rownames(g2) == nam]
  }
}

GOSE = mapvalues(GOSE, 
                 from=c(1,2,3,4,5,6,7), 
                 to=c(2,3,4,5,6,7,8))

deaths = death_ids[death_ids %in% subjects & !death_ids %in% gos_e_rev_90$Main.GUID]

for(name in names(GOSE)){
  if(name %in% deaths){
    GOSE[name] = 1
  }
}
GOSE= as.data.frame(GOSE)


cluster_90d_outcomes = as.data.frame(cbind(cluster_labels, GOSE))



# Combine GOS-E 180-day scores with deaths (GOS-E score of 1)
g1 = as.data.frame(as.factor(gos_e_rev_180$`Form Completion.GlasgowOutcomeScalExtScore`), row.names = as.character(gos_e_rev_180$Main.GUID))
g2 = droplevels(g1)
colnames(g2) = "GOSE"
subjects = names(cluster_labels)
GOSE =  rep(NA, length(subjects))
names(GOSE) = subjects

for(nam in names(GOSE)){
  if(nam %in% rownames(g2)){
    GOSE[nam] = g2$GOSE[rownames(g2) == nam]
  }
}

GOSE = mapvalues(GOSE, 
                 from=c(1,2,3,4,5,6,7), 
                 to=c(2,3,4,5,6,7,8))

deaths = death_ids[death_ids %in% subjects & !death_ids %in% gos_e_rev_180$Main.GUID]

for(name in names(GOSE)){
  if(name %in% deaths){
    GOSE[name] = 1
  }
}
GOSE= as.data.frame(GOSE)

cluster_180d_outcomes = as.data.frame(cbind(cluster_labels, GOSE))

save(cluster_90d_outcomes, cluster_180d_outcomes, file = "GOSE.RData")



#### Phenotype 90-D GOSE Effect Size ####

phen1= cluster_90d_outcomes %>% filter(cluster_labels == 1) %>% na.omit()
phen2 = cluster_90d_outcomes %>% filter(cluster_labels == 2) %>% na.omit()
phen3 = cluster_90d_outcomes %>% filter(cluster_labels == 3) %>% na.omit()

cohen.d(phen1$GOSE, phen2$GOSE)
cohen.d(phen1$GOSE, phen3$GOSE)
cohen.d(phen2$GOSE, phen3$GOSE)

pnorm(0, mean(phen1$GOSE) - mean(phen3$GOSE), sqrt(sd(phen1$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)
pnorm(0, mean(phen2$GOSE) - mean(phen3$GOSE), sqrt(sd(phen2$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)



#### Phenotype 180-D GOSE Effect Size ####

phen1= cluster_180d_outcomes %>% filter(cluster_labels == 1) %>% na.omit()
phen2 = cluster_180d_outcomes %>% filter(cluster_labels == 2)%>% na.omit()
phen3 = cluster_180d_outcomes %>% filter(cluster_labels == 3)%>% na.omit()

cohen.d(phen1$GOSE, phen2$GOSE)
cohen.d(phen1$GOSE, phen3$GOSE)
cohen.d(phen2$GOSE, phen3$GOSE)

pnorm(0, mean(phen1$GOSE) - mean(phen3$GOSE), sqrt(sd(phen1$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)
pnorm(0, mean(phen2$GOSE) - mean(phen3$GOSE), sqrt(sd(phen2$GOSE)^2 + sd(phen3$GOSE)^2), lower.tail = FALSE)






#### Figure 3B #### 
cluster_labels = mapvalues(cluster_labels, from = c(1,2,3), to = c("A", "B", "C"))
boxplotoutcomes(cluster_90d_outcomes, 'GOSE', cluster_labels, 'Patient Phenotype', 'GOSE-E Score at 90 Days', c("#008EA0FF", "#C71000FF", "#8A4198FF"))
kruskal.test(cluster_90d_outcomes$GOSE, cluster_90d_outcomes$cluster_labels, p.adj = 'holm')

#### Figure 3C #### 
boxplotoutcomes(cluster_180d_outcomes, 'GOSE', cluster_labels, 'Patient Phenotype', 'GOSE-E Score at 180 Days', c("#008EA0FF", "#C71000FF", "#8A4198FF"))
kruskal.test(cluster_180d_outcomes$GOSE, cluster_180d_outcomes$cluster_labels, p.adj = 'holm')
