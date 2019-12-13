# Import dependencies
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(plyr, quietly = TRUE)
library(ggsci, quietly = TRUE)
library(ggpubr, quietly = TRUE)
library(robustHD, quietly = TRUE)
library(mice, quietly = TRUE)

# Set Working directory
local_path = paste(path, "5_Feature_Profiles", sep="")
setwd(local_path)

# Source functions
source('boxplotfeats.R')
source(paste(path, 'helper_functions.R', sep = ""))

# Load data
load(paste(path,'3_GLRM_Clustering/avg_clustering_results.RData', sep = ""))
load(paste(path,'3_GLRM_Clustering/avg_subset.RData', sep = ""))
load(paste(path,'2_Data_Imputation/cobrit_imputeddfs.RData', sep = ""))
row.names(avg_x) = names(cluster_labels)


# Import COBRIT subject randomization data frame, contains treatment arm assignments
randomization =read.csv('~/Documents/CHOP/TBI Clustering Pipeline Code Repository/Data Files/COBRIT Injury Baseline/Randomization/Randomization.csv', header = TRUE, stringsAsFactors = FALSE)

treatment_groups = randomization %>%
  select(Randomization.Main.GUID, Randomization.Randomization.StudyTreatmentCat)

chisq.test(table(cluster_labels, treatment_groups$Randomization.Randomization.StudyTreatmentCat))


#### Table 2 ####

# Demographics 

# Average imputed datasets
data1 = mice::complete(imputed_cobrit_m5, 1)
data2 = mice::complete(imputed_cobrit_m5, 2)
data3 = mice::complete(imputed_cobrit_m5, 3)
data4 = mice::complete(imputed_cobrit_m5, 4)
data5 = mice::complete(imputed_cobrit_m5, 5)

# bind matrices
x_combined = do.call(cbind, list(data.matrix(data1),data.matrix(data2),data.matrix(data3),data.matrix(data4),data.matrix(data5)))
#recast the matrix into three dimensions
dim(x_combined) <- c(1213,dim(data1)[2],5) 


#now apply should work
avg_x_all = as.data.frame(apply(x_combined, c(1,2), mean))
colnames(avg_x_all) = colnames(data1)

cluster_labels = mapvalues(cluster_labels, from = c('1', '2', '3'), to = c('A', 'B', 'C'))
# A n = 511; B  n = 334; C n = 368

# Age
ageA = avg_x_all %>% select(Main.AgeYrs) %>% filter(cluster_labels == 'A')
ageB = avg_x_all %>% select(Main.AgeYrs) %>% filter(cluster_labels == 'B')
ageC = avg_x_all %>% select(Main.AgeYrs) %>% filter(cluster_labels == 'C')

median(ageA$Main.AgeYrs)
IQR(ageA$Main.AgeYrs)
median(ageB$Main.AgeYrs)
IQR(ageB$Main.AgeYrs)
median(ageC$Main.AgeYrs)
IQR(ageC$Main.AgeYrs)

kruskal.test(avg_x_all$Main.AgeYrs, cluster_labels, p.adjust = "holm")

# Gender
genderA = avg_x_all %>% select(Demographics.GenderTyp) %>% filter(cluster_labels == 'A')
genderB = avg_x_all %>% select(Demographics.GenderTyp) %>% filter(cluster_labels == 'B')
genderC = avg_x_all %>% select(Demographics.GenderTyp) %>% filter(cluster_labels == 'C')

table(genderA)
table(genderA) / dim(genderA)[1]
table(genderB)
table(genderB) / dim(genderB)[1]
table(genderC)
table(genderC) / dim(genderC)[1]
chisq.test(table(cbind(avg_x_all$Demographics.GenderTyp, cluster_labels)))


# Now combine imputed datasets to get the mode of values 
modeall = as.data.frame(apply(x_combined, c(1,2), Mode))
dim(modeall)
colnames(modeall) = colnames(data1)
row.names(modeall) = row.names(data1)

# Race 
labels = c('A', 'B', 'C')
for(label in labels){
  print(label)
  X = modeall  %>% filter(cluster_labels == label)
  print(table(X$Race.White))
  print(table(X$Race.White) / dim(X)[1])
  print(table(X$Race.Black))
  print(table(X$Race.Black) / dim(X)[1])
  print(table(X$`Race.Asian/Other`))
  print(table(X$`Race.Asian/Other`) / dim(X)[1])
  
}

race_tbl = as.table(cbind(c(347,61,12), c(369,60,17), c(282,54,11))) # values from above 
chisq.test(race_tbl)

# Ethnicity
A = modeall  %>% filter(cluster_labels == 'A')
B = modeall  %>% filter(cluster_labels == 'B')
C = modeall  %>% filter(cluster_labels == 'C')

table(A$Demographics.EthnCatCOBRIT)
table(B$Demographics.EthnCatCOBRIT)
table(C$Demographics.EthnCatCOBRIT)

eth_tbl = as.table(cbind(c(18,402), c(22,424), c(9,338)))
chisq.test(eth_tbl)

# Education 
labels = c(A, B, C)
for(label in labels){
  table(label['Educ.<=HS'])
  table(label['Educ.SomeCol/TradeSch'])
  table(label['Educ.>=ColGrad'])
}

educ_tbl = as.table(cbind(c(203,142,75), c(211,161,74), c(182,110,55)))
chisq.test(educ_tbl)





#### Table 3 ####
# Print median and IQR 
for(feature in colnames(avg_x)){
  print(feature)
  print(median(avg_x[cluster_labels == 'A', feature]))
  print(IQR(avg_x[cluster_labels == 'A', feature]))
}
# do for 'B' and 'C' as well

# Multinomial logistic regression
cluster_labels2 <- relevel(cluster_labels, ref = "C")
test <- multinom(cluster_labels2 ~ glucose + hematocrit + prothrombintime + ptinternationalnormalizedratio + plateletsplt1000microliterlabtestresltval + hemoglobinhbgdllabtestresltval, data = avg_x)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(test))
exp(confint(test, level = 0.95))

#### Table 4 ####
## TBI features by phenotype 

# Concat labels column to the 'mode' df 
allfeat_matrix_withlabels = cbind(modeall, cluster_labels)

# Identify the TBI specific columns 
tbi_cols = c(
  "InjuryInfo. 24:00-5:59",                                     
  "InjuryInfo. 6:00-11:59",                                     
  "InjuryInfo. 12:00-17:59",                                    
  "InjuryInfo. 18:00-23:59",                                   
  "InjMech.Other",                                              
  "InjMech.Fall", 
  "InjMech.Motor",
  "Baseline.CT.CTMesencephalicCistTyp",                         
  "Baseline.CT.CTSAHTyp",                                       
  "Baseline.CT.IVHStatus",                                     
  "Baseline.CT.HydrcephPresInd",                                
  "Baseline.CT.CTLesnHighMixedDensityInd",                      
  "CTIntraparenLesnAnatSite.No intraparenchymal areas involved",
  "CTIntraparenLesnAnatSite.Right frontal",                     
  "CTIntraparenLesnAnatSite.Left frontal",                     
  "CTIntraparenLesnAnatSite.Right temporal",                    
  "CTIntraparenLesnAnatSite.Left temporal",                     
  "CTIntraparenLesnAnatSite.Right parietal",                   
  "CTIntraparenLesnAnatSite.Left parietal",                     
  "CTIntraparenLesnAnatSite.Right occipital",  
  "CTIntraparenLesnAnatSite.Left occipital",                    
  "CTIntraparenLesnAnatSite.Brainstem/diencephalon/CC",         
  "CTIntraparenLesnAnatSite.Cerebellar",                        
  "CTSbdrlLesnAnatSite.No subdural areas involved",             
  "CTSbdrlLesnAnatSite.Right supratentorial",                   
  "CTSbdrlLesnAnatSite.Left supratentorial",                    
  "CTSbdrlLesnAnatSite.Falcine",                              
  "CTSbdrlLesnAnatSite.Interhemispheric",                      
  "CTSbdrlLesnAnatSite.Tentorial",                             
  "CTSbdrlLesnAnatSite.Posterior fossa",                        
  "CTEpdurlLesnAnatSite.No subdural areas involved",            
  "CTEpdurlLesnAnatSite.Right supratentorial",                  
  "CTEpdurlLesnAnatSite.Left supratentorial",                   
  "CTEpdurlLesnAnatSite.Posterior fossa", 
  "MidlineShift.<=5mm",                                        
  "MidlineShift.>10mm",                                         
  "MidlineShift.>5<10mm",                                       
  "MidlineShift.None",
  "Seizures", 
  "Previous TBI",
  "Treated psychiatric problem"
)

for(col in tbi_cols){
  t = table(allfeat_matrix_withlabels[,col], allfeat_matrix_withlabels$cluster_labels)
  print(col)
  print(t)
  print(chisq.test(t))
}



