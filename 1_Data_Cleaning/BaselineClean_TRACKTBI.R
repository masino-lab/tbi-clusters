# Import dependencies
library(dplyr, quietly = TRUE)
library(plyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(reshape, quietly = TRUE)

# Set Working directory
local_path = paste(path, "1_Data_Cleaning", sep="")
setwd(local_path)

# Load data 
track = read.csv(paste(path, '0_Data_Files/TRACKTBI Injury Baseline/InjuryInfo_BaselineLabs/query_result_TRACKTBI_InjuryHx_2018-05-05T11-28-47.csv', sep = ""), na.strings=c("","NA"))

columns = c(
  "TRACKTBI_InjuryHx.Main.GUID", 
  "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestName",
  "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestOTH",
  "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestPerfInd",
  "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltVal",
  "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltUoM" 
)

df = track[,columns]
df = df %>% tidyr::fill(TRACKTBI_InjuryHx.Main.GUID)


platelets = df[(df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestName == 'Other, specify' & df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestOTH == 'Platelet Count (PLT)'), c("TRACKTBI_InjuryHx.Main.GUID", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltVal", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltUoM")] 
colnames(platelets) = c('Main.GUID', 'Platelet Count (PLT)', 'PLT UoM')
inr = df[(df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestName == 'Other, specify' & df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestOTH == 'INR'), c("TRACKTBI_InjuryHx.Main.GUID", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltVal", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltUoM")]
colnames(inr) = c('Main.GUID', 'INR', 'INR UoM')
hematocrit =  df[df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestName == 'Hematocrit (HCT)', c("TRACKTBI_InjuryHx.Main.GUID", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltVal", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltUoM")]
colnames(hematocrit) = c('Main.GUID', 'Hematocrit (HCT)', 'HCT UoM')
hemoglobin =  df[df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestName == 'Hemoglobin (HB)', c("TRACKTBI_InjuryHx.Main.GUID", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltVal", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltUoM")]
colnames(hemoglobin) = c('Main.GUID', 'Hemoglobin (HB)', 'HB UoM')
pt = df[df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestName == 'Prothrombin Time (PT)', c("TRACKTBI_InjuryHx.Main.GUID", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltVal", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltUoM")]
colnames(pt) = c('Main.GUID', 'Prothrombin Time (PT)', 'PT UoM')
glucose = df[df$TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestName == 'Glucose', c("TRACKTBI_InjuryHx.Main.GUID", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltVal", "TRACKTBI_InjuryHx.Daily.Labs.Bloodwork.LabTestResltUoM")]
colnames(glucose) = c('Main.GUID', 'Glucose', 'Glucose UoM')


df_new = hematocrit %>%
          left_join(pt, by="Main.GUID") %>% 
          left_join(inr, by="Main.GUID") %>% 
          left_join(platelets, by="Main.GUID")  %>% 
          left_join(hemoglobin, by="Main.GUID") %>%
          left_join(glucose, by="Main.GUID")

# check that UoM columns all have the same units 
length(unique(df_new$`PLT UoM`)) == 1
length(unique(df_new$`INR UoM`)) == 1
length(unique(df_new$`HCT UoM`)) == 1
length(unique(df_new$`HB UoM`)) == 1
length(unique(df_new$`PT UoM`)) == 1

# remove UoM columns 
df_new = df_new[,-grep('UoM', colnames(df_new))]
row.names(df_new) = df_new$Main.GUID
df_new$Main.GUID = NULL

# remove observations with NA's 
df_clean = df_new[complete.cases(df_new),]
sum(is.na(df_clean))

track_tbi_predictors = df_clean

colnames(track_tbi_predictors) = c("hematocrit", "prothrombintime", "ptinternationalnormalizedratio",
                                    "plateletsplt1000microliterlabtestresltval", "hemoglobinhbgdllabtestresltval", "glucose")


track_tbi_predictors = data.frame(apply(track_tbi_predictors, 2, as.numeric), row.names = rownames(df_clean))

save(track_tbi_predictors, file = 'track_tbi_predictors.RData')
