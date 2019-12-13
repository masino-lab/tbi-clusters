# Import dependencies
library(mice, quietly = TRUE)

# Set working directory
local_path = paste(path, "2_Data_Imputation", sep = "")
setwd(local_path)

# Import  df
load(paste(path, "1_Data_Cleaning/cobrit_cleaned.RData", sep=""))

# Count missing values in each column 
colSums(is.na(cobrit_merged))

#### Indicator Variables for MNAR Data ####

# Variables with >30% of values are missing (363.9)

missing30 = colSums(is.na(cobrit_merged)) > 363.9
grtr30_missing_cols = colnames(cobrit_merged[, missing30])

# Include indicator columns for columns with missing data 

# 1.	Differential test (ANC/ALC)
Differential_test_cols = cobrit_merged[, colnames(cobrit_merged) %in% c('X..Neutrophil.....LabTestResltVal', 'Absolute.neutrophil.count..ANC....1000.microliter.LabTestResltVal',
                           'X..Lymphocytes.....LabTestResltVal', 'Absolute.lymphocyte.count..ALC....1000.microliter.LabTestResltVal')]


cobrit_merged[rowSums(is.na(Differential_test_cols)) == 4, 'Differential.blood.test.completed'] = 0
cobrit_merged[rowSums(is.na(Differential_test_cols)) < 4, 'Differential.blood.test.completed'] = 1

# 2.	Hepatic blood panel (AST/ALT)
Hep_test_cols = cobrit_merged[, colnames(cobrit_merged) %in% c('AST..SGOT....IU.L.LabTestResltVal', 'ALT..SGPT....IU.L.LabTestResltVal')]

cobrit_merged[rowSums(is.na(Hep_test_cols)) == 2, 'Hepatic.blood.test.completed'] = 0
cobrit_merged[rowSums(is.na(Hep_test_cols)) < 2, 'Hepatic.blood.test.completed'] = 1

# 3.	Arterial blood test 

arterial_test_cols = cobrit_merged[, colnames(cobrit_merged) %in% c("PaO2Measr", "PulmBloodGasPPCO2", "PulmGasExFIO2Val" ,"Arterial.pH")]

cobrit_merged[rowSums(is.na(arterial_test_cols)) == 4, 'Arterial.blood.test.completed'] = 0
cobrit_merged[rowSums(is.na(arterial_test_cols)) < 4, 'Arterial.blood.test.completed'] = 1

# 4.	Toxicology Screen (already an indicator variable fore this)

# Replace NA's with zeros (NA's in this case signify the toxicology screen was not completed)
cobrit_merged$Toxicology.ToxicologyScreenCompltdLoc[is.na(cobrit_merged$Toxicology.ToxicologyScreenCompltdLoc)] = 0

# 5. Alcohol Screen (already an indicator variable fore this)

# Replace NA's with zeros (NA's in this case signify the toxicology screen was not completed)
cobrit_merged$Alcohol.ToxicologyScreenCompltdLoc[is.na(cobrit_merged$Alcohol.ToxicologyScreenCompltdLoc)] = 0

# remove the variables which now have indicator variables 
rm_vars = c('X..Neutrophil.....LabTestResltVal', 'Absolute.neutrophil.count..ANC....1000.microliter.LabTestResltVal',
            'X..Lymphocytes.....LabTestResltVal', 'Absolute.lymphocyte.count..ALC....1000.microliter.LabTestResltVal',
            'AST..SGOT....IU.L.LabTestResltVal', 'ALT..SGPT....IU.L.LabTestResltVal',
            "PaO2Measr", "PulmBloodGasPPCO2", "PulmGasExFIO2Val" ,"Arterial.pH", "Alcohol.AlchBldLvlMeasr")

cobrit_merged = cobrit_merged[,!colnames(cobrit_merged) %in% rm_vars]

#### Exclude Variables with >30% Missing Data ####

# remove variables with >30% data (MNAR)
cobrit_merged_2 = cobrit_merged[, !colnames(cobrit_merged) %in% grtr30_missing_cols]

# variables with 10-30% missing data

grtr10less30_cols = colnames(cobrit_merged_2)[(colSums(is.na(cobrit_merged_2)) > .10*dim(cobrit_merged_2)[1] & colSums(is.na(cobrit_merged_2)) < .30*dim(cobrit_merged_2)[1])]



# convert binary variables to factor datatype 

cols = colnames(cobrit_merged_2)[sapply(sapply(cobrit_merged_2, unique), length) <= 3]
cobrit_merged_2[,cols] = data.frame(apply(cobrit_merged_2[,cols], 2, as.factor))

# remove columns of all zeros
cobrit_merged_2 = cobrit_merged_2[, !colnames(cobrit_merged_2) %in% names(cobrit_merged_2[, sapply(cobrit_merged_2, function(v) var(v, na.rm=TRUE)==0)])]

# Indicator columns excluded from clustering 
indicator_columns = c(
"Differential.blood.test.completed",                               
"Hepatic.blood.test.completed",                                    
"Arterial.blood.test.completed",
"Baseline.Assessments.TransfusionInd",                            
"Transfusion...Fresh.frozen.plasma..FFP..TransfusionInd",          
"Transfusion...Platelet..PLT..TransfusionInd",                     
"Transfusion...Packed.Red.Blood.Cells..PRBC..TransfusionInd",     
"Transfusion...Whole.Blood.TransfusionInd",                        
"Transfusion...Cryoprecipitate..Cryo..TransfusionInd",             
"Transfusion...Factor.7.TransfusionInd",                           
"Ins.Outs.TotalVolHypertonicSalineVal",
"Alcohol.ToxicologyScreenCompltdLoc",                              
"Toxicology.ToxicologyScreenCompltdLoc", 
"Net Fluid Intake",  
"Creatinine...mg.dL.LabTestResltVal",                              
"Glucose.level...mg.dL.LabTestResltVal", 
"Potassium..K....mMol.L.LabTestResltVal",                          
"Sodium..Na....mMol.L.LabTestResltVal",
"White.blood.cell.count..WBC......1000.microliter.LabTestResltVal",
"Hematocrit..HCT......LabTestResltVal",
"Baseline.CT.CTSbdrlLesnVolMeasr",                                 
"Baseline.CT.CTEpdurlLesnVolMeasr",
"Baseline.CT.CTMidlineShiftMeasr"
) # the last two are not indicators but they are excluded as they are already represented by an indicator variable


cobrit_merged_2 = cobrit_merged_2[,!colnames(cobrit_merged_2) %in% c(indicator_columns, grtr10less30_cols)]

#### Remaining Missing Data Imputation ####

# remaining variables contain <10% data which can be imputed using multiple imputation (mice package in R)
imputed_cobrit_m5 =  mice(cobrit_merged_2, m=5, maxit = 50, method = 'rf', seed = 500)

# save imputed dfs 
save(imputed_cobrit_m5, file = 'cobrit_imputeddfs.RData')



