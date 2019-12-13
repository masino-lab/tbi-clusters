# Import dependences
library(plyr, quietly = TRUE)
library(dummies, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(reshape, quietly = TRUE)

# Set working directory
local_path = paste(path, "0_Data_Files/COBRIT Injury Baseline", sep = "")
setwd(local_path)


# Import csvs
base_labs = read.csv(paste(local_path,'/BaselineLabs_240/BaselineLabs.csv', sep = ""), row.names = 'Main.GUID')
ct = read.csv(paste(local_path,'/Baseline_CT_240/Baseline_CT_Validated.csv', sep = ""), row.names = 'Main.GUID', na.strings=c(" ","","NA"))
demographics = read.csv(paste(local_path, '/DemogrphsCOBRIT_240/DemogrphsCOBRIT_2-12-15.csv', sep = ""), row.names = 'Main.GUID')
injury_info = read.csv(paste(local_path, '/InjuryInfo_240/InjuryInfo.csv', sep = ""), row.names = 'Main.GUID')
safety_labs = read.csv(paste(local_path,'/SafetyLabTest_240/SafetyLabTest.csv', sep = ""))
medhx = read.csv(paste(local_path,'/MedHxTBICOBRIT_240/MedHxTBICOBRIT_2-20-15.csv', sep = ""), na.strings=c("","NA"))


#### Baseline Labs ####

# Drop columns with all NA's
base_labs = base_labs[, colSums(is.na(base_labs)) != nrow(base_labs)]

# Drop metadata columns
base_labs_metadata = c(
  "record",                                                        
  "SiteCode.SiteCode",                                              
  "Baseline.Assessments.StudyDay_AssmntCompltNum",                  
  "Baseline.Assessments.NeuroWorsenDefInSheetInd",
  "Highest.measurements.BPMeasureMethod",
  "Lowest.measurements.BPMeasureMethod", 
  "Lowest.measurements.VentilatorSettingRate",
  "Highest.measurements.VentilatorSettingRate",
  "Alcohol.HrsBetInjBACCt" 
)
base_labs = base_labs[, !colnames(base_labs) %in% base_labs_metadata]

# Select highest values
highest_labs = base_labs[,grep('Highest', colnames(base_labs))]
lowest_labs = base_labs[,grep('Lowest', colnames(base_labs))]


lab_names = c(
  'Temp',
  'BldPressrSyst',
  'BldPressrDiastl',
  'HeartRate',
  'RespRate',
  'O2SatMeasr',
  'PaO2Measr',
  'PulmBloodGasPPCO2',
  'PulmGasExFIO2Val',
  'Bilirubin',
  'Lactate',
  'Creatinine',
  'Glucose',
  'Potassium',
  'Sodium',
  'White.Blood.Cell.Count',
  'Hematocrit',
  'Arterial.pH',
  'Prothrombin.Time',
  'Partial.Thrombin.Time',
  'PT.International.Normalized.Ratio'
)

avg_cols_df = c()
for(lab in lab_names){
  high_col = highest_labs[,grep(lab, colnames(highest_labs))]
  low_col = lowest_labs[,grep(lab, colnames(lowest_labs))]
  
  if(!(is.integer(low_col) && length(low_col) == 0L)){
    avg_col = rowMeans(cbind(high_col, low_col), na.rm = FALSE)
  }
  else{
    avg_col = high_col
  }
  avg_cols_df = cbind(avg_cols_df, avg_col)
  
}
colnames(avg_cols_df) = lab_names

# remove highest and lowest columns from base_labs df
base_labs = base_labs[, !colnames(base_labs) %in% c(colnames(highest_labs),colnames(lowest_labs))]

# add in newly calculated columns
base_labs = cbind(base_labs, avg_cols_df)

# Calculate net fluid intake (TotalIns - TotalOuts)
base_labs['Net Fluid Intake'] = base_labs["Ins.Outs.TotalInsMeasr"] - base_labs["Ins.Outs.TotalOutsMeasr"]

# remove Ins/Outs columns 
base_labs = base_labs[, !colnames(base_labs) %in% c("Ins.Outs.TotalInsMeasr", "Ins.Outs.TotalOutsMeasr")]

# Get datatypes
factor_cols = base_labs[,sapply(base_labs, class) == 'factor']

# map string factors to binary values
factor_cols[,1:7] = sapply(factor_cols[,1:7], function(x) mapvalues(x, c(" ", 'No', 'Yes'), c(NA,0,1)))
factor_cols[,8:9] = sapply(factor_cols[,8:9], function(x) mapvalues(x, c(" ", 'Data missing/Unknown/Test not ordered', 'Network hospital', 'Referring hospital'), c(NA, NA,1,1)))
factor_cols[,c(11:12,17)] = sapply(factor_cols[,c(11:12,17)], function(x) mapvalues(x, c(" ", "Data missing/unknown/Not ordered", "Negative", "Positive, administered medically", "Positive, not as the result of medical intervention"), c(NA, NA, 0, 1, 1)))
factor_cols[,c(10,13:16, 18:19)] = sapply(factor_cols[,c(10,13:16, 18:19)], function(x) mapvalues(x, c(" ", "Negative", "Not available", "Positive"), c(NA, 0, NA, 1)))




# insert new factor columns back into base_labs df
base_labs[, colnames(base_labs) %in% colnames(factor_cols)] = factor_cols

#### CT ####

# remove metadata and redundant column 
ct_metadata = c(
  "Baseline.CT.HrsBetInjCTCt"
)

redundant_cols = c(
  "Baseline.CT.CTMidlineShiftMeasr",
  "Baseline.CT.CTIntraparenLesnVolMeasr",
  "Baseline.CT.CTSbdrlLesnVolMeasr",
  "Baseline.CT.CTEpdurlLesnVolMeasr"
)
ct = ct[, !colnames(ct) %in% c(ct_metadata, redundant_cols)]

intraparen_anat_cols = c("CTIntraparenLesnAnatSite.No intraparenchymal areas involved", 
                         "CTIntraparenLesnAnatSite.Right frontal", 
                         "CTIntraparenLesnAnatSite.Left frontal", 
                         "CTIntraparenLesnAnatSite.Right temporal", 
                         "CTIntraparenLesnAnatSite.Left temporal",
                         "CTIntraparenLesnAnatSite.Right parietal",
                         "CTIntraparenLesnAnatSite.Left parietal", 
                         "CTIntraparenLesnAnatSite.Right occipital",
                         "CTIntraparenLesnAnatSite.Left occipital",
                         "CTIntraparenLesnAnatSite.Brainstem/diencephalon/CC", 
                         "CTIntraparenLesnAnatSite.Cerebellar")

intraparen_regions = c('No intraparenchymal areas involved', 
                       'Right frontal', 'Left frontal', 
                       'Right temporal', 'Left temporal',
                       'Right parietal', 'Left parietal', 
                       'Right occipital', 'Left occipital', 
                       'Brainstem/diencephalon/CC', 'Cerebellar')

ct[,intraparen_anat_cols] = 0
for(i in c(1:length(intraparen_regions))){
  ind = grep(intraparen_regions[i], ct[,"Baseline.CT.CTIntraparenLesnAnatSite"])
  ct[ind, intraparen_anat_cols[i]] = 1
}

ct = ct[, !colnames(ct) %in% c("Baseline.CT.CTIntraparenLesnAnatSite")]


sbdrl_anat_cols = c('CTSbdrlLesnAnatSite.No subdural areas involved', 
                    'CTSbdrlLesnAnatSite.Right supratentorial', 
                    'CTSbdrlLesnAnatSite.Left supratentorial', 
                    'CTSbdrlLesnAnatSite.Falcine', 
                    'CTSbdrlLesnAnatSite.Interhemispheric',
                    'CTSbdrlLesnAnatSite.Tentorial', 
                    'CTSbdrlLesnAnatSite.Posterior fossa')

sbdrl_regions = c('No subdural areas involved', 'Right supratentorial', 
                  'Left supratentorial', 'Falcine', 'Interhemispheric',
                  'Tentorial', 'Posterior fossa')

ct[,sbdrl_anat_cols] = 0
for(i in c(1:length(sbdrl_regions))){
  ind = grep(sbdrl_regions[i], ct[,"Baseline.CT.CTSbdrlLesnAnatSite"])
  ct[ind, sbdrl_anat_cols[i]] = 1
}


ct = ct[, !colnames(ct) %in% c("Baseline.CT.CTSbdrlLesnAnatSite")]

epdurl_anat_cols = c('CTEpdurlLesnAnatSite.No subdural areas involved', 
                     'CTEpdurlLesnAnatSite.Right supratentorial', 
                     'CTEpdurlLesnAnatSite.Left supratentorial', 
                     'CTEpdurlLesnAnatSite.Falcine',
                     'CTEpdurlLesnAnatSite.Interhemispheric',
                     'CTEpdurlLesnAnatSite.Tentorial', 
                     'CTEpdurlLesnAnatSite.Posterior fossa')

epdurl_regions = c('No epidural areas involved', 'Right supratentorial', 
                   'Left supratentorial', 'Falcine', 'Interhemispheric',
                    'Tentorial', 'Posterior fossa')

ct[,epdurl_anat_cols] = 0
for(i in c(1:length(epdurl_regions))){
  ind = grep(epdurl_regions[i], ct[,"Baseline.CT.CTEpdurlLesnAnatSite"])
  ct[ind, epdurl_anat_cols[i]] = 1
}


ct = ct[, !colnames(ct) %in% c("Baseline.CT.CTEpdurlLesnAnatSite")]

# remap string values into binary variables for remaining factor columns

# Baseline.CT.CTMesencephalicCistTyp
na_ind = which(grepl("^\\s*$", ct[,'Baseline.CT.CTMesencephalicCistTyp']))
ind = grep('Abnormal', ct[,'Baseline.CT.CTMesencephalicCistTyp'])
ct[,'Baseline.CT.CTMesencephalicCistTyp'] = 0
ct[ind,'Baseline.CT.CTMesencephalicCistTyp'] = 1
ct[na_ind,'Baseline.CT.CTMesencephalicCistTyp'] = NA



# Baseline.CT.CTSAHTyp
na_ind = which(grepl("^\\s*$", ct[,'Baseline.CT.CTSAHTyp']))
ind = grep('Present', ct[,"Baseline.CT.CTSAHTyp"])
ct[,"Baseline.CT.CTSAHTyp"] = 0
ct[ind,"Baseline.CT.CTSAHTyp"] = 1
ct[na_ind,"Baseline.CT.CTSAHTyp"] = NA

# Baseline.CT.IVHStatus
na_ind = which(grepl("^\\s*$", ct[,'Baseline.CT.IVHStatus']))
ind = grep('Present', ct[,"Baseline.CT.IVHStatus"])
ct[,"Baseline.CT.IVHStatus"] = 0
ct[ind,"Baseline.CT.IVHStatus"] = 1
ct[na_ind, 'Baseline.CT.IVHStatus'] = NA

# "Baseline.CT.HydrcephPresInd"
na_ind = which(grepl("^\\s*$", ct[,'Baseline.CT.HydrcephPresInd']))
ind = grep('Yes', ct[,"Baseline.CT.HydrcephPresInd"])
ct[,"Baseline.CT.HydrcephPresInd"] = 0
ct[ind,"Baseline.CT.HydrcephPresInd"] = 1
ct[na_ind,"Baseline.CT.HydrcephPresInd"] = NA

# "Baseline.CT.CTLesnHighMixedDensityInd"
na_ind = which(grepl("^\\s*$", ct[,'Baseline.CT.CTLesnHighMixedDensityInd']))
ind = grep('Present', ct[,"Baseline.CT.CTLesnHighMixedDensityInd"])
ct[,"Baseline.CT.CTLesnHighMixedDensityInd"] = 0 
ct[ind,"Baseline.CT.CTLesnHighMixedDensityInd"] = 1
ct[na_ind,"Baseline.CT.CTLesnHighMixedDensityInd"] = NA


# dummy 'category' columns
cat_cols = c(
  "Baseline.CT.CTMidlineShiftCat",
  "Baseline.CT.CTIntraparenLesnVolCat",
  "Baseline.CT.CTSbdrlLesnVolCat",
  "Baseline.CT.CTEpdurlLesnVolCat"
)


mls_dummy = as.data.frame(dummy(unlist(ct['Baseline.CT.CTMidlineShiftCat']), data = NULL, sep = ".", drop = TRUE, fun = as.factor, verbose = FALSE))
colnames(mls_dummy) = c("MidlineShift.<=5mm",  "MidlineShift.>10mm", "MidlineShift.>5<10mm", "MidlineShift.None", "MidlineShift.NA")
                        
mls_dummy[which(mls_dummy['MidlineShift.NA'] == 1), c("MidlineShift.<=5mm",  "MidlineShift.>10mm", "MidlineShift.>5<10mm", "MidlineShift.None")] = NA
mls_dummy["MidelineShift.NA"] = NULL


ipvol_dummy = as.data.frame(dummy(unlist(ct['Baseline.CT.CTIntraparenLesnVolCat']), data = NULL, sep = ".", drop = TRUE, fun = as.factor, verbose = FALSE))
colnames(ipvol_dummy) = c("IPVolCat.>25cc", "IPVolCat.<=25cc", "IPVolCat.No involvement", "IPVolCat.NA")

ipvol_dummy[which(ipvol_dummy['IPVolCat.NA'] == 1), c("IPVolCat.>25cc", "IPVolCat.<=25cc", "IPVolCat.No involvement")] = NA
ipvol_dummy["IPVolCat.NA"] = NULL


sdvol_dummy = as.data.frame(dummy(unlist(ct['Baseline.CT.CTSbdrlLesnVolCat']), data = NULL, sep = ".", drop = TRUE, fun = as.factor, verbose = FALSE))
colnames(sdvol_dummy) = c("SdVolCat.Evaculated", "SdVolCat.>25cc", "SdVolCat.<=25cc", "SdVolCat.No involvement", "SdVolCat.NA")

sdvol_dummy[which(sdvol_dummy["SdVolCat.NA"] == 1), c("SdVolCat.Evaculated", "SdVolCat.>25cc", "SdVolCat.<=25cc", "SdVolCat.No involvement")] = NA
sdvol_dummy["SdVolCat.NA"] = NULL


epvoldummy = as.data.frame(dummy(unlist(ct['Baseline.CT.CTEpdurlLesnVolCat']), data = NULL, sep = ".", drop = TRUE, fun = as.factor, verbose = FALSE))
colnames(epvoldummy) = c("EpVolCat.Evaculated", "EpVolCat.>25cc", "EpVolCat.<=25cc", "EpVolCat.No involvement", "EpVolCat.NA")

epvoldummy[which(epvoldummy["EpVolCat.NA"] == 1), c("EpVolCat.Evaculated", "EpVolCat.>25cc", "EpVolCat.<=25cc", "EpVolCat.No involvement")] = NA
epvoldummy["EpVolCat.NA"] = NULL

ct = cbind(ct, mls_dummy, ipvol_dummy, sdvol_dummy, epvoldummy)
ct = ct[,!colnames(ct) %in% cat_cols]

#### Demographics ####

# Drop columns with all NA's
demographics = demographics[, colSums(is.na(demographics)) != nrow(demographics)]

# Drop 'record' column
demographics = demographics[,-1]

# Convert two-factor variables to binary values (0,1) 

# Gender
demographics['Demographics.GenderTyp'] = revalue(unlist(demographics['Demographics.GenderTyp']), c('Male' = 0, 'Female' = 1))

# Hand Preference
demographics['Demographics.HandPrefTyp'] = mapvalues(unlist(demographics['Demographics.HandPrefTyp']), 
                                                     c('Right hand', 'Left hand', 'Unknown'), 
                                                     c(0,1,NA))

# Employment Status: Cosolidate into Employed (1), Unemployed (0) 
demographics['Demographics.EmplymtStatusPreInjTyp'] = revalue(unlist(demographics['Demographics.EmplymtStatusPreInjTyp']),
                             c('Full-time student (and unemployed)' = 0,
                               "Employed full-time" = 1,
                               "Homemaker" = 0, 
                               "Unemployed/Disabled" = 0, 
                               "Retired" = 0,
                               "Employed part-time" = 1,
                               "Part-time student (and unemployed)" = 0,
                               "Data missing/Unknown/Refused" = NA
                              ))

# Ethnicity
demographics['Demographics.EthnCatCOBRIT'] = revalue(unlist(demographics['Demographics.EthnCatCOBRIT']), c('Non-Hispanic' = 0, 'Hispanic' = 1))

# Dummy Race variable 

demographics['Demographics.RaceCatCOBRIT'] = mapvalues(unlist(demographics['Demographics.RaceCatCOBRIT']),
                                                       c('Black or African American','White', 'Asian', "American Indian or Alaskan Native","More than one race",          
                                                         "Native Hawaiian or other Pacific Islander", 'Data Missing/Unknown/Refused'), 
                                                       c('Black', 'White', 'Asian/Other', 'Asian/Other', 'Asian/Other','Asian/Other', NA))


race_dummy = as.data.frame(dummy(unlist(demographics['Demographics.RaceCatCOBRIT']), data = NULL, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
colnames(race_dummy) = c('Race.Asian/Other', 'Race.Black', 'Race.White', 'Race.NA')

race_dummy[which(race_dummy['Race.NA'] == 1), c('Race.Asian/Other', 'Race.Black', 'Race.White')] = NA
race_dummy = race_dummy[, -4]

demographics = cbind(demographics, race_dummy)
demographics = demographics[,!colnames(demographics) %in% c('Demographics.RaceCatCOBRIT')]


# Marital Status, Married or Single
demographics$Demographics.MartlPartnerStatusCOBRIT = mapvalues(unlist(demographics$Demographics.MartlPartnerStatusCOBRIT),
                                                               c("Single, never married","Married (common or civil law)", "Divorced or separated", "Widowed","Data Missing/Unknown/Refused"), c("Single","Married","Divorced/Widowed","Divorced/Widowed",NA))      
  
marr_dummy = as.data.frame(dummy(unlist(demographics$Demographics.MartlPartnerStatusCOBRIT),data = NULL, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))                                                              
colnames(marr_dummy) =  c("MarStatus.Divorced/Widowed", "MarStatus.Married", "MarStatus.Single", "MarStatus.NA")                                                                                                                            
marr_dummy[which(unlist(marr_dummy['MarStatus.NA']) == 1), c("MarStatus.Divorced/Widowed", "MarStatus.Married", "MarStatus.Single")] = NA
marr_dummy = marr_dummy[, -4]

demographics = cbind(demographics, marr_dummy)
demographics = demographics[,!colnames(demographics) %in% c('Demographics.MartlPartnerStatusCOBRIT')]


# Living Situation
# Decision point: consolidate  "unknown" living situation with "lives alone"
demographics$Demographics.LivingSituationPreInjTyp = mapvalues(demographics$Demographics.LivingSituationPreInjTyp,
               c("Lives with spouse and/or other family member(s)", "Lives Alone",                                         
                 "Lives with friend(s) or roommate(s) or co-habitating", "Data Missing/Unknown/Refused",                        
                 "Other, specify", "Lives in a group home/assisted living"),
               c("Living.Family", "Living.Alone", "Living.Cohabit", "Living.NA", "Living.Alone", "Living.Cohabit"))


living_dummy = as.data.frame(dummy(unlist(demographics$Demographics.LivingSituationPreInjTyp), data = NULL, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
colnames(living_dummy) = c("Living.NA", "Living.Cohabit","Living.Alone", "Living.Family")
living_dummy[which(living_dummy['Living.NA'] == 1), c("Living.Cohabit","Living.Alone", "Living.Family")] = NA
living_dummy = living_dummy[, -1]

demographics = cbind(demographics, living_dummy)
demographics = demographics[,!colnames(demographics) %in% c('Demographics.LivingSituationPreInjTyp')]

# Education
demographics$Demographics.EduLvlUSATypCOBRIT = mapvalues(unlist(demographics$Demographics.EduLvlUSATypCOBRIT),
                 c("Some high school", "Some college", "College graduate", "High school graduate/GED",              
                   "Graduate school completed","Data missing/Unknown/Refused",
                   "Technical, Vocational, or Trade School", "Some elementary School", "Some graduate school"),
                 c("<=HS","SomeCol/TradeSch",">=ColGrad","<=HS", ">=ColGrad", NA, "SomeCol/TradeSch", "<=HS", ">=ColGrad")
                 )

educ_dummy = as.data.frame(dummy(unlist(demographics$Demographics.EduLvlUSATypCOBRIT), data = NULL, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
colnames(educ_dummy) = c("Educ.>=ColGrad", "Educ.<=HS","Educ.SomeCol/TradeSch", "Educ.NA")
educ_dummy[which(educ_dummy['Educ.NA'] == 1), c("Educ.>=ColGrad", "Educ.<=HS","Educ.SomeCol/TradeSch")] = NA
educ_dummy = educ_dummy[, -4]

demographics = cbind(demographics, educ_dummy)
demographics = demographics[,!colnames(demographics) %in% c('Demographics.EduLvlUSATypCOBRIT')]

# Annual Income
demographics$Demographics.AnnualIncomePreInjCat = mapvalues(demographics$Demographics.AnnualIncomePreInjCat,
            c("None", "Data missing/Unknown/Refused", "$30,000-$49,999 per year", "$50,000-$99,999 per year",    
              "Less than $10,000 per year", "$10,000-$29,999 per year", "More than $100,000 per year"),
            c("None", NA, "$30,000-$49,999 per year", "$50,000-$99,999 per year",    
              "Less than $10,000 per year", "$10,000-$29,999 per year", "More than $100,000 per year"))

income_dummy = as.data.frame(dummy(unlist(demographics$Demographics.AnnualIncomePreInjCat), data = NULL, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
colnames(income_dummy) = c("Income.$10,000-$29,999 per year", "Income.$30,000-$49,999 per year", "Income.$50,000-$99,999 per year",
                           "Income.Less than $10,000 per year", "Income.More than $100,000 per year", "Income.None", "Income.NA")
income_dummy[which(income_dummy['Income.NA'] == 1), c("Income.$10,000-$29,999 per year", "Income.$30,000-$49,999 per year", "Income.$50,000-$99,999 per year","Income.Less than $10,000 per year", "Income.More than $100,000 per year", "Income.None")] = NA
income_dummy = income_dummy[, -7]

demographics = cbind(demographics, income_dummy)
demographics = demographics[,!colnames(demographics) %in% c('Demographics.AnnualIncomePreInjCat')]

#### Injury Info ####

# Only columns we are keeping from 'injury info' are Injury Time Range and TBI Mechanism. All other
# columns are metadata

# Injury Time

injury_info$Injury.information.InjTimeRng = mapvalues(injury_info$Injury.information.InjTimeRng,
                          c(
                            '00:00 thru 02:59',
                            '03:00 thru 05:59',
                            '06:00 thru 08:59',
                            '09:00 thru 11:59',
                            '12:00 thru 14:59',
                            '15:00 thru 17:59',
                            '18:00 thru 20:59',
                            '21:00 thru 23:59'), c(
                                                    '24:00-5:59',
                                                    '24:00-5:59',
                                                    '6:00-11:59', 
                                                    '6:00-11:59',
                                                    '12:00-17:59',
                                                    '12:00-17:59',
                                                    '18:00-23:59',
                                                    '18:00-23:59'))



injtime_dummy = as.data.frame(dummy(unlist(injury_info$Injury.information.InjTimeRng), data = NULL, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
colnames(injtime_dummy) = c(paste("InjuryInfo.", levels(injury_info$Injury.information.InjTimeRng)))

injury_info = cbind(injury_info, injtime_dummy)
injury_info = injury_info[,!colnames(injury_info) %in% c('Injury.information.InjTimeRng')]

# Injury Mechanism
injury_info$Injury.information.TBIMechInjTyp = mapvalues(injury_info$Injury.information.TBIMechInjTyp,
                       c("Another Mechanism, specify","Assault",                                               
                         "Fall from a moving object (bike/skateboard/horse/etc.)",
                         "Fall from stationary object (roof/ladder/etc.)",        
                         "Individual struck by any type of vehicle",
                         "Motor vehicle (driver/passenger)",                     
                         "Motorcycle/ATV/golf cart (driver/passenger)",
                         "Sports (football/gymnastics/etc.)",                    
                         "Struck on head by object, not assault (tree/etc.)"),
                       c("Other", "Other", "Fall", "Fall", "Motor", "Motor", "Motor", "Other", "Other"))

injmech_dummy = as.data.frame(dummy(unlist(injury_info$Injury.information.TBIMechInjTyp), data = NULL, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
colnames(injmech_dummy) = c("InjMech.Other", "InjMech.Fall", "InjMech.Motor")

injury_info = cbind(injury_info, injmech_dummy)
injury_info = injury_info[,!colnames(injury_info) %in% c('Injury.information.TBIMechInjTyp')]

# remove metadata columns
metadata_cols = c("Injury.information.InjTimeEstimateMinCt", "Injury.information.HospPresTyp",             
"Injury.information.HospTrnsprtTypCOBRIT", "Injury.information.HrsBetInjRandCt",         
"Injury.information.HrsBetInjArrRefHospCt", "Injury.information.HrsBetInjArrCOBRITHospCt")

injury_info = injury_info[,!colnames(injury_info) %in% metadata_cols]

#### Safety Labs ####

# Filter for Day 1 labs
safety_labs = safety_labs[safety_labs$Evaluation.Day.EvaluationDayNum == 1,]

# Remove metadata 
safety_labs = safety_labs[,-grep('blood.draw.time', colnames(safety_labs))]

# Restructure index and drop remaining metadata cols
row.names(safety_labs) = safety_labs$Main.GUID
safety_labs$Evaluation.Day.EvaluationDayNum = NULL
safety_labs$Main.GUID = NULL

#### MedHx ####

# Drop columns with all NA's
medhx = medhx[, colSums(is.na(medhx)) != nrow(medhx)]
medhx$record = NULL


medhx = medhx %>% fill(Main.GUID)
medhx = cast(medhx, Main.GUID ~ Conditions.COBRITMedConditionTyp, value = "Conditions.MedclCondExst3MonthPreInjInd")

medhx <- as.matrix(medhx)
medhx[which(medhx == "No")] = 0 
medhx[which(medhx == "Yes")] = 1
medhx[which(medhx == "Not documented/Unknown")] = NA
medhx[which(medhx == " ")] = NA 
medhx <- as.data.frame(medhx)
row.names(medhx) = medhx$Main.GUID
medhx$Main.GUID = NULL

#### CONCATENATE INDIVIDUAL DATAFRAMES ####

# merge demographics, injury_info, base_labs, safety_labs, ct, medhx

df1 = merge(demographics, injury_info, by=0)
row.names(df1) = df1$Row.names
df1$Row.names = NULL
df2 = merge(df1, base_labs, by=0)
row.names(df2) = df2$Row.names
df2$Row.names = NULL
df3 = merge(df2, safety_labs, by=0, all=TRUE)
row.names(df3) = df3$Row.names
df3$Row.names = NULL
df4 = merge(df3, ct, by=0)
row.names(df4) = df4$Row.names
df4$Row.names = NULL
cobrit_merged = merge(df4, medhx, by=0)
row.names(cobrit_merged) = cobrit_merged$Row.names
cobrit_merged$Row.names = NULL         # cobrit_cleaned is final cleaned df, input for missing data imputation


save(cobrit_merged, file=paste(path, '1_Data_Cleaning/cobrit_cleaned.RData', sep=""))
# 1213 x 156
