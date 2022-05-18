library(data.table)
library(dplyr)
library(janitor)

#read in data
#column names were edited in excel prior to import
survey = fread(file = 'Data/Jagathon_Survey.csv')

#remove unnecessary rows
survey_clean1 = survey %>% filter(!row_number() %in% c(2))

#use first row as column names
survey_clean2 = survey_clean1 %>% row_to_names(row_number = 1)

#remove unused columns
survey_clean3 = survey_clean2 %>% select(Finished, Jag_Assoc:Zip)

#remove unfinished surveys
survey_clean4 = survey_clean3 %>% filter(Finished == '1')

#convert values to numeric (currently stored as characters)
survey_clean5 = survey_clean4 %>% mutate_at(c('ID_JagCom', 'ID_JagOverlap', 'ID_JagAttach', 
                                              'ID_JagBelong', 'ID_JagValue', 'ID_JagImportant', 
                                              'ID_iupuiCom', 'ID_iupuiOverlap', 'ID_iupuiAttach', 
                                              'ID_iupuiBelong', 'ID_iupuiValue', 'ID_iupuiImportant', 
                                              'Jag_feelings_overall', 'Jag_feelings_pleasant', 
                                              'Jag_feelings_attractive', 'Future_part', 'Future_donate', 
                                              'Gender', 'iupuiStatus', 'Employment'), as.numeric)

#write.csv(survey_clean5, 'Data/survey_almostClean.csv')

#create not in function for further cleaning
`%!in%` <- Negate(`%in%`)

#create list of numbers to use for cleaning age column
ages = c(1:100)

#clean age column by removing non-numeric or unrealistic answers (all values not between 1 and 100)
survey_clean5$Age[survey_clean5$Age %!in% ages] <- 0

#compute mean of age column to input for missing values
age_real = as.numeric(survey_clean5$Age[-c(28, 80)])
mean(age_real)

#mean age is 23.523, which will be rounded to 23.5

#replace missing ages with mean
survey_clean5$Age[survey_clean5$Age == 0] <- 23.5

#convert Age column to numeric
survey_clean5$Age = as.numeric(survey_clean5$Age)

#clean association column that indicates level of association with Jagathon
# create different levels for various levels of association
# 1: went to Jagathon, 2: went multiple times, 3: heard of Jagathon, 4: Committee/chair member,
# 5: Jagpal, 6: Ambassador, 7: Alumni, 8: Never gone
# Groups will be: 1-No involvement, 2-minimal involvement, 3-heavy involvement

#write.csv(survey_clean5, 'Data/survey_almostClean.csv')

#groups were assigned in excel
#averages of the identity questions were also calculated in excel

#read in new cleaned dataset
survey_cleaner = fread(file = 'Data/survey_cleaner.csv')

#check data types
sapply(survey_cleaner, class)

#convert data types to numeric for columns that are to be used in regression
survey_clean6 = survey_cleaner %>% mutate_at(c('Jag_Assoc_Group', 'ID_JagAvg', 'ID_iupuiAvg', 
                                               'Jag_feelingsAvg'), as.numeric)

#check data types again
sapply(survey_clean6, class)

#convert specific variables to factors for use in regression
survey_clean6$Jag_Assoc_Group = factor(survey_clean6$Jag_Assoc_Group)
survey_clean6$Gender = factor(survey_clean6$Gender)
survey_clean6$Employment = factor(survey_clean6$Employment)
survey_clean6$iupuiStatus = factor(survey_clean6$iupuiStatus)

str(survey_clean6)

#write.csv(survey_clean6, 'Data/survey_clean.csv')

survey_clean7 = fread('Data/survey_clean.csv')

model_vars = survey_clean7 %>% select(Jag_Assoc_Group, ID_JagAvg, ID_iupuiAvg, Jag_feelingsAvg, 
                                      Age, iupuiStatus, Employment)

model_vars$Jag_Assoc_Group = factor(model_vars$Jag_Assoc_Group)
model_vars$iupuiStatus = factor(model_vars$iupuiStatus)
model_vars$Employment = factor(model_vars$Employment)

levels(model_vars$Jag_Assoc_Group) = c('No Involvement', 'Some Involvement', 'High Involvement')
levels(model_vars$iupuiStatus) = c('Undergraduate', 'Graduate', 'Other')
levels(model_vars$Employment) = c('Part-Time Student', 'Full-Time Student', 'Part-Time Employed', 
                                  'Full-Time Employed', 'Unemployed')

colnames(model_vars) = c('Involvement', 'Jagathon Identity', 'IUPUI Identity', 'Feelings Towards Jagathon',
                         'Age', 'Student Status', 'Employment')

#more cleaning for use in Tableau
survey_clean8 = fread('Data/surveyFactors.csv')

#rename the levels of several categorical variables
survey_clean8$Gender = factor(survey_clean8$Gender)
levels(survey_clean8$Gender) = c('Male', 'Female')
survey_clean8$Employment = factor(survey_clean8$Employment)
levels(survey_clean8$Employment) = c('Part-Time Student', 'Full-Time Student', 'Part-Time Employed',
                                     'Full-Time Employed', 'Unemployed')
survey_clean8$iupuiStatus_value = factor(survey_clean8$iupuiStatus_value)
levels(survey_clean8$iupuiStatus_value) = c('Freshman', 'Sophomore', 'Junior', 'Senior', 'Graduate', 
                                    'Professional', 'Staff', 'Other')

#write.csv(survey_clean8, 'Data/surveyTab.csv')
