library(data.table)
library(dplyr)
library(psych)
library(ggplot2)

#read in data, create numeric only dataframe to be used in correlation plot
data = fread(file = 'Data/survey_clean.csv')
data_correlations = data %>% select(Jag_Assoc_Group, ID_JagAvg, ID_iupuiAvg, Jag_feelingsAvg, 
                                    Future_part:Gender, iupuiStatus, Employment)

#create correlation plot
colnames(data_correlations) = c('Involvement', 'Jagathon_Identity', 'IUPUI_Identity',
                                  'Feelings_Towards_Jagathon', 'Future_Participation', 'Future_Donation',
                                  'Age', 'Gender', 'Student_Status', 'Employment')
corPlot(data_correlations, cex = 0.4)

data = data %>% rename(`Future Participation` = Future_part, `Future Donation` = Future_donate)

#distribution of variables
ggplot(data, aes(x = ID_iupuiAvg)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'light blue', bins = 6) +
  geom_density(alpha = 0.05, color = 'red')

ggplot(data, aes(x = ID_JagAvg)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'light blue', bins = 6) +
  geom_density(alpha = 0.05, color = 'red')

ggplot(data, aes(x = `Future Donation`)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'light blue', bins = 6) +
  geom_density(alpha = 0.05, color = 'red')

describe(data$`Future Donation`)

ggplot(data, aes(x = `Future Participation`)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'light blue', bins = 6) +
  geom_density(alpha = 0.05, color = 'red')

describe(data$`Future Participation`)

ggplot(data, aes(x = Jag_feelingsAvg)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'light blue', bins = 6) +
  geom_density(alpha = 0.05, color = 'red')

describe(data$Jag_feelingsAvg)

describe(data$Age)

data_factored = fread(file = 'Data/surveyFactors.csv')

data_factored$Involvement = factor(data_factored$Involvement)

table(data_factored$Involvement)




