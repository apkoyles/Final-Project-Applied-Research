library(data.table)
library(dplyr)
library(psych)
library(ggplot2)
library(car)
library(boot)
library(ggpubr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

#read in data
survey_data = fread(file = 'Data/surveyFactors.csv')

survey_data = survey_data %>% select(Finished:Zip)

#create factors for the variables that will be used in regression that need it
survey_data$`Student Status` = factor(survey_data$`Student Status`)
survey_data$`Involvement` = factor(survey_data$`Involvement`)
survey_data$Employment = factor(survey_data$Employment)
levels(survey_data$Employment) = c('Part-Time Student', 'Full-Time Student', 'Part-Time Employed',
                                     'Full-Time Employed', 'Unemployed')
levels(survey_data$`Student Status`) = c('Undergraduate Student', 'Graduate Student', 'Non-Student')
levels(survey_data$`Involvement`) = c('No Involvement', 'Some Involvement', 'High Involvement')

#write.csv(survey_data, 'Data/surveyFactors.csv')

library(ltm)

#calculate cronbachs alpha with confidence interval
#0.696 is a questionable alpha but is very close to the acceptable range, indicating that the results from
# this study have potential to be valid
set.seed(123)
cronbach.alpha(survey_data, CI = T)

#create initial participation model
mPart_init = lm(`Future Participation`~Involvement+`Jagathon Identity`+`IUPUI Identity`+`Feelings Towards Jagathon`
                +Age+`Student Status`+Employment, data = survey_data)
summary(mPart_init)
tab_model(mPart_init)

#create initial donation model
mDonate_init = lm(`Future Donation`~Involvement+`Jagathon Identity`+`IUPUI Identity`+`Feelings Towards Jagathon`
                  +Age+`Student Status`+Employment, data = survey_data)
summary(mDonate_init)
tab_model(mDonate_init)

#calculate vif to see if there is any multicollinearity
vif(mPart_init)
vif(mDonate_init)

#low vifs (<5) indicate minimal multicollinearity

#bootstrap for donation model
#set seed for bootstrapping
set.seed(29)

#bootstrap for r-squared value
rsq <- function(formula, data, index) {
  d <- data[index,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

# bootstrapping with 1000 replications
rsq_boot = boot(survey_data, rsq, 1000, formula=`Future Donation`~Involvement+`Jagathon Identity`+
                  `IUPUI Identity`+`Feelings Towards Jagathon`+Age+`Student Status`+Employment)

rsq_boot

#low bias and std. error indicates a pretty good estimate from the original sample

#confidence interval for r squared based on bootstrapping
boot.ci(rsq_boot)


#bootstrap for se
boot.fn = function(formula, data, index) {
  d = data[index,]
  fit = lm(formula, data = d)
  return(coef(fit))
}

#1000 replications
se = boot(survey_data, boot.fn, 1000, formula = `Future Donation`~Involvement+`Jagathon Identity`+
            `IUPUI Identity`+`Feelings Towards Jagathon`+Age+`Student Status`+Employment)
se

#low bias and se indicates that the model does a good job predicting the coefficients

boot.ci(se)

#conf. intervals are a little wide but we still appear to have decent estimates of the coefficients

#bootstrap for participation model
#set seed for bootstrapping
set.seed(42)

# bootstrapping with 1000 replications
rsq_boot2 = boot(survey_data, rsq, 1000, formula=`Future Participation`~Involvement+`Jagathon Identity`
                 +`IUPUI Identity`+`Feelings Towards Jagathon`+Age+`Student Status`+Employment)

rsq_boot2

#low bias and std. error indicates a pretty good estimate from the original sample

#confidence interval for r squared based on bootstrapping
boot.ci(rsq_boot2)



#bootstrap for coefficients
partMatrix = model.matrix(~Future_part+ID_Jag+ID_IUPUI+Jag_feelings+Age+status+Employment-1, 
                          data = surveydata2)
donateMatrix = model.matrix(~Future_donate+ID_Jag+ID_IUPUI+Jag_feelings+Age+status+Employment-1,
                            data = surveydata2)

regPart = boot(data = as.data.frame(partMatrix), statistic = boot.fn, formula = Future_part~., R = 1000)
regPart

regDonate = boot(data = as.data.frame(donateMatrix), statistic = boot.fn, formula = Future_donate~., R = 1000)
regDonate

#low bias and se indicates that the model does a good job predicting the coefficients

#split data into test and train set 80/20
set.seed(11)
splits = sort(sample(nrow(survey_data)*0.8))
train = survey_data[splits, ]
test = survey_data[-splits, ]

#fit the two models to the training set
md_train = lm(`Future Donation`~Involvement+`Jagathon Identity`+`IUPUI Identity`+
                `Feelings Towards Jagathon`+Age+`Student Status`+Employment, data = train)
summary(md_train)

mp_train = lm(`Future Participation`~Involvement+`Jagathon Identity`+`IUPUI Identity`
              +`Feelings Towards Jagathon`+Age+`Student Status`+Employment, data = train)
summary(mp_train)

#r2 goes down a little with training, as to be expected

#test the models on the testing data set
## donations model
preds = predict(md_train, newdata = test)

#plot predicted vs actual
ggplot(test, aes(x = preds, y = `Future Donation`)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_cor(aes(label =..rr.label..), label.x = 3, label.y = 7) +
  ggtitle('Predicted vs. Actual Future Donations') + 
  xlab('Predicted Future Donation Response') +
  ylab('Actual Future Donation Response')

## participation model
preds2 = predict(mp_train, newdata = test)

#plot predicted vs actual
ggplot(test, aes(x = preds2, y = `Future Participation`)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_cor(aes(label =..rr.label..), label.x = 3, label.y = 7) +
  ggtitle('Predicted vs. Actual Future Participation') + 
  xlab('Predicted Future Participation Response') +
  ylab('Actual Future Participation Response')


surveydata2 = survey_data
surveydata2 = surveydata2 %>% rename(Future_part = `Future Participation`, Future_donate = `Future Donation`,
                                     ID_Jag = `Jagathon Identity`, ID_IUPUI = `IUPUI Identity`,
                                     Jag_feelings = `Feelings Towards Jagathon`, status = `Student Status`)
