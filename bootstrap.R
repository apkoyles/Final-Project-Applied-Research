#set seed for bootstrapping
set.seed(29)

#bootstrap for r-squared value
rsq <- function(formula, data, index) {
  d <- data[index,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

# bootstrapping with 1000 replications
rsq_boot = boot(survey_data, rsq, 1000, formula=Future_donate~Jag_Assoc_Group+ID_JagAvg+ID_iupuiAvg+
                  Jag_feelingsAvg+Age+Gender+Employment+iupuiStatus)

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
se = boot(survey_data, boot.fn, 1000, formula = Future_donate~Jag_Assoc_Group+ID_JagAvg+ID_iupuiAvg+
            Jag_feelingsAvg+Age+Gender+Employment+iupuiStatus)
se

#low bias and se indicates that the model does a good job predicting the coefficients

boot.ci(se)

#conf. intervals are a little wide but we still appear to have decent estimates of the coefficients