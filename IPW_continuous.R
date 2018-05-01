library(ipw)
library(survey)
data = read.csv("data/tidy_data.csv")
temp <- ipwpoint(exposure = Uninsured, family="gaussian", numerator=~1, denominator=~dem08_frac2+dem12_frac2+rep08_frac2+rep12_frac2+Median.Earnings.2010.dollars+Poverty.Rate.below.federal.poverty.threshold+Management.professional.and.related.occupations+Service.occupations+Sales.and.office.occupations+Farming.fishing.and.forestry.occupations+Construction.extraction.maintenance.and.repair.occupations+Unemployment, data = data) 
summary(temp$ipw.weights)
w <- temp$ipw.weights
#truncate weights at 1%
tw2 <- ifelse(w < quantile(w, probs=.01), quantile(w, probs=.01), w)
tw2 <- ifelse(w > quantile(w, probs=.99), quantile(w, probs=.99), tw2)
summary(tw2)
ipwplot(weights = tw2, logscale = FALSE, main = "Stabilized weights")
data$sw <- tw2
data$Uninsured2 <- data$Uninsured ^ 2
msm <- (svyglm(rep16_frac2 ~ Uninsured + Uninsured2, design = svydesign(~ 1, weights = ~sw, data = data)))
coef(msm)
confint(msm)