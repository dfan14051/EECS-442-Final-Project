input = read.csv("data/tidy_data.csv")
fit <- lm(formula=rep16_frac2~.,data=input)
print(summary(fit))
coef(fit)
confint(fit)