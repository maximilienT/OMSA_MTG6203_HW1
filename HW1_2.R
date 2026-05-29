# 1. Import and run LM on usedcars data
my_data <- read.csv("Loan.csv")
str(my_data)
my_data$Education <- as.factor(my_data$Education)

# 2. Linear probability model
loan_linear <- lm(Loan ~., data = my_data)
summary(loan_linear)

# 3. values over or under 1 and 0
head(sort(predict(loan_linear)),10)
tail(sort(predict(loan_linear)),10)

# 4. run logit model
loan_log <- glm(Loan ~., family = binomial(link = logit), data = my_data)
summary(loan_log)

# 5. confusion matrix and percent correctly predicted
yhat <- rep(1, nrow(my_data))
yhat[predict(loan_log, type = "response") < mean(my_data$Loan)] <-0

confusion <- table(yhat, my_data$Loan)
confusion

# PCP overall
sum(diag(confusion)/sum(confusion))
# PCP 1
confusion[1,1] / sum(confusion[,1])
# PCP 2
confusion[2,2] / sum(confusion[,2])

# 6. calculate predicted prob of success
coef(loan_log)
colMeans(my_data[,2:4])
# 1
exp(-13.17783285 + 0.05979075*73.774200 + 0.58707882*2.396400 + 0.16267911*1.937938 + #What to do for education levels??)

# 2
xvalues <- data.frame(Loan = 73.774200, Family = 2.396400, CCAvg = 1.937938, )
predict(loan_log, newdata = xvalues, type="response")

# 7. Compare coefficients from linear and log models
cbind(coef(loan_linear), coef(loan_log))

# 8. calculate partial effects
