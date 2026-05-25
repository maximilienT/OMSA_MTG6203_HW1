# 1. Import and run LM on usedcars data
my_data <- read.csv("UsedCars.csv")
price_model <- lm(Price ~ Age+KM+HP+Metallic+Automatic+CC+Doors+Gears+Weight, data = my_data)

summary(price_model)

# 2. get fitted and residuals, compare to residuals of model
fit_values <- fitted(price_model)
resid_values <- residuals(price_model)

cbind(my_data$Price, fit_values, resid_values)[1:10,]

# 3. Reproduce Tstat and compare
bhat <- summary(price_model)$coefficients[,1]
se <- summary(price_model)$coefficients[,2]
tstat <- bhat / se
cbind(tstat, summary(price_model)$coefficients[,3])

# 4. determine critical value
df <- price_model$df.residual
alpha <- 1-.95
qt(1-alpha,df)

# 5. calculate the p-value
p_val <- 2*pt(-abs(tstat), df)
cbind(p_val, summary(price_model)$coefficients[,4])

# 6. which variables have significant effect on outcome?
cbind(tstat, qt(1-alpha,df))

# 7. Calculate R^2 values
summary(price_model)$r.squared
var(fit_values) / var(my_data$Price)

# 8. multicollinearity
install.packages('car')
library(car)
vif(price_model)

# 9. reproduce VIF for weight
lm.res2 <- lm(Weight ~ Age + KM + HP + Metallic + Automatic + CC + Doors + Gears, data = my_data)
r2.weight <- summary(lm.res2)$r.squared
1/(1-r2.weight)

# 10. Remove insignificant variables
lm.removed <- lm(Price ~ Age + KM + HP + Automatic + Gears + Weight, data = my_data)
summary(lm.removed)

# 11. compare models
cbind(summary(price_model)$r.squared, summary(price_model)$adj.r.squared)
cbind(summary(lm.removed)$r.squared, summary(lm.removed)$adj.r.squared)

# 12. 