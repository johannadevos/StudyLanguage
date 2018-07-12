### Checking the assumptions for regression models
### Following the tutorial by Bodo Winter (2013)

library(arm) # To create binned residuals plot

# Define the model you want to check the assumptions for as 'model' (e.g., model <- reg_group)
# Define the data as 'data' (e.g., data <- no_dropout)

## Is there a linear relationship between the dependent and independent variables?
# The plot should not show any obvious pattern in the residuals
plot(fitted(model), residuals(model), xlab = "Predicted values", ylab = "Residuals")
abline(h = 0)

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(model), resid(model), cex.pts=1, col.int="black", xlab = "Predicted values")

# A weak relationship is visible: smaller fitted values tend to have negative residuals,
# larger fitted values seem to have positive residuals
cor.test(fitted(model), residuals(model))
summary(lm(residuals(model) ~ fitted(model)))

#2 Absence of collinearity

#3 Homoskedasticity
# The standard deviations of the residuals should not depend on the x-value
plot(fitted(model), residuals(model), xlab = "Predicted values", ylab = "Residuals")
abline(h = 0)

#4 Normality of residuals
hist(residuals(model)) 
qqnorm(residuals(model))

#5 Absence of influential data points

# Calculate Cook's distance and visualise outcomes
data$Cook <- cooks.distance.estex(influence(model, group = 'SubjectCode'))
plot(data$Cook, ylab = "Cook's distance")
# Different guidelines. Either, Cook's distance should not be >1 or >0.85 (assumption met)
# Or, it shouldn't be >4/n (assumption not met, but no real outliers)

#6 Independence
