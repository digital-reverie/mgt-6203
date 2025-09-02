# Import data
cars <- read.csv("HW-1/UsedCars.csv")

# Fit regression model
model_full <- lm(Price ~ Age + KM + HP + Metallic + Automatic + CC + Doors + Gears + Weight, data = cars)

# Show results
summary(model_full)


# Get fitted values and residuals
fitted_vals <- fitted(model_full)
residuals_vals <- resid(model_full)

# Compare for first 10 rows
results <- data.frame(
  Actual_Y = cars$Price[1:10],
  Fitted_Y = fitted_vals[1:10],
  Residuals = residuals_vals[1:10]
)

results


coefs <- summary(model_full)$coefficients
betas <- coefs[,1]
se_betas <- coefs[,2]
t_stats_manual <- betas / se_betas

# Compare with R’s built-in t values
data.frame(
  Manual_t = t_stats_manual,
  R_t = coefs[,3]
)

df <- model_full$df.residual
t_crit <- qt(0.975, df)   # two-tailed
t_crit

p_vals_manual <- 2 * (1 - pt(abs(t_stats_manual), df))
data.frame(
  Manual_p = p_vals_manual,
  R_p = coefs[,4]
)

y <- cars$Price
y_hat <- fitted(model_full)
rss <- sum((y - y_hat)^2)
tss <- sum((y - mean(y))^2)
R2_manual <- 1 - rss/tss

R2_manual
summary(model_full)$r.squared


library(car)

vif(model_full)

model_weight <- lm(Weight ~ Age + KM + HP + Metallic + Automatic + CC + Doors + Gears, data = cars)
R2_weight <- summary(model_weight)$r.squared
VIF_weight_manual <- 1 / (1 - R2_weight)

VIF_weight_manual

model_reduced <- lm(Price ~ Age + KM + HP + Weight, data = cars) # example, adjust based on your Step 6
summary(model_reduced)

summary(model_full)$r.squared
summary(model_full)$adj.r.squared

summary(model_reduced)$r.squared
summary(model_reduced)$adj.r.squared