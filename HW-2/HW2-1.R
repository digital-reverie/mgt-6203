# Task 1 Step 1
# Load dataset
loan <- read.csv("HW-2/Loan.csv")

# Convert Education into factor
loan$Education <- as.factor(loan$Education)

# Quick check
str(loan)

# Task 1 Step 2
lpm <- lm(Loan ~ Income + Family + CCAvg + Education, data = loan)
summary(lpm)

# Task 1 Step 3
fitted_vals <- fitted(lpm)
summary(fitted_vals)

# Customers with fitted y < 0 or > 1
subset(loan, fitted_vals < 0 | fitted_vals > 1)[1:10, ]


# Task 2 Step 4
logit <- glm(Loan ~ Income + Family + CCAvg + Education,
             data = loan, family = binomial(link="logit"))
summary(logit)


# Task 2 Step 5
# Predicted probabilities
pred_probs <- predict(logit, type = "response")

# Threshold = fraction of success in data
threshold <- mean(loan$Loan)

# Predictions
pred_class <- ifelse(pred_probs > threshold, 1, 0)

# Confusion matrix
table(Predicted = pred_class, Actual = loan$Loan)

# Percent correctly predicted (overall and by class)
overall_pcp <- mean(pred_class == loan$Loan)
pcp_y1 <- mean(pred_class[loan$Loan==1] == 1)
pcp_y0 <- mean(pred_class[loan$Loan==0] == 0)

overall_pcp; pcp_y1; pcp_y0


# Task 2 Step 6
mean_income <- mean(loan$Income)
mean_family <- mean(loan$Family)
mean_ccavg <- mean(loan$CCAvg)

# Formula calculation
xb <- coef(logit)["(Intercept)"] +
      coef(logit)["Income"] * mean_income +
      coef(logit)["Family"] * mean_family +
      coef(logit)["CCAvg"] * mean_ccavg +
      coef(logit)["Education2"] * 1  # since Education=2

prob <- exp(xb) / (1 + exp(xb))
prob

newdata <- data.frame(
  Income = mean_income,
  Family = mean_family,
  CCAvg = mean_ccavg,
  Education = factor(2, levels = c(1,2,3))
)
predict(logit, newdata, type="response")

# Task 2 Step 7
coef(lpm)
coef(logit)


# Task 2 Step 8
p <- predict(logit, newdata, type="response") # prob at means
beta <- coef(logit)

# partial effect for Income
dydx_income <- p * (1 - p) * beta["Income"]
dydx_family <- p * (1 - p) * beta["Family"]
dydx_ccavg  <- p * (1 - p) * beta["CCAvg"]
dydx_edu2   <- p * (1 - p) * beta["Education2"]
dydx_edu3   <- p * (1 - p) * beta["Education3"]

c(Income=dydx_income, Family=dydx_family, CCAvg=dydx_ccavg,
  Education2=dydx_edu2, Education3=dydx_edu3)

coef(lpm)

