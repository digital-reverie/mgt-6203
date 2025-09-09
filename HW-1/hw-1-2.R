# 1. Import with strings as factors
dat <- read.csv("HW-1/UsedCars2.csv", stringsAsFactors = TRUE)

# sanity checks
str(dat)
summary(dat)

# make sure the intended categorical vars are factors
# (stringsAsFactors should already do this for Model and Color)
is.factor(dat$Model); is.factor(dat$Color)

# Metallic/Automatic are 0/1 numeric per spec—leave numeric (as asked).
# If your file happens to parse them as characters, force numeric:
# dat$Metallic  <- as.numeric(dat$Metallic)
# dat$Automatic <- as.numeric(dat$Automatic)


# from Part 1: Age, KM, HP, Automatic, Gears, Weight were significant
lm2 <- lm(Price ~ Age + KM + HP + Automatic + Gears + Weight + Color, data = dat)
summary(lm2)

# How is Color treated?
levels(dat$Color)         # all levels
contrasts(dat$Color)      # dummy coding matrix
model.matrix(lm2)[1:6, ]  # peek at actual design columns (optional)


drop1(lm2, scope = ~ Color, test = "F")  # partial F-test for the factor
# or compare to a model without Color
lm2_nocolor <- lm(Price ~ Age + KM + HP + Automatic + Gears + Weight, data = dat)
anova(lm2_nocolor, lm2)  # if p-value small, Color adds explanatory power

lm_int <- lm(Price ~ Age + KM + Age:KM + HP + Automatic + Gears + Weight, data = dat)
# shorthand: Price ~ Age * KM + HP + Automatic + Gears + Weight
summary(lm_int)

# Optional partial F test for the interaction term
lm_no_int <- lm(Price ~ Age + KM + HP + Automatic + Gears + Weight, data = dat)
anova(lm_no_int, lm_int)

lm_int <- lm(Price ~ Age + KM + Age:KM + HP + Automatic + Gears + Weight, data = dat)
# shorthand: Price ~ Age * KM + HP + Automatic + Gears + Weight
summary(lm_int)

# Optional partial F test for the interaction term
lm_no_int <- lm(Price ~ Age + KM + HP + Automatic + Gears + Weight, data = dat)
anova(lm_no_int, lm_int)

# scatter
plot(dat$KM, dat$Price, pch = 20, xlab = "KM", ylab = "Price",
     main = "Price vs KM with 4th-degree polynomial fit (holding Automatic at mean)")

# polynomial regression up to degree 4, raw basis + Automatic
lm3 <- lm(Price ~ poly(KM, 4, raw = TRUE) + Automatic, data = dat)
summary(lm3)
# Total coefficients = Intercept + 4 (KM^1..KM^4) + 1 (Automatic) = 6

# build a KM grid and predict at mean(Automatic)
km.grid <- seq(from = min(dat$KM, na.rm=TRUE),
               to   = max(dat$KM, na.rm=TRUE),
               by = 1000)

auto_mean <- mean(dat$Automatic, na.rm = TRUE)  # per instructions
preds <- predict(lm3, newdata = list(KM = km.grid,
                                     Automatic = rep(auto_mean, length(km.grid))))

# overlay fitted curve
lines(km.grid, preds, lwd = 2)
