# ECONOMETRICS HOMEWORK 
# Németh Mariann - WZTT7X

# dataset ----------------------------------------------------------------------

used_cars <- read.csv("used_cars.csv")


#selecting variables
cars_v1 <- used_cars[, c("brand", "model_year", "milage", "fuel_type", 
                      "transmission", "accident", "clean_title", "price")]

#cleaning up data
cars_v1$price <- as.numeric(gsub("[^0-9]", "", cars_v1$price))
#price is very very right tailed, so 250000 cutoff
cars_v1 <- cars_v1[cars_v1$price <= 250000, ]
cars_v1$milage <- as.numeric(gsub("[^0-9]", "", cars_v1$milage))
#simplify fuel type and accident
cars_v1 <- cars_v1[cars_v1$fuel_type %in% c("Diesel", "E85 Flex Fuel", "Gasoline", 
                                   "Hybrid", "Plug-In Hybrid"), ]
cars_v1$fuel_type[cars_v1$fuel_type == "Plug-In Hybrid"] <- "Hybrid"
cars_v1$fuel_type <- as.factor(cars_v1$fuel_type)
cars_v1$accident <- ifelse(is.na(cars_v1$accident) | cars_v1$accident == "" | cars_v1$accident == " ", 
                        "Missing",
                        ifelse(cars_v1$accident == "At least 1 accident or damage reported", 
                               "Accident Reported", 
                               "None reported"))
cars_v1$accident <- as.factor(cars_v1$accident)
#transmissions table is very ugly, so make it nice:
cars_v1$transmission_clean <- ifelse(grepl("Manual|M/T|Mt", cars_v1$transmission, 
                                        ignore.case=TRUE), "Manual", 
                                  ifelse(grepl("Automatic|A/T|AT|CVT|Auto", 
                                               cars_v1$transmission, 
                                               ignore.case=TRUE), "Automatic", 
                                         "Other"))
cars_v1$transmission_clean <- droplevels(as.factor(cars_v1$transmission_clean))
cars_v1$clean_title <- ifelse(!is.na(cars_v1$clean_title) & cars_v1$clean_title == "Yes",
                           1, 0) # 1 for yes, 0 for no
cars_v1$clean_title <- as.factor(cars_v1$clean_title)
#too many brands --> tiers
luxury_tier <- c("Aston", "Bentley", "Ferrari", "Lamborghini", "Lotus", 
                 "Maserati", "Maybach", "McLaren", "Rolls-Royce", "Porsche")
premium_tier  <- c("Alfa", "Audi", "BMW", "Cadillac", "Genesis", "INFINITI", 
                   "Jaguar", "Land", "Lexus", "Lincoln", "Mercedes-Benz", "Volvo")
mainstream_tier    <- c("Buick", "Chevrolet", "Chrysler", "Dodge", "FIAT", 
                        "Ford", "GMC", "Honda", "Hummer", "Hyundai", "Jeep", 
                        "Kia", "Mazda", "Mercury", "MINI", "Mitsubishi", 
                        "Nissan", "Plymouth", "Pontiac", "RAM", "Saab", 
                        "Saturn", "Scion", "smart", "Subaru", "Suzuki", 
                        "Toyota", "Volkswagen")
cars_v1$brand_tier <- "Mainstream"
cars_v1$brand_tier[cars_v1$brand %in% premium_tier]  <- "Premium"
cars_v1$brand_tier[cars_v1$brand %in% luxury_tier] <- "Luxury"
cars_v1$brand_tier <- as.factor(cars_v1$brand_tier)

cars_v1$log_price <- log(cars_v1$price)
cars_v1$early_hybrid <- ifelse(cars_v1$fuel_type == "Hybrid" & 
                                 cars_v1$model_year <= 2018, 1, 0) # 1 for early hybrid, 0 for everything else
cars_v1$early_hybrid <- as.factor(cars_v1$early_hybrid)

cars_v1 <- na.omit(cars_v1)

nrow(cars_v1)

cars <- cars_v1[, c("price", "log_price", "milage", "model_year", 
                       "brand_tier", "transmission_clean", "fuel_type", 
                       "early_hybrid", "accident", "clean_title")]

# descriptive statistics  ------------------------------------------------------

summary(cars)


# numerical variables
mean(cars$price) # 38670.07
sd(cars$price) # 33770.95

mean(cars$log_price) # 10.25619
sd(cars$log_price) # 0.8012468

mean(cars$milage) # 66881.68
sd(cars$milage) # 52221.24

mean(cars$model_year) # 2015.414
sd(cars$model_year) # 5.851593


# qualitative variables

table(cars$brand_tier)
# Luxury Mainstream    Premium 
# 284       1962       1499 

table(cars$transmission_clean)
# Automatic    Manual     Other 
# 2973       359       413

table(cars$fuel_type)
# Diesel E85 Flex Fuel      Gasoline        Hybrid 
# 116           139          3262           228 

table(cars$early_hybrid)
# 0(no)    1(yes)
# 3686     59

table(cars$accident)
# Accident Reported           Missing     None reported 
# 966                         108         2671 

table(cars$clean_title)
# 0(no)    1(yes)
# 556      3189 


# visualizations  --------------------------------------------------------------

library(ggplot2)

# vis for important things -----------------------------------------------------

# price variable
ggplot(cars, aes(x = price)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(color = "red")
#right-skewed
#unimodal

# log price
ggplot(cars, aes(x = log_price)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(color = "red")
#symmetric(ish)
#unimodal

# model year variable
ggplot(cars, aes(x = model_year)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(color = "red")
#left skewed
#unimodal

# hybrid model years
ggplot(cars, aes(x = model_year)) +
  geom_bar(data = subset(cars, fuel_type == "Hybrid"))

# early vs modern hybrid prices
ggplot(cars, aes(x = ifelse(fuel_type == "Hybrid" & model_year <= 2018, "Early Hybrids",
                            ifelse(fuel_type == "Hybrid" & model_year > 2018, "Modern Hybrids", "Everything Else")), 
                 y = price)) +
  geom_boxplot()

# early vs modern hybrid prices LOG!
ggplot(cars, aes(x = ifelse(fuel_type == "Hybrid" & model_year <= 2018, "Early Hybrids",
                            ifelse(fuel_type == "Hybrid" & model_year > 2018, "Modern Hybrids", "Everything Else")), 
                 y = log_price)) +
  geom_boxplot()

# fuel type variable
ggplot(cars, aes(x = fuel_type)) +
  geom_bar()

# fuel type against prices
ggplot(cars, aes(x = fuel_type, y = price)) +
  geom_boxplot()

# vis for everything else ------------------------------------------------------

# milage variable
ggplot(cars, aes(x = milage)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(color = "red")
#right skewed
#unimodal

# transmission (clean) variable
ggplot(cars, aes(x = transmission_clean)) +
  geom_bar()

# transmission against prices
ggplot(cars, aes(x = transmission_clean, y = price)) +
  geom_boxplot()

# accident variable
ggplot(cars, aes(x = accident)) +
  geom_bar()

# accident against prices
ggplot(cars, aes(x = accident, y = price)) +
  geom_boxplot()

# brand tiers variable
ggplot(cars, aes(x = brand_tier)) +
  geom_bar()

# brand tiers against prices
ggplot(cars, aes(x = brand_tier, y = price)) +
  geom_boxplot()

# clean title variable
ggplot(cars, aes(x = clean_title)) +
  geom_bar()

# clean title against prices
ggplot(cars, aes(x = clean_title, y = price)) +
  geom_boxplot()

# correlation matrix
car_correlations <- cor(cars[, c("price", "milage", "model_year")])
print(car_correlations)
# moderate correlations


# hypothesis -------------------------------------------------------------------

# Hybrid cars manufactured before 2018 (with primitive technology) face a 
# significant negative penalty compared to other vehicles, 
# when all other variables remain unchanged (ceteris paribus)

# H0: beta_early_hybrid >= 0 
# H1: beta_early_hybrid < 0

#rooting for H1 !!


# multiple regression modeling -------------------------------------------------

# baseline model
basic_model <- lm(price ~ brand_tier + model_year + milage + fuel_type + 
                    transmission_clean + accident + clean_title + early_hybrid,
                  data = cars)
summary(basic_model)

# log model
log_model <- lm(log_price ~ brand_tier + model_year + milage + fuel_type + 
                  transmission_clean + accident + clean_title + early_hybrid, 
                data = cars)
summary(log_model)

# quadratic model
quadratic_model <- lm(log_price ~ brand_tier + model_year + milage + I(milage^2) + 
                        fuel_type + transmission_clean + accident + clean_title + early_hybrid, 
                      data = cars)
summary(quadratic_model)


# heteroskedasticity !!! -------------------------------------------------------

# breusch- pagan test
lmtest::bptest(basic_model, studentize = TRUE)
lmtest::bptest(log_model, studentize = TRUE)
lmtest::bptest(quadratic_model, studentize = TRUE)

# very heteroscedasticity !!!

# White correction, sandwich(xd) estimator
robust_basic     <- lmtest::coeftest(basic_model, vcov. = sandwich::vcovHC(basic_model, type = "HC1"))
robust_log       <- lmtest::coeftest(log_model, vcov. = sandwich::vcovHC(log_model, type = "HC1"))
robust_quadratic <- lmtest::coeftest(quadratic_model, vcov. = sandwich::vcovHC(quadratic_model, type = "HC1"))

print(robust_basic)
print(robust_log)
print(robust_quadratic)


# multicollinearity diagnostics ------------------------------------------------
# VIF (Variance Inflation Factor)
vif_results <- as.data.frame(car::vif(log_model))
vif_results$squared_VIF <- vif_results[, 3]^2
print(vif_results)
# squared vif all < 2 --> smaller than 5 and 10 --> no multicollinearity :)


# ramsey-reset tests -----------------------------------------------------------

lmtest::resettest(basic_model)
lmtest::resettest(log_model)
lmtest::resettest(quadratic_model)
# quadratic model reset test: 9 (much smaller than the others:)


# model selection criteria -----------------------------------------------------

AIC(basic_model, log_model, quadratic_model)
BIC(basic_model, log_model, quadratic_model)
# best score: quadratic model !!


# model specification ----------------------------------------------------------

specification_anova <- anova(log_model, quadratic_model)
print(specification_anova)


# final model ------------------------------------------------------------------

# slopes:
beta_milage  <- quadratic_model$coefficients["milage"]
beta_milage2 <- quadratic_model$coefficients["I(milage^2)"]

depreciation_floor_miles <- -beta_milage / (2 * beta_milage2)
round(depreciation_floor_miles, 0)

# out of sample prediction:
# gas car
hypothetical_car <- data.frame(
  brand_tier         = factor("Mainstream", levels = levels(cars$brand_tier)),
  model_year         = 2010,
  milage             = 35000,
  fuel_type          = factor("Gasoline", levels = levels(cars$fuel_type)),
  transmission_clean = factor("Automatic", levels = levels(cars$transmission_clean)),
  accident           = factor("None reported", levels = levels(cars$accident)),
  clean_title        = factor(1, levels = levels(cars$clean_title)),
  early_hybrid       = factor(0, levels = levels(cars$early_hybrid))
)

# same specs but early hybrid
hypothetical_car_2 <- data.frame(
  brand_tier         = factor("Mainstream", levels = levels(cars$brand_tier)),
  model_year         = 2010,
  milage             = 35000,
  fuel_type          = factor("Hybrid", levels = levels(cars$fuel_type)),
  transmission_clean = factor("Automatic", levels = levels(cars$transmission_clean)),
  accident           = factor("None reported", levels = levels(cars$accident)),
  clean_title        = factor(1, levels = levels(cars$clean_title)),
  early_hybrid       = factor(1, levels = levels(cars$early_hybrid))
)


predicted_log_val <- predict(quadratic_model, newdata = hypothetical_car)
predicted_usd_val <- exp(predicted_log_val)

predicted_log_val_2 <- predict(quadratic_model, newdata = hypothetical_car_2)
predicted_usd_val_2 <- exp(predicted_log_val_2)


cat("predicted gasoline car: $", 
    (predicted_usd_val), "\n")

cat("predicted hybrid car: $", 
    (predicted_usd_val_2), "\n")









