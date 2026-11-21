library(abd)
library(ggplot2)

# 7.1 Correlation Analysis

# 7.1.1 Load and Inspect the Data

data("Crickets")
head(Crickets)
str(Crickets)

# 7.1.2 Explore the data
ggplot(Crickets, aes(x = sperm.viability, y = lysozyme)) +
  geom_point() +
  labs(
    title = "Sperm Viability and Lysozyme Levels in Crickets",
    x = "Sperm Viability",
    y = "Lysozyme Levels"
  ) +
  theme_minimal()

# 7.1.3 Perform correlation analysis
# Perform Pearson Correlation
cor_pearson <- cor.test(
  Crickets$sperm.viability,
  Crickets$lysozyme,
  method = "pearson"
)
cor_pearson

# Perform Spearman Correlation
cor_spearman <- cor.test(
  Crickets$sperm.viability,
  Crickets$lysozyme,
  method = "spearman",
  exact = FALSE
)
cor_spearman

# 7.2 Simple Linear Regression

# 7.2.1 Load and inspect the data
data("Rattlesnakes")

head(Rattlesnakes)
str(Rattlesnakes)
colSums(is.na(Rattlesnakes))

# 7.2.2 Explore the data
ggplot(Rattlesnakes, aes(x = meal.size, y = temp.change)) +
  geom_point() +
  labs(
    title = "Meal Size and Postprandial Temperature Change",
    x = "Meal Size",
    y = "Postprandial Temperature Change"
  ) +
  theme_minimal()

# 7.2.3 Fit a simple linear regression model
model <- lm(temp.change ~ meal.size, data = Rattlesnakes)

Rattlesnakes$fitted <- fitted(model)
Rattlesnakes$residuals <- residuals(model)

model

# 7.2.4 Examine the regression results
summary(model)

# 7.2.5 Visualize the fitted regression line
ggplot(Rattlesnakes, aes(x = meal.size, y = temp.change)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Simple Linear Regression of Meal Size and Temperature Change",
    x = "Meal Size",
    y = "Postprandial Temperature Change"
  ) +
  theme_minimal()

# 7.2.6 Examine model residuals
head(Rattlesnakes)

ggplot(Rattlesnakes, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals versus Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

plot(model, which = 2)

# 7.2.7 Predict using the regression model
range(Rattlesnakes$meal.size)

predict(model,
        newdata = data.frame(meal.size = 10),
        interval = "confidence")

predict(model,
        newdata = data.frame(meal.size = 10),
        interval = "prediction")

# 7.3 Practice Exercise: Temperature and Ozone Concentration

# 7.3.1 Load and prepare the data
data("airquality")

ozone_data <- airquality[, c("Temp", "Ozone")]
colSums(is.na(ozone_data))

ozone_data <- na.omit(ozone_data)

head(ozone_data)
str(ozone_data)

# 7.3.2 Explore the relationship
ggplot(ozone_data, aes(x = Temp, y = Ozone)) +
  geom_point() +
  labs(
    title = "Temperature and Ozone Concentration",
    x = "Temperature (°F)",
    y = "Ozone concentration (ppb)"
  ) +
  theme_minimal()

# 7.3.3 Perform correlation analysis
cor.test(
  ozone_data$Temp,
  ozone_data$Ozone,
  method = "pearson"
)

# 7.3.4 Fit a simple linear regression model
ozone_model <- lm(Ozone ~ Temp, data = ozone_data)

summary(ozone_model)

ggplot(ozone_data, aes(x = Temp, y = Ozone)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Simple Linear Regression of Temperature and Ozone Concentration",
    x = "Temperature (°F)",
    y = "Ozone concentration (ppb)"
  ) +
  theme_minimal()

# 7.3.5 Examine model residuals
ozone_data$fitted <- fitted(ozone_model)
ozone_data$residuals <- residuals(ozone_model)

ggplot(ozone_data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals versus Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

plot(ozone_model, which = 2)

# 7.3.6 Make a prediction
predict(ozone_model,
        newdata = data.frame(Temp = 80),
        interval = "prediction")
