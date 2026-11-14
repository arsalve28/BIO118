# 6.1. Comparing Means of More than Two Groups

# 6.1.1 Load and inspect the data

# Load the dataset
data("iris")

# Inspect the data
head(iris)
str(iris)
summary(iris)

# Count the observations in each species
table(iris$Species)

# 6.1.2 Explore the data

library(ggplot2)

# Histograms by species
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(bins = 10, color = "white") +
  facet_wrap(~Species) +
  labs(
    title = "Distribution of Sepal Length by Species",
    x = "Sepal Length (cm)",
    y = "Number of Flowers"
  ) +
  theme_minimal()

# create a box plot
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(
    title = "Sepal Length Across Iris Species",
    x = "Species",
    y = "Sepal Length (cm)"
  ) +
  theme_minimal()

# calculate descriptive statistics
library(dplyr)
iris %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    mean_length = mean(Sepal.Length),
    sd_length = sd(Sepal.Length)
  )

# 6.1.4 Assess the Assumptions and Choose an Appropriate Test

# assess normality 

# create a Q-Q plot
ggplot(iris, aes(sample = Sepal.Length)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Species) +
  labs(
    title = "Normal Q-Q Plots of Sepal Length",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# perform Shapiro test on iris setosa
shapiro.test(
  iris$Sepal.Length[iris$Species == "setosa"]
)

# Shapiro test on iris versicolor
shapiro.test(
  iris$Sepal.Length[iris$Species == "versicolor"]
)

# Shapiro test on iris virginica
shapiro.test(
  iris$Sepal.Length[iris$Species == "virginica"]
)

# assess equality of variances

# perform levene’s test
library(car)
leveneTest(
  Sepal.Length ~ Species,
  data = iris
)

# Perform one-way ANOVA
anova_result <- aov(
  Sepal.Length ~ Species,
  data = iris
)

# Display the ANOVA results
summary(anova_result)

# Perform Tukey's HSD test
TukeyHSD(anova_result)

# Perform the Kruskal-Wallis test
kruskal.test(
  Sepal.Length ~ Species,
  data = iris
)

# 6.1.8 Conduct Pairwise Comparisons After Kruskal–Wallis

pairwise.wilcox.test(
  iris$Sepal.Length,
  iris$Species,
  p.adjust.method = "bonferroni"
)

# 6.2. Comparing Means Across Two Factors

# Load the dataset
data("ToothGrowth")

# Inspect the data
head(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)

# Check the number of observations
nrow(ToothGrowth)

# Count observations by supplement type and dose
table(ToothGrowth$supp, ToothGrowth$dose)

# Convert dose to a categorical variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Verify the variable types
str(ToothGrowth)

# 6.2.4 Explore the Data

# summarize the observations
library(dplyr)
ToothGrowth_summary <- ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(
    n = n(),
    mean_length = mean(len),
    sd_length = sd(len),
    .groups = "drop" # removes grouping after calculations are complete
  )

# compare mean tooth lengths
library(ggplot2)
ggplot(
  ToothGrowth,
  aes(x = dose, y = len, fill = supp)
) +
  geom_boxplot() +
  labs(
    title = "Tooth Length by Supplement Type and Dose",
    x = "Vitamin C Dose (mg/day)",
    y = "Tooth Length",
    fill = "Supplement Type"
  ) +
  theme_minimal()

# create an interaction plot
ggplot(
  ToothGrowth_summary,
  aes(
    x = dose,
    y = mean_length,
    color = supp,
    group = supp
  )
) +
  geom_line() +
  geom_point(size=3) +
  labs(
    title = "Interaction Between Supplement Type and Dose",
    x = "Vitamin C Dose (mg/day)",
    y = "Mean Tooth Length",
    color = "Supplement Type"
  ) +
  theme_minimal()

# assess the assumption

# Fit the two-way ANOVA model
anova_result <- aov(
  len ~ supp * dose,
  data = ToothGrowth
)
# Inspect the model
summary(anova_result)

# examine the residuals

# Histogram of residuals
hist(
  residuals(anova_result),
  main = "Histogram of ANOVA Residuals",
  xlab = "Residuals"
)

# Normal Q-Q plot
qqnorm(residuals(anova_result))
qqline(residuals(anova_result))

# perform Shapiro test
shapiro.test(residuals(anova_result))

# assess equality of variances

library(car)
leveneTest(
  len ~ supp * dose,
  data = ToothGrowth
)

# # Display the two-way ANOVA results
summary(anova_result)

# Compare supplement types
TukeyHSD(
  anova_result,
  which = "supp"
)

# Compare dose levels
TukeyHSD(
  anova_result,
  which = "dose"
)

# Practice Test

library(tidyverse)
library(car)

# 6.3.1 One-way comparison: ozone concentration across months
# 1a. Load and inspect the data.
data("airquality")
head(airquality)
str(airquality)
summary(airquality)

# 1b-1d. Remove missing ozone values, convert Month to a factor, and count observations.
ozone_data <- airquality |>
  filter(!is.na(Ozone))
ozone_data$Month <- as.factor(ozone_data$Month)
table(ozone_data$Month)

# 2. Explore distributions and calculate descriptive statistics.
ggplot(ozone_data, aes(x = Ozone)) +
  geom_histogram(bins = 10, color = "white") +
  facet_wrap(~Month) +
  labs(
    title = "Ozone Concentrations by Month",
    x = "Ozone concentration (ppb)",
    y = "Number of days"
  ) +
  theme_minimal()

ggplot(ozone_data, aes(x = Month, y = Ozone)) +
  geom_boxplot() +
  labs(
    title = "Ozone Concentrations Across Months",
    x = "Month",
    y = "Ozone concentration (ppb)"
  ) +
  theme_minimal()

ozone_data |>
  group_by(Month) |>
  summarise(
    n = n(),
    mean_ozone = mean(Ozone),
    sd_ozone = sd(Ozone)
  )

# 3. Hypotheses: same ozone distribution across months versus at least one difference.

# 4. Assess normality and equality of variances.
ggplot(ozone_data, aes(sample = Ozone)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Month) +
  labs(
    title = "Normal Q-Q Plots of Ozone by Month",
    x = "Theoretical quantiles",
    y = "Ozone concentration (ppb)"
  ) +
  theme_minimal()

shapiro.test(ozone_data$Ozone[ozone_data$Month == "5"])
shapiro.test(ozone_data$Ozone[ozone_data$Month == "6"])
shapiro.test(ozone_data$Ozone[ozone_data$Month == "7"])
shapiro.test(ozone_data$Ozone[ozone_data$Month == "8"])
shapiro.test(ozone_data$Ozone[ozone_data$Month == "9"])
leveneTest(Ozone ~ Month, data = ozone_data)

# 5. Conduct the Kruskal-Wallis test and Bonferroni-adjusted pairwise comparisons.
kruskal.test(Ozone ~ Month, data = ozone_data)
pairwise.wilcox.test(
  ozone_data$Ozone,
  ozone_data$Month,
  p.adjust.method = "bonferroni"
)

# 6. Interpret findings in the R Markdown file.


# 6.3.2 Two-way ANOVA: fertilizer and irrigation effects on plant growth
# 1. Generate the simulated dataset.
set.seed(123)
n <- 60

fertilizer <- rep(
  c("Fertilizer A", "Fertilizer B", "Fertilizer C"),
  each = n / 3
)

irrigation <- rep(
  rep(c("Low", "High"), each = n / 6),
  3
)

plant_growth <- rnorm(n, mean = 5, sd = 1) +
  ifelse(
    fertilizer == "Fertilizer B", 0.5,
    ifelse(fertilizer == "Fertilizer C", 1, 0)
  ) +
  ifelse(irrigation == "High", 1, 0)

plant_data <- data.frame(
  Fertilizer = fertilizer,
  Irrigation = irrigation,
  Plant_Growth = plant_growth
)

plant_data$Fertilizer <- as.factor(plant_data$Fertilizer)
plant_data$Irrigation <- as.factor(plant_data$Irrigation)

# 2. Inspect the data and count the treatment combinations.
head(plant_data)
str(plant_data)
table(plant_data$Fertilizer, plant_data$Irrigation)

# 3a. Summarize plant growth for each treatment combination.
plant_summary <- plant_data |>
  group_by(Fertilizer, Irrigation) |>
  summarise(
    n = n(),
    mean_growth = mean(Plant_Growth),
    sd_growth = sd(Plant_Growth)
  )
plant_summary

# 3b-3c. Compare distributions and create an interaction plot.
ggplot(plant_data, aes(x = Fertilizer, y = Plant_Growth, fill = Irrigation)) +
  geom_boxplot() +
  labs(
    title = "Plant Growth by Fertilizer and Irrigation",
    x = "Fertilizer type",
    y = "Plant growth (cm)",
    fill = "Irrigation"
  ) +
  theme_minimal()

ggplot(
  plant_summary,
  aes(x = Irrigation, y = mean_growth, color = Fertilizer, group = Fertilizer)
) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Interaction Between Fertilizer and Irrigation",
    x = "Irrigation level",
    y = "Mean plant growth (cm)",
    color = "Fertilizer type"
  ) +
  theme_minimal()

# 4. Hypotheses: main effect of fertilizer, main effect of irrigation, and their interaction.

# 5a. Fit the two-way ANOVA model.
anova_result <- aov(
  Plant_Growth ~ Fertilizer * Irrigation,
  data = plant_data
)
summary(anova_result)

# 5b-5e. Examine residuals and assess assumptions.
hist(
  residuals(anova_result),
  main = "Histogram of ANOVA Residuals",
  xlab = "Residuals"
)
qqnorm(residuals(anova_result))
qqline(residuals(anova_result))
shapiro.test(residuals(anova_result))
leveneTest(Plant_Growth ~ Fertilizer * Irrigation, data = plant_data)

# 6. Conduct Tukey follow-up comparisons.
TukeyHSD(anova_result, which = "Fertilizer")
TukeyHSD(anova_result, which = "Irrigation")

# 7. Interpret findings in the R Markdown file.
