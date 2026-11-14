# Comparing one sample to a Reference Value

# 6.1.1 Load and Inspect the Data 

# examine the dataset structure

data("trees")

head(trees)
str(trees)
summary(trees$Height)

# check missing values or unusual observations
sum(is.na(trees$Height))

# 6.1.2 Explore the Data

# use a histogram / boxplot to examine distribution of tree heights

library(tidyverse)

# create a histogram

ggplot(trees, aes(x = Height)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Distribution of Black Cherry Tree Heights",
    x = "Height (feet)",
    y = "Number of Trees"
  ) +
  theme_minimal()

# create a boxplot
ggplot(trees, aes(x="", y = Height)) +
  geom_boxplot () +
  labs(
    title = "Black Cherry Tree Heights",
    x = NULL,
    y = "Height (feet)"
  ) +
  theme_minimal()

# 6.1.3 Formulate the Hypotheses

# create a Q-Q plot
ggplot(trees, aes(sample = Height)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Tree Heights",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# perform Shapiro Test
shapiro.test (trees$Height)

# perform the one-sample t-test
# perform t-test
t.test (trees$Height, mu =  75)
 
# 6.1.7 Perform One-Sample Wilcoxon Signed-Rank Test

tree_sample <- data.frame(
  Tree_ID = 1:20,
  Height = c(
    35, 36, 37, 38, 39,
    40, 41, 42, 43, 44,
    52, 53, 54, 55, 56,
    57, 58, 59, 60, 61
  )
)
tree_sample

# exlore the data

# create a histogram
ggplot(tree_sample, aes(x = Height)) +
  geom_histogram(bins = 10, color = "white") +
  labs(
    title = "Distribution of Simulated Tree Heights",
    
    x = "Tree Height (feet)",
    y = "Number of Trees"
  ) +
  theme_minimal()

# create a box plot
ggplot(tree_sample, aes(x = "", y = Height)) +
  geom_boxplot() +
  labs(
    title = "Simulated Tree Heights",
    x = NULL,
    y = "Tree Height (feet)"
  ) +
  theme_minimal()

# Assess normality
# Q-Q plot
ggplot(tree_sample, aes(sample = Height)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Simulated Tree Heights",
    
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# shapiro test
shapiro.test(tree_sample$Height)

# Perform the One-Sample Wilcoxon Signed-Rank Test

wilcox.test(
  tree_sample$Height,
  mu = 45,
  alternative = "two.sided",
  conf.int = TRUE
)


# 6.2 Comparing Paired Samples

# 6.2.2 Load and Inspect the Data 
data("sleep")
head(sleep)
str(sleep)

# 6.2.4 Prepare the Paired Data

# reshape to wide format
library(tidyverse)
sleep_wide <- sleep %>%
  pivot_wider(
    names_from = group,
    values_from = extra,
    
    names_prefix = "drug"
  )
head(sleep_wide)

# calculate the within-subject difference
sleep_wide <- sleep_wide %>%
  mutate(difference = drug1 - drug2)
head(sleep_wide)

# 6.2.5 Explore the Differences

ggplot(sleep_wide, aes(x = difference)) +
  geom_histogram(bins = 6, color = "white") +
  labs(
    title = "Distribution of Paired Differences in Sleep Increase",
    x = "Difference (Drug 1 - Drug 2, hours)",
    y = "Number of Subjects"
  ) +
  theme_minimal()

# shapiro test
shapiro.test(sleep_wide$difference)

# Perform the Paired t-test

t.test(
  sleep_wide$drug1,
  sleep_wide$drug2,
  paired = TRUE
)

# 6.2.7 Perform the Wilcoxon Signed-Ran Test

wilcox.test(
  sleep_wide$drug1,
  sleep_wide$drug2,
  paired = TRUE,
  alternative = "two.sided"
)


# 6.3 Comparing Means of Two Independent Groups

# 6.3.2 Load and inspect the data
data("iris")
head(iris)
str(iris)

# 6.3.3 Prepare the data

data_subset <- iris %>%
  filter(Species %in% c("setosa", "versicolor"))
head(data_subset)

# remove unused factor levels
data_subset <- droplevels(data_subset)

# check the number of observations in each group
table(data_subset$Species)


# 6.3.5 Explore the data

# create a histogram
ggplot(data_subset, aes(x = Sepal.Length)) +
  geom_histogram(bins = 10, color = "white") +
  facet_wrap(~Species) +
  labs(
    title = "Distribution of Sepal Length by Species",
    x = "Sepal Length (cm)",
    y = "Number of Flowers"
  ) +
  theme_minimal()

# create a boxplot
ggplot(data_subset, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Sepal Length Between Species",
    x = "Species",
    y = "Sepal Length (cm)"
  ) +
  theme_minimal()

# calculate the mean and standard deviation for each group
data_subset %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    mean_length = mean(Sepal.Length),
    sd_length = sd(Sepal.Length)
  )

# 6.3.6 Assess the assumptions

# create Q-Q plot
ggplot(data_subset, aes(sample = Sepal.Length)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Species) +
  labs(
    title = "Normal Q-Q Plots of Sepal Length by Species",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Use shapiro test
shapiro.test(
  data_subset$Sepal.Length[data_subset$Species == "setosa"]
)

shapiro.test(
  data_subset$Sepal.Length[data_subset$Species == "versicolor"]
)

# examine variances using levene’s test
library(car)
leveneTest(
  Sepal.Length ~ Species,
  data = data_subset
)

# 6.3.7 Perform Welch's Two-Sample t-Test

# Welch’s t-test
t.test(
  Sepal.Length ~ Species,
  data = data_subset
)

# 6.3.8 Demonstrate the Mann–Whitney U Test

wilcox.test(
  Sepal.Length ~ Species,
  data = data_subset,
  alternative = "two.sided"
)

# Practice Test

library(tidyverse)

# 6.4.1 One-sample comparison: ozone concentration
# 1a-1b. Load and inspect the data.
data("airquality")
head(airquality)
str(airquality)
summary(airquality$Ozone)

# 1c. Count missing and recorded ozone measurements.
sum(is.na(airquality$Ozone))
sum(!is.na(airquality$Ozone))

# 1d-1e. Keep the recorded measurements and check the retained data.
ozone_data <- airquality[!is.na(airquality$Ozone), ]
table(is.na(ozone_data$Ozone))
summary(ozone_data$Ozone)

# 2. Explore the distribution.
ggplot(ozone_data, aes(x = Ozone)) +
  geom_histogram(bins = 10, color = "white") +
  labs(
    title = "Daily Ozone Concentrations",
    x = "Ozone concentration (ppb)",
    y = "Number of days"
  ) +
  theme_minimal()

ggplot(ozone_data, aes(x = "", y = Ozone)) +
  geom_boxplot() +
  labs(
    title = "Ozone Concentrations",
    x = NULL,
    y = "Ozone concentration (ppb)"
  ) +
  theme_minimal()

# 3. Hypotheses: population mean = 30 ppb versus population mean != 30 ppb.

# 4. Assess normality.
ggplot(ozone_data, aes(sample = Ozone)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Ozone Concentrations",
    x = "Theoretical quantiles",
    y = "Ozone concentration (ppb)"
  ) +
  theme_minimal()
shapiro.test(ozone_data$Ozone)

# 5. Conduct the one-sample Wilcoxon test and t-test.
wilcox.test(ozone_data$Ozone, mu = 30, alternative = "two.sided")
t.test(ozone_data$Ozone, mu = 30, alternative = "two.sided")

# 6. Interpret the findings in the R Markdown file.


# 6.4.2 Paired comparison: chick weights at Day 10 and Day 21
# 1. Load and inspect the data.
data("ChickWeight")
head(ChickWeight)
str(ChickWeight)
summary(ChickWeight)

# 2a-2e. Create pairs, remove incomplete pairs, and calculate differences.
chick_wide <- ChickWeight |>
  filter(Time %in% c(10, 21)) |>
  pivot_wider(
    names_from = Time,
    values_from = weight,
    names_prefix = "day"
  )

chick_complete <- chick_wide |>
  filter(!is.na(day10), !is.na(day21)) |>
  mutate(difference = day21 - day10)

head(chick_wide)
head(chick_complete)
chick_wide |> summarise(total_chicks = n())
chick_complete |> summarise(
  complete_pairs = n(),
  mean_difference = mean(difference),
  sd_difference = sd(difference)
)

# 3. Explore the paired differences.
ggplot(chick_complete, aes(x = difference)) +
  geom_histogram(bins = 10, color = "white") +
  labs(
    title = "Differences in Chick Weight",
    x = "Day 21 weight minus Day 10 weight (g)",
    y = "Number of chicks"
  ) +
  theme_minimal()

# 4. Hypotheses: mean difference = 0 versus mean difference != 0.

# 5. Assess the distribution of paired differences.
ggplot(chick_complete, aes(sample = difference)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Paired Differences",
    x = "Theoretical quantiles",
    y = "Weight difference (g)"
  ) +
  theme_minimal()
shapiro.test(chick_complete$difference)

# 6. Conduct the paired t-test and Wilcoxon signed-rank test.
t.test(chick_complete$day21, chick_complete$day10, paired = TRUE)
wilcox.test(chick_complete$day21, chick_complete$day10, paired = TRUE)

# 7. Interpret the findings in the R Markdown file.


# 6.4.3 Independent-group comparison: control and treatment 1
# 1. Load and inspect the data.
data("PlantGrowth")
head(PlantGrowth)
str(PlantGrowth)
summary(PlantGrowth)

# 2. Select control and treatment 1 and check the group counts.
plant_two <- PlantGrowth |>
  filter(group %in% c("ctrl", "trt1")) |>
  droplevels()
table(plant_two$group)

# 3. Hypotheses: equal population means versus different population means.

# 4. Compare the groups and calculate summaries.
ggplot(plant_two, aes(x = group, y = weight)) +
  geom_boxplot() +
  labs(
    title = "Plant Dry Weight by Group",
    x = "Group",
    y = "Plant dry weight (g)"
  ) +
  theme_minimal()

plant_two |>
  group_by(group) |>
  summarise(
    n = n(),
    mean_weight = mean(weight),
    sd_weight = sd(weight)
  )

# 5. Assess normality within each group.
ggplot(plant_two, aes(sample = weight)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~group) +
  labs(
    title = "Normal Q-Q Plots of Plant Weight by Group",
    x = "Theoretical quantiles",
    y = "Plant dry weight (g)"
  ) +
  theme_minimal()
shapiro.test(plant_two$weight[plant_two$group == "ctrl"])
shapiro.test(plant_two$weight[plant_two$group == "trt1"])

# 6. Conduct Welch's two-sample t-test.
t.test(weight ~ group, data = plant_two)

# 7. Interpret the findings in the R Markdown file.


