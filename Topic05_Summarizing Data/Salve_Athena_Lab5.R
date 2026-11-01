# 5.1 Load and Inspect the Dataset

data(iris)
head(iris)
str(iris)
names(iris)

# 5.2 Measures of Central Tendency

# 5.2.1 Mean

library (dplyr)

iris %>%
  group_by(Species) %>%
  summarise (
    Mean = mean(Sepal.Length)
  )

# 5.2.2 Median

iris %>% 
  group_by(Species) %>%
  summarise(
    Median = median(Sepal.Length)
  )

# 5.2.3 Mode

# create a function to calculate the mode
mode_function <- function(x) {
  frequencies <- table(x)
  names(frequencies)[which.max(frequencies)]
}

# use the function to calculate the mode of sepal length for each species
iris %>%
  group_by(Species) %>%
  summarise(
    Mode = mode_function(Sepal.Length)
  )

# 5.3 Measures of Variability

# 5.3.1 Range
iris %>%
  group_by(Species) %>%
  summarise(
    Minimum = min(Sepal.Length),
    Maximum = max(Sepal.Length),
    Range = Maximum - Minimum
  )

# 5.3.2 Variance
iris %>%
  group_by(Species) %>%
  summarise(
    Variance = var(Sepal.Length)
  )

# 5.3.3 Standard Deviation
iris %>%
  group_by(Species) %>%
  summarise(
    SD = sd(Sepal.Length)
  )

# 5.3.4 Interquartile Range (IQR)
iris %>%
  group_by(Species) %>%
  summarise(
    Q1 = quantile(Sepal.Length, 0.25),
    Q3 = quantile(Sepal.Length, 0.75),
    IQR = IQR(Sepal.Length)
  )

# 5.4 Frequence distribution

# create a frequency table
species_freq <- table(iris$Species)
species_freq

# calculate the relative frequency and percentage
# Relative frequency 
prop.table (species_freq)

# Percentage 
prop.table (species_freq) * 100

# combine counts and percentages in a single table 
iris %>% 
  count (Species, name = "Frequency") %>%
  mutate(
    Percentage = round(Frequency / sum(Frequency)*100,2)
  )

# 5.5 Measures of distribution

install.packages ("e1071")
library(e1071)

# calculate skewness and kurtosis
iris %>%
  group_by(Species) %>%
  summarise(
    Skewness = skewness(Sepal.Length),
    Kurtosis = kurtosis(Sepal.Length)
  )

# 5.6 Visualizing the data

# 5.6.1 Histogram

library(ggplot2)
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.3) +
  facet_wrap(~ Species) +
  labs(
    title = "Distribution of Sepal Length by Iris Species",
    x = "Sepal Length (cm)",
    y = "Frequency"
  )

# 5.6.2 Boxplots

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(
    title = "Sepal Length by Iris Species",
    x = "Species",
    y = "Sepal Length (cm)"
  )

# Practice Exercise: Summarizing data using the CO2 dataset 

?CO2

# 1. Load and inspect the dataset 
data(CO2)
head(CO2)
str(CO2)
names(CO2)

# 2. Central tendency

# Mean and Median of Carbon Dioxide uptake for the entire dataset

CO2 %>%
  summarise(
    mean(CO2$uptake),
    median(CO2$uptake)
  )

# Mean and Median of Carbon Dioxide uptake for each treatment
CO2 %>%
  group_by(Treatment) %>%
  summarise(
    Mean = mean(uptake),
    Median = median(uptake)
  )

# 3. Variability

# Minimum, Maximum, Range, Variance, SD, and IQR of Uptake for Each Treatment
CO2 %>% 
  group_by(Treatment) %>%
  summarise(
    Minimum = min(uptake),
    Maximum = max(uptake),
    Range = Maximum - Minimum,
    Variance = var(uptake),
    SD = sd(uptake),
    Q1 = quantile(uptake, 0.25),
    Q3 = quantile(uptake, 0.75),
    IQR = IQR(uptake)
  )

# 4. Frequency distribution

# create a frequency table
treatment_freq <- table(CO2$Treatment)
treatment_freq

# calculate the relative frequency and percentage
# Relative frequency 
prop.table (treatment_freq)

# Percentage 
prop.table (treatment_freq) * 100

# combine counts and percentages in a single table 
CO2 %>% 
  count (Treatment, name = "Frequency") %>%
  mutate(
    Percentage = round(Frequency / sum(Frequency)*100,2)
  )

# 5. Distribution Shape

# Measures of distribution

library(e1071)

CO2 %>%
  summarise(
    Skewness = skewness(uptake),
    Kurtosis = kurtosis(uptake)
  )

# Visualizing the data

ggplot(CO2, aes(x = uptake)) +
  geom_histogram(binwidth = 5) +
  labs(
    title = "Distribution of Carbon Dioxide Uptake",
    x = "Carbon dioxide uptake (µmol m⁻² s⁻¹)",
    y = "Frequency"
  )

# 5.6.2 Boxplots

ggplot(CO2, aes(x = Treatment, y = uptake)) +
  geom_boxplot() +
  labs(
    title = "Carbon Dioxide Uptake by Treatment",
    x = "Treatment",
    y = "Carbon dioxide uptake (µmol m⁻² s⁻¹)"
  )
