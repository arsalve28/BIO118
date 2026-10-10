# Laboratory Exercise 4A: Practice Exercises 1–9

library(ggplot2)


# Exercise 1: Scatter plot
ggplot(
  airquality,
  aes(
    x = Wind,
    y = Ozone,
    color = factor(Month)
  )
) +
  geom_point() +
  labs(
    title = "Relationship Between Wind Speed and Ozone Concentration",
    x = "Wind Speed (mph)",
    y = "Ozone Concentration (ppb)",
    color = "Month"
  )


# Exercise 2: Line plot
ggplot(
  airquality,
  aes(
    x = Day,
    y = Ozone,
    group = factor(Month),
    color = factor(Month)
  )
) +
  geom_line() +
  labs(
    title = "Daily Variation in Ozone Concentration by Month",
    x = "Day of Month",
    y = "Ozone Concentration (ppb)",
    color = "Month"
  )


# Exercise 3: Histogram
ggplot(
  airquality,
  aes(x = Wind)
) +
  geom_histogram(
    binwidth = 2,
    fill = "lightblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Wind Speed",
    x = "Wind Speed (mph)",
    y = "Frequency"
  )


# Exercise 4: Box Plot
ggplot(
  airquality,
  aes(
    x = factor(Month),
    y = Temp,
    fill = factor(Month)
  )
) +
  geom_boxplot() +
  labs(
    title = "Monthly Distribution of Daily Maximum Temperature",
    x = "Month",
    y = "Daily Maximum Temperature (°F)",
    fill = "Month"
  ) +
  theme_classic()


# Exercise 5: Scatter plot with a linear trend line
ggplot(
  airquality,
  aes(
    x = Wind,
    y = Temp
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Wind Speed and Daily Maximum Temperature",
    x = "Wind Speed (mph)",
    y = "Daily Maximum Temperature (°F)"
  ) +
  theme_classic()


# Exercise 6: Scatter plot with separate panels for each month
ggplot(
  airquality,
  aes(
    x = Wind,
    y = Ozone
  )
) +
  geom_point() +
  facet_wrap(~ Month) +
  labs(
    title = "Relationship Between Wind Speed and Ozone Concentration by Month",
    x = "Wind Speed (mph)",
    y = "Ozone Concentration (ppb)"
  )


# Exercise 7: Calculate mean temperature for each month
mean_temp <- aggregate(
  Temp ~ Month,
  data = airquality,
  mean
)

# Display the summary table
mean_temp

# Bar plot of monthly mean temperature
ggplot(
  mean_temp,
  aes(
    x = factor(Month),
    y = Temp,
    fill = factor(Month)
  )
) +
  geom_col() +
  labs(
    title = "Monthly Mean Daily Maximum Temperature",
    x = "Month",
    y = "Mean Daily Maximum Temperature (°F)",
    fill = "Month"
  )


# Exercise 8: Heatmap
heatmap_plot <- ggplot(
  airquality,
  aes(
    x = Day,
    y = factor(Month),
    fill = Temp
  )
) +
  geom_tile() +
  labs(
    title = "Variation in Daily Maximum Temperature Across Months",
    x = "Day of Month",
    y = "Month",
    fill = "Daily Maximum\nTemperature (°F)"
  ) +
  theme_bw()

heatmap_plot

# Exercise 9: Store plot

ggsave(
  "Salve_AthenaNicole_Plot.jpeg",
  plot = heatmap_plot,
  width = 8,
  height = 5,
  dpi = 300
  
)



# Laboratory Exercise 4B

# Loading and Exploring the Dataset

data("who")
glimpse(who)
head(who)

# Examining Missing Data

colSums(is.na(who))

# Tidying the Dataset

who_tidy <- who %>%
  pivot_longer(
    cols = c(-country, -iso2, -iso3, -year),
    names_to = "profile",
    values_to = "cases"
  )
glimpse(who_tidy)

who_tidy <- who_tidy %>%
  mutate(profile = gsub("newrel_", "new_rel_", profile))

who_tidy <- who_tidy %>%
  separate(profile, into = c("new", "type", "sex_age")) %>%
  separate(sex_age, into = c("sex", "age"), sep = "(?<=m|f)")
glimpse(who_tidy)

# Defining the Data for Analysis

tb_data <- who_tidy %>%
  filter(type == "sp", !is.na(cases))
glimpse(tb_data)

# Exploring Variation and Distributions

# TB cases across years

tb_by_year <- tb_data %>%
  group_by(year) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))
print(tb_by_year, n = 33)

ggplot(tb_by_year, aes(x = year, y = total_cases)) +
  geom_line() +
  labs(
    title = "Annual Recorded New Smear-Positive Pulmonary TB Cases",
    x = "Year",
    y = "Total Recorded Cases"
  ) +
  theme_classic()

# TB Cases by Sex

tb_by_sex <- tb_data %>%
  group_by(sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))
tb_by_sex

ggplot(tb_by_sex, aes(x = sex, y = total_cases, fill = sex)) +
  geom_col() +
  labs(
    title = "Recorded New Smear-Positive Pulmonary TB Cases by Sex",
    x = "Sex",
    y = "Total Recorded Cases",
    fill = "Sex"
  ) +
  theme_classic()

# TB Cases by Age Group

tb_by_age <- tb_data %>%
  group_by(age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))
tb_by_age

ggplot(tb_by_age, aes(x = age, y = total_cases)) +
  geom_col() +
  labs(
    title = "Recorded New Smear-Positive Pulmonary TB Cases by Age Group",
    x = "Age Group",
    y = "Total Recorded Cases"
  ) +
  theme_classic()


# Bivariate Analysis

tb_year_sex <- tb_data %>%
  group_by(year, sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))
glimpse(tb_year_sex)

ggplot(tb_year_sex, aes(x = year, y = total_cases, color = sex)) +
  geom_line() +
  labs(
    title = "Annual Recorded New Smear-Positive Pulmonary TB Cases by Sex",
    x = "Year",
    y = "Total Recorded Cases",
    color = "Sex"
  ) +
  theme_classic()

# Multivariable Exploration

tb_year_age_sex <- tb_data %>%
  group_by(year, age, sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))
glimpse(tb_year_age_sex)

ggplot(
  tb_year_age_sex,
  aes(x = year, y = total_cases, color = sex)
) +
  geom_point() +
  facet_wrap(~ age) +
  labs(
    title = "Annual Recorded New Smear-Positive Pulmonary TB Cases by Age Group and Sex",
    x = "Year",
    y = "Total Recorded Cases",
    color = "Sex"
  ) +
  theme_classic()

# Investigating Unusual Observations

ggplot(tb_data, aes(x = "", y = cases)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Recorded New Smear-Positive Pulmonary TB Case Counts",
    x = NULL,
    y = "Recorded Cases per Country-Year-Sex-Age Group"
  ) +
  theme_classic()

# Practice Exercise

glimpse(airquality)
head(airquality)
colSums(is.na(airquality))

