#2.1.1 Character Data
species_name <- "Avicennia marina"
species_upper <- toupper(species_name)
print(species_upper)
class(species_upper) #

#2.1.2 Numeric Data
water_temp <- 28.6
temp_fahrenheit <- (water_temp * 9/5) + 32
print(temp_fahrenheit) 
class(temp_fahrenheit)

#2.1.3 Integer Data
sample_size <- 50L
sqrt_sample_size <- sqrt(sample_size)
print(sqrt_sample_size) 
class(sample_size) 
class(sqrt_sample_size)

#2.1.4 Logical Data
average_temp <- 28.6
is_conservation_area <- TRUE
is_temp_high <- average_temp > 25
print(is_temp_high) 
class(is_temp_high)

#2.2.1 Arithmetic Operators
plant_growth <- 10.5
total_growth <- plant_growth * 5
print(total_growth)

#2.2.2 Relational Operators
region_A <- 3.8
region_B <- 4.2
is_region_B_richer <- region_B > region_A
print(is_region_B_richer) 

#2.2.3
is_species_X_present <- TRUE
is_species_Y_present <- TRUE
is_either_species_present <- is_species_X_present | is_species_Y_present
print(is_either_species_present) 

#PRACTICE

#2.3.1 Monitoring Species Population in Different Habitats
habitat_A_population <- 320
habitat_B_population <- 275
total_population <- habitat_A_population + habitat_B_population
is_habitat_A_larger <- habitat_A_population > habitat_B_population
population_difference <- habitat_A_population - habitat_B_population
print(paste("Habitat A Population:", habitat_A_population))
print(paste("Habitat B Population:", habitat_B_population))
print(paste("Total Population:", total_population))
print(paste("Is Habitat A larger than habitat B?", is_habitat_A_larger))
print(paste("Population Difference:", population_difference))

#2.3.2 Analyzing Environmental Data for Climate Change Impact
current_temperature <- 25
predicted_increase <- 3.8
future_temperature <- current_temperature + predicted_increase
current_temp_fahrenheit <- (current_temperature * 9/5) + 32
future_temp_fahrenheit <- (future_temperature * 9/5) + 32
exceeds_threshold <- future_temperature > 5
print(paste("Current Temperature:", current_temperature, "°C"))
print(paste("Predicted Increase:", predicted_increase, "°C"))
print(paste("Future Temperature:", future_temperature, "°C"))
print(paste("Current Temperature in Fahrenheit:", current_temp_fahrenheit, "°F"))
print(paste("Future Temperature in Fahrenheit:", future_temp_fahrenheit, "°F"))
print(paste("Does the future temperature exceeds threshold?", exceeds_threshold))

#2.3.3 Evaluating Pollutant Levels in a River
chemical_X_concentration <- 15.3
chemical_Y_concentration <- 9.8
is_chemical_X_safe <- chemical_X_concentration < 10
is_chemical_Y_safe <- chemical_Y_concentration < 10
are_both_safe <- is_chemical_X_safe & is_chemical_Y_safe
print(paste("Chemical X Concentration:", chemical_X_concentration, "ppm"))
print(paste("Chemical Y Concentration:", chemical_Y_concentration, "ppm"))
print(paste("Is Chemical X Safe?", is_chemical_X_safe))
print(paste("Is Chemical Y Safe?", is_chemical_Y_safe))
print(paste("Are Both Chemicals Safe?", are_both_safe))


