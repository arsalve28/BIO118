#Bio 118 Laboratory Exercise 2C
#2C Using Control Structure

#2.1 Conditional Statements
#use: if (condition) {statement}
#Example
population <- 45
if (population < 50) {
  print("Population below critical level")
}
#output: [1] "Population below critical level"

#2.1.1 if statement

# Population data for three species
species_population <- c(150, 30, 75)

# Check if any species is endangered
if (any(species_population < 50)) {
  print("Endangered species detected.")
}
#Output: [1] "Endangered species detected."


#2.1.2 if-else statement
#use: if (condition) {statement_if_true} else {statement_if_false}
#Example
temperatures <- c(18, 22, 25, 30, 15)
avg_temperatures <- mean(temperatures)

if (avg_temperatures > 20) {
  print ("High risk of fire")
} else {
  print ("Low risk of fire")
}
#output: [1] "High risk of fire"


#2.2 Loops

#2.2.1 for Loop

#use: for (variable in sequence) {statements}
#example 1
for (i in 1:5) {
  print(i)
}
#output: [1] 1, [1] 2, [1] 3, [1] 4, [1] 5

#example 2

# Initial population and growth rate
initial_population <- 100
growth_rate <- 0.10

# Vector to store population sizes
plant_growth <- numeric(5)
plant_growth[1] <- initial_population

# Calculate population size for each year using a for loop
for (year in 2:5) {
  plant_growth[year] <- plant_growth[year - 1] * (1 + growth_rate)
}

# Print the population growth over 5 years
print("Plant population growth over 5 years:")

#output: [1] "Plant population growth over 5 years:"

print(plant_growth)

#output: [1] 100.00 110.00 121.00 133.10 146.41

#2.2.2 nested for loop
#example

# Vectors of species interactions
predator_prey <- c("Wolf-Deer", "Fox-Rabbit", "Eagle-Snake")
mutualistic <- c("Bee-Flower", "Clownfish-Anemone", "Fox-Rabbit")
# Use nested loops to find common species in both interactions
for (pred in predator_prey) {
  for (mut in mutualistic) {
    if (pred == mut) {
      print(paste("Species interaction found in both vectors:", pred))
    }
  }
}

#output: [1] "Species interaction found in both vectors: Fox-Rabbit"

#2.2.3 while loop
#use: while (condition) {statements}
#example
# Initial concentration and decay rate
concentration <- 80
decay_rate <- 0.15
month <- 0

# Use while loop to simulate decay until concentration drops below 10 ppm
while (concentration >= 10) {
  print(
    
    paste("Month", month, "concentration:", round(concentration, 2), "ppm"))
  concentration <- concentration * (1 - decay_rate)
  month <- month + 1
}


#2.3 Loop Control Statements

#2.3.1 break
# Create a vector of
species_population <- c(100, 90, 80, 70, 60, 50, 40, 30, 20, 10)

# Use for loop to find the first species with population below 50
for (pop in species_population) {
  if (pop < 50) {
    print(paste("First species below critical population:", pop))
    break
  }
}

#output: [1] "First species below critical population: 40"

#2.3.2 repeat 
#example
# Initial population

population <- 10
# Use repeat loop to increase population until it exceeds 100
repeat {
  print(paste("Current population:", population))
  population <- population + 10
  if (population > 100) {
    break
  }
}

#2.3.3 next

# Use for loop to print numbers 1 to 10, skipping even numbers
for (i in 1:10) {
  if (i %% 2 == 0) {
    next
  }
  print(i)
}

# Use for loop to print numbers 1 to 10, skipping even numbers
for (i in 1:10) {
  if (i %% 2 == 0) {
    next
  }
  print(i)
}


#PRACTICE

#2.4.1 Analyzing Gene Expression Data

#create data frame named gene_expression
gene_expression <- data.frame(
  Gene = c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE",
           "GeneA", "GeneB", "GeneC", "GeneD", "GeneE",
           "GeneA", "GeneB", "GeneC", "GeneD", "GeneE"),
  
  Expression_Level = c(15.3, 10.8, 12.5, 18.1, 11.7,
                       20.2, 17.6, 14.3, 19.5, 16.2,
                       13.8, 21.1, 19.7, 22.5, 20.3),
  
  Tissue = c("Tissue1", "Tissue1", "Tissue1", "Tissue1", "Tissue1",
             "Tissue2", "Tissue2", "Tissue2", "Tissue2", "Tissue2",
             "Tissue3", "Tissue3", "Tissue3", "Tissue3", "Tissue3")
)

#view data frame
gene_expression

#for loop and if statement to identify genes with an expression_level above 20
for (i in 1:15) {
  if (gene_expression [i, 2] > 20) {
    print (paste(gene_expression [i, 1],
                 gene_expression [i, 3]))
  }
}

#while loop to find the first gene with expression level below 15

#select tissue 2
tissue_2 <- gene_expression [6:10,]

#start with the first gene
i <- 1

#use while loop until expression level drops below 15
while (tissue_2 [i, 2] >= 15) {
  print(tissue_2 [i, ])
  i <- i + 1
}


#2.4.2 Monitoring Species Populations

#create a vector named species_population
species_populations <- c(150, 45, 75, 200, 30)

#for loop and if-else statement to classify species
for (i in 1:5) {
  if (species_populations[i] < 50) {
    print(paste("Species", i, "Endangered"))
  } else {
    print(paste("Species", i, "Stable"))
  }
}

#nested for loops to simulate population growth for each species
species <- c("Species1", "Species2", "Species3", "Species4", "Species5")

for (i in 1:5) {
  
  population <- species_populations[i]
  
  for (year in 1:3) {
    population <- population * 1.05
    print(paste(species[i], "Year", year, "Population:", round(population, 2)))
  }
}

#2.4.3 Monitoring Water Quality

#create a vector named pollution_levels
pollution_levels <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

#for loop, if statement, and next to skip measurements below 50
for (i in 1:11) {
  if (pollution_levels[i] < 50) {
    next
  }
  
  if (pollution_levels[i] > 50) {
    print(paste("Warning:", pollution_levels[i], "ppm"))
  }
}

#repeat loop to monitor pollution levels until critical level is detected
i <- 1

repeat {
  print(paste("Current pollution level:", pollution_levels[i], "ppm"))
  
  if (pollution_levels[i] >= 90) {
    print(paste("Critical warning:", pollution_levels[i], "ppm"))
    break
  }
  
  i <- i + 1
}


#Bio 118 Laboratory Exercise 2D
#2D User-Defined Functions

#2.1 Create and Test User-defined Functions

#2.1.1 Growth Rate of a Population
# Define the function to calculate growth rate
calculate_growth_rate <- function(initial_population, final_population) {
  growth_rate <- ((final_population - initial_population) /
                    initial_population) * 100
  return(growth_rate)
}
# Test the function
growth_rate_result <- calculate_growth_rate(100, 150)
print(paste("Growth rate is:", growth_rate_result, "%"))
#output: [1] "Growth rate is: 50 %"



#2.1.2 Returning a Result from a Function
# Define a function to check temperature suitability
check_temperature <- function(temperatures) {
  avg_temp <- mean(temperatures)
  if (avg_temp >= 10 && avg_temp <= 25) {
    return("Suitable")
  } else {
    return("Not suitable")
  }
}
# Test the function with a vector of temperatures
result <- check_temperature(c(18, 20, 15, 22, 19))

print(result)
#output: [1] "Suitable"



#2.1.3 Temperature Conversion
# Define the function to convert Celsius to Fahrenheit
celsius_to_fahrenheit <- function(temp_celsius) {
  temp_fahrenheit <- (temp_celsius * 9/5) + 32
  return(temp_fahrenheit)
}
# Test the function
temp_fahrenheit_result <- celsius_to_fahrenheit(25)
print(paste("Temperature in Fahrenheit is:", temp_fahrenheit_result))
#output: [1] "Temperature in Fahrenheit is: 77"



#2.1.4 Species Diversity Index
# Define the function to calculate Shannon diversity index
calculate_diversity_index <- function(species_counts) {
  N <- sum(species_counts)
  proportions <- species_counts / N
  diversity_index <- -sum(proportions * log(proportions))
  return(diversity_index)
}
# Test the function
species_counts <- c(10, 20, 30, 40)
diversity_index_result <- calculate_diversity_index(species_counts)
print(paste("Shannon diversity index is:", diversity_index_result))
#Output: [1] "Shannon diversity index is: 1.27985422583367"



#2.1.5 Water Quality Based on Pollution Levels
# Define the function to assess water quality
assess_water_quality <- function(pollution_levels) {
  avg_pollution <- mean(pollution_levels)
  if (avg_pollution < 30) {
    quality <- "Good"
  } else if (avg_pollution >= 30 && avg_pollution <= 70) {
    quality <- "Moderate"
  } else {
    quality <- "Poor"
  }
  return(quality)
}
# Test the function
pollution_levels <- c(25, 35, 50, 60, 80)
water_quality_result <- assess_water_quality(pollution_levels)
print(paste("Water quality is:", water_quality_result))
#output: [1] "Water quality is: Moderate"



#PRACTICE

#2.2.1 DNA Transcription

# Create a function for DNA transcription
transcribe_dna_to_rna <- function(dna_sequence) {
  gsub("T", "U", dna_sequence)
}

# Test the function
dna_sequence <- "ATGCGTAGCTAGCTAGCGT"
transcribe_dna_to_rna(dna_sequence)



#2.2.2 Predicting Future Population Size

# Define the function to predict population size
predict_population <- function(current_population, growth_rate, years) {
  future_population <- current_population * (1 + growth_rate)^years
  return(future_population)
}

# Test the function
population_result <- predict_population(1000, 0.05, 10)
print(population_result)



#2.2.3 Environmental Decay Simulation

# Define the function to simulate decay
simulate_decay <- function(initial_concentration, decay_rate) {
  concentration <- initial_concentration
  steps <- 0
  
  while (concentration >= 1) {
    concentration <- concentration * (1 - decay_rate)
    steps <- steps + 1
  }
  
  return(steps)
}

# Test the function
decay_steps <- simulate_decay(50, 0.10)
print(paste("Number of steps:", decay_steps))

