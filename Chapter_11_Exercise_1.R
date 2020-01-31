# Exercise 1: working with data frames (review)

# Install devtools package: allows installations from GitHub
install.packages("devtools")

# Install "fueleconomy" dataset from GitHub
devtools::install_github("hadley/fueleconomy")

# Use the `libary()` function to load the "fueleconomy" package
library(fueleconomy)

# You should now have access to the `vehicles` data frame
# You can use `View()` to inspect it
View(vehicles)

# Select the different manufacturers (makes) of the cars in this data set. 
# Save this vector in a variable
manufacturers <- vehicles[, "make"]

# Use the `unique()` function to determine how many different car manufacturers
# are represented by the data set
unique(manufacturers)

# Filter the data set for vehicles manufactured in 1997
year_filter <- vehicles$year == 1997
vehicles_1997 <- vehicles[year_filter, ]

# Arrange the 1997 cars by highway (`hwy`) gas milage
# Hint: use the `order()` function to get a vector of indices in order by value
# See also:
# https://www.r-bloggers.com/r-sorting-a-data-frame-by-the-contents-of-a-column/
indices <- order(vehicles$hwy)
vehicles_sorted <- vehicles[indices, ]
View(vehicles_sorted)

# Mutate the 1997 cars data frame to add a column `average` that has the average
# gas milage (between city and highway mpg) for each car
temp <- (vehicles_1997$hwy + vehicles_1997$cty) / 2
vehicles_1997$average <- temp
View(vehicles_1997)

# Filter the whole vehicles data set for 2-Wheel Drive vehicles that get more
# than 20 miles/gallon in the city. 
# Save this new data frame in a variable.
filter1 <- vehicles$drive == "2-Wheel Drive"
filter2 <- vehicles$hwy < 20
filter3 <- filter1 == TRUE & filter2 == TRUE
filtered <- vehicles[filter3, ]

# Of the above vehicles, what is the vehicle ID of the vehicle with the worst 
# hwy mpg?
# Hint: filter for the worst vehicle, then select its ID.
filter <- filtered$hwy == min(filtered$hwy)
filtered[filter, "id"]

# Write a function that takes a `year_choice` and a `make_choice` as parameters, 
# and returns the vehicle model that gets the most hwy miles/gallon of vehicles 
# of that make in that year.
# You'll need to filter more (and do some selecting)!
mostEfficient <- function(year_choice, make_choice) {
  filter <- vehicles$year == year_choice & vehicles$make == make_choice
  filtered <- vehicles[filter, ]
  filter2 <- filtered$hwy == max(filtered$hwy)
  return(filtered[filter2, "model"])
}

# What was the most efficient Honda model of 1995?
mostEfficient(1995, "Honda")
