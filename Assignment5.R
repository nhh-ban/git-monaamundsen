######## Problem 2 ########
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)

# Loading the file 
raw_file <- readLines("suites_dw_Table1.txt")

# Using the substr() function to extract the first two letters of each line
substr(x = raw_file, start = 1, stop = 2)

# Finding out wich line starts with "--" 
L <- 
  (substr(x = raw_file, start = 1, stop = 2) == "--") |> 
  #function that returns the index of all TRUES
  which() |>
  #function that picks out the minimum value
  min()

#this vector returns 14, meaning that the first
#line starting with "--" is line number 14:
L

#We can also see the line by indexing the raw file with the number we found:
raw_file[14]

#Saving the variable descriptions in a txt.file I called "variable_descriptions"
cat(raw_file[1:(L-2)], sep = "\n", file = "variable_descriptions.txt")

#Extract the variable names
variable_names <- 
  # using str_split to split the string by "|"
  str_split(string = raw_file[L-1], pattern = "\\|", n = Inf, simplify = TRUE) |>
  unlist() |>
  # using str_trim() to get rid of all the empty space 
  str_trim()

#the variable_names vector contains all the variable names 
variable_names

# Read the data
comma_separated_values <- 
  # taking the elements of raw_file after the first "--" 
  raw_file[-(1:L)] %>% 
  # replacing all "|" with ","
  gsub("\\|", ",", .) %>% 
  # removing all empty space 
  gsub(" ", "", .)

#Adding the variable names 
comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)  

#Saving the data in a .csv file I called "galaxies.csv"
cat(comma_separated_values_with_names, sep = "\n", file = "galaxies.csv")

#And lastly reading the csv file into R as a data frame 
galaxies <- read_csv("galaxies.csv")

######## Problem 3 ########
galaxies |>
  ggplot(aes(x = a_26)) +
  geom_histogram(fill = "blue", color = "white")

######## Problem 4 ########
#1
# Loading the file 
raw_file2 <- readLines("UCNG_table4.txt")

# Using the substr() function to extract the first two letters of each line
substr(x = raw_file2, start = 1, stop = 2)

# Finding out wich line starts with "--" 
L2 <- 
  (substr(x = raw_file2, start = 1, stop = 2) == "--") |> 
  #function that returns the index of all TRUES
  which() |>
  #function that picks out the minimum value
  min()

#Extract the variable names
variable_names2 <- 
  # using str_split to split the string by "|"
  str_split(string = raw_file2[L2-1], pattern = "\\|", n = Inf, simplify = TRUE) |>
  unlist() |>
  # using str_trim() to get rid of all the empty space 
  str_trim()

comma_separated_values2 <- 
  # taking the elements of raw_file after the first "--" 
  raw_file2[-(1:L2)] %>% 
  # replacing all "|" with ","
  gsub("\\|", ",", .) %>% 
  # removing all empty space 
  gsub(" ", "", .)

comma_separated_values_with_names2 <- 
  c(paste(variable_names2, collapse = ","),
    comma_separated_values2)  

#Saving the data in a .csv file I called "galaxies.csv"
cat(comma_separated_values_with_names2, sep = "\n", file = "expansion.csv")

#And lastly reading the csv file into R as a data frame 
expansion <- read_csv("expansion.csv")

joined_data <- inner_join(expansion, galaxies, by = "name")

joined_data |>
  ggplot(aes(x = cz, y = D)) +
  geom_point() +
  geom_smooth(method = "lm")
  theme_bw()
  
#2
model <- lm(cz ~ D, data = joined_data)
summary(model)  

hubble_constant <- coef(model)["D"]
hubble_constant

paste0("My estimated Hubble's constant is ", round(hubble_constant, digits = 2))

######## Problem 5 ########


