
# R basics

# In this chapter we will introduce to the R basics and some exercises to get familiar to how R works.

## Math operations

# Sum

1+1

# Subtraction

5-2

# Multiplication

2*2

# Division

8/2

# Round the number

round(3.14)
round(3.14, 1) # The "1" indicates to round it up to 1 decimal digit.

# You can use help `?round` in the console to see the description of the function, and the default arguments.

## Basic shortpaths

### Perform Combinations

c(1, 2, 3)
c(1:3) # The ":" indicates a range between the first and second numbers. 

#Try to write a combination with the number 10, 11, 56, 57,58

#...


### Create a comment with `ctrl + shift + m`


# Comments help you organize your code. The software will not run the comment. 


### Create a table

# A simple table with the number of trips by car, PT, walking, and cycling in a hypothetical street segment at a certain period.

# Define variables

modes <- c("car", "PT", "walking", "cycling") # you can use "=" or "<-"
Trips = c(200, 50, 300, 150) # uppercase letters modify

# Join the variables to create a table

table_example = data.frame(modes, Trips)

# Take a look at the table

# Visualize the table by clicking on the "Data" in the "Environment" page or use :

View(table_example)

# dataset[row, column]
# Look at the first row 

table_example[1,] #rows and columns start from 1 in R, differently from Python which starts from 0.

# Look at first row and column

table_example[1,1]

#try to find the element "300"

#...

# Remove the first row

table_example = table_example[-1,] #first column

## Practical exercise

# Dataset: the number of trips between all municipalities in the Lisbon Metropolitan Area, Portugal [@IMOB].

### Import dataset

# You can click directly in the file under the "Files" pan, or:

table_raw = readRDS("data/TRIPSmode.Rds")

# It is good practice to not use the original database

table_trips = table_raw

# After you type " you can use `tab` to navigate between folders and files and `enter` to autocomplete.

### Take a first look at the data

# Summary statistics

summary(table_trips)

# Check the structure of the data

str(table_trips)

table_trips

head(table_trips, 3) # first 3 values

# Check the number of rows (observations) and columns (variables)

nrow(table_trips)
ncol(table_trips)

# Open the dataset

View(table_trips)

### Explore the table_trips

# Check the total number of trips

# Use `$` to select a variable of the table_trips

sum(table_trips$Total)

#...

# Percentage of car trips related to the total


sum(table_trips$Car)/sum(table_trips$Total) * 100


# Percentage of active trips related to the total


(sum(table_trips$Walk) + sum(table_trips$Bike)) / sum(table_trips$Total) * 100


### Modify original table_trips

# Create a column with the sum of the number of trips for active modes


table_trips$Active = table_trips$Walk + table_trips$Bike

# Create a variable of motorized trips to add the sum of trips by "PT" and "car"

#...

# Filter by condition (create new tables)

# Filter trips only with origin from Lisbon


trips_Lisbon = table_trips[table_trips$Origin == "Lisboa",]


# Filter trips with origin different from Lisbon


trips_out_Lisbon = table_trips[table_trips$Origin != "Lisboa",]


# Filter trips with origin and destination in Lisbon


trips_in_Out_Lisbon = table_trips[table_trips$Origin == "Lisboa" & table_trips$Destination == "Lisboa",]


# Create a table only with origin, destination and walking trips

# There are many ways to do the same operation.


names(table_trips)

# Create a table only with origin,destination, and Walk

trips_walk = table_trips[ ,c(1,2,4)]

trips_walk2 = table_trips[ ,-c(3,5:9)]

# create a table with trips from car and PT...

#...

### Export data

# Save data in .csv and .Rds

# write.csv(table_trips, 'Data/dataset_basic.csv') # , row.names = FALSE
# saveRDS(table_trips, 'Data/dataset_basic_2.Rds') #Choose a different file. 


### Import data

# csv_file = read.csv("Data/dataset_basic.csv")
# rds_file = readRDS("Data/dataset_basic_2.Rds")

