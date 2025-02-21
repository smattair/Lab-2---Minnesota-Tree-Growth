---
title: "Lab 2"
author: "Sierra Mattair"
output: 
  html_document:
    self-contained: true
---

# Question 1: Read in the dataset and check the structure

library(readr) 
trees_data <- read_csv("/Users/sierramattair/github/Lab2/tree_dat.csv") 
glimpse(trees_data)

# Description of structure:

# The dataset contains columns like 'stand', 'species', 'age', 'year', and possibly other variables.

# The 'stand' column represents the stand number (e.g., Stand 1),

# 'species' holds the species of trees (e.g., Abies balsamea),

# 'age' represents the age of the tree, and 'year' represents the year of the record.

# Question 2: How many records have been made in stand 1?
stand1_records <- trees_data %\>% 
 filter(standID == 1) 
n_stand1 <- nrow(stand1_records) 
print(n_stand1) 
# Answer: 979

# Question 3: How many records of Abies balsamea and Pinus strobus species have been made?

species_records <- trees_data %\>% 
 filter(species %in% c("ABBA", "PIST")) 
 n_species <- nrow(species_records) 
print(n_species) 
# Answer:17221

# Question 4: How many trees are older than 200 years in the last year of the dataset?
last_year <- max(trees_data$year) 
older_than_200 <- nrow(filter(trees_data, year == last_year & age > 200)) %>%
# Answer: 7

# Question 5: What is the oldest tree in the dataset found using slice_max?
oldest_tree <- trees_data %>% 
 slice_max(age, n = 1) 
print(oldest_tree)
# Answer: treeID = 24, species = Pire, age = 269

# Question 6: Find the oldest 5 trees recorded in 2001.
oldest_trees_2001 <- trees_data %>% filter(year == 2001) %>%
 slice_max(age, n = 5) 
print(oldest_trees_2001) 
# Answer: tree IDs = 24(age=263), 25(age=259), 1595(age=212), 1598(age=206), 1712(age=206)

# Question 7: Using slice_sample, how many trees are in a 30% sample of those recorded in 2002?
sampled_trees_2002 <- trees_data %>% 
 filter(year == 2002) %>%
 slice_sample(prop = 0.3) 
print(nrow(sampled_trees_2002)) 
# Answer: 687

# Question 8: Filter trees in stand 5 for 2007, sort by descending radius at breast height, and get the top 3 records from 2007
top_trees_2007 <- trees_data %>% 
 filter(standID == 5, year == 2007) %>% 
 arrange(desc(rad_ib)) %>% 
 slice_head(n = 3) 
print (top_trees_2007)
# Answer: 128, 157, 135

library(dplyr) 
# Question 9: Reduce the dataset to relevant columns and find the smallest 3 trees in stand 3, year 2007 
smallest_trees_2007 <- trees_data %>% 
 select(treeID, standID, year, rad_ib) %>%
 filter(standID == 3, year == 2007) %>% 
 slice_min(rad_ib, n = 3)
print(smallest_trees_2007) 
# Answer: 50, 56,36 (treeID)

# Question 10: Use select to remove the stand column. Use glimspe to show the dataset.
trees_data_no_stand <- trees_data %>% 
 select(-stand) 
# Remove the stand column 
glimpse(trees_data_no_stand) 
# Data set structure: 131,386 rows, 7 columns (treeID, standID, year, species, age, inc, rad_ib)

# Question 11: Identify an option (there are multiple) that would help select all columns with the string “ID” in the name. Using glimpse to view the remaining dataset
# Use dplyr::select to find an option that selects all columns containing "ID"
trees_IDs <- trees_data %>% 
select(contains("ID")) 
# Select all columns with "ID" in the name glimpse(trees_IDs) 
# View the remaining dataset 
# Answer: 131,386 rows, 2 columns (treeID, standID)

# Question 12: Select columns containing 'ID' or 'stand'
selected_columns <- trees_data %>% 
select(matches("ID|stand")) 
# Select columns with 'ID' or 'stand' in the name
glimpse(selected_columns) 
# Verify the selection: 3 columns (treeID, standID, stand)

# Question 13: Looking back at the data dictionary, rename rad_inc and inc to include _[unit] in the name.
trees_data <- trees_data %>% 
 rename(rad_ib_mm = rad_ib,inc_mm = inc)
# Verify the changes 
glimpse(trees_data) 
# New Names: inc_mm, road_ib_mm, road_ib_cm

# Question 14: Use mutate to compute DBH in centimeters, and BA in m2. What is the mean BA_m2 of the the species POTR in 2007?
trees_data <- trees_data %>% 
 mutate(DBH_cm = 2 * rad_ib_cm, BA_m2 = 0.00007854 * DBH_cm^2) 
# Filter for species "POTR" in 2007 and calculate the mean BA 
mean_BA_POTR_2007 <- trees_data %>%
 filter(species == "POTR", year == 2007) %>% 
 summarise(mean_BA = mean(BA_m2, na.rm = TRUE)) 

print(mean_BA_POTR_2007) 
# Answer: Mean BA_m2 = 3.70cm

# Question 15: Use count (see ?count) to determine how many records are from estabilshed trees?
trees_data <- trees_data %>% 
mutate(established = if_else(age > 5, TRUE, FALSE)) 
# TRUE if age \> 5, FALSE otherwise 
# Now, you can count how many records are from established trees
count_established_trees <- trees_data %>% 
count(established) 
# View the result
print(count_established_trees) 
# Answer: True = 122503, False = 8883

# Question 16: Classify trees into DBH_class and count each class in 2007
trees_data <- trees_data %>% 
 mutate(DBH_class = case_when( DBH_cm < 10 ~ "Small", DBH_cm >= 10 &  DBH_cm < 30 ~ "Medium", DBH_cm >= 30 ~ "Large" )) 
 
print(db_classes_2007) 
#Answer: Classes -\> pole (1198), sawlog(1093)

# Question 17: Compute the mean DBH (in cm) and standard deviation of DBH (in cm) for all trees in 2007. Explain the values you found and their statistical meaning
dbh_summary_2007 <- trees_data %>% 
 filter(year == 2007) %>% 
# Filter for 2007 
summarize(mean_DBH = mean(rad_ib_cm, na.rm = TRUE), 
sd_DBH = sd(rad_ib_cm, na.rm = TRUE) ) 

print(dbh_summary_2007) 
# Answer: mean_DBH = 80.5, sd_DBH = 30.7 
# Mean DBH -> avg. diameter at breast height in cm for trees in 2007. Mean shows central tendency. 
# Standard Deviation of DBH -> measure of spread of DBH values in 2007

# Question 18: Compute the per species mean tree age using only those ages recorded in 2003. Identify the three species with the oldest mean age.
mean_age_species_2003 <- trees_data %>% 
 filter(year == 2003) %>% 
 group_by(species) %>% 
 summarize(mean_age = mean(age, na.rm = TRUE)) %>%     arrange(desc(mean_age)) %>% 
 slice_head(n = 3) 
# Get the top 3 species with the oldest mean age  
print(mean_age_species_2003) 
# Answer: THOC (127), FRNI (83.1), PIST (73.3)

# Question 19: In a single summarize call, find the number of unique years with records in the data set along with the first and last year recorded?
year_summary <- trees_data %>% 
summarize( unique_years = n_distinct(year), first_year = min(year), last_year = max(year) )
print(year_summary) 
# Answer: Unique_years = 111, first_year = 1897, last_year = 2007

# Question 20: Determine the stands with the largest number of unique years recorded. Report all stands with largest (or tied with the largest) temporal record.
stands_with_largest_years <- trees_data %>% 
 group_by(stand) %>% 
# Group by stand 
 summarize(unique_years = n_distinct(year)) %>%
filter(unique_years == max(unique_years))
print(stands_with_largest_years) 
# Answer: A1, D1, D2, D3, F1

# Final Question: Use a combination of dplyr verbs to compute these values and report the 3 species with the fastest growth, and the 3 species with the slowest growth.
library(dplyr)
# Step 1: Compute the annual growth rate for each tree using lag()
trees_data <- trees_data %>% 
  group_by(treeID) %>% 
  mutate(growth_rate = rad_ib_cm - lag(rad_ib_cm)) %>% 
  ungroup () %>%
  summarize(years_recorded = n()) %>% 
  ungroup() 
# Step 2: Filter out trees with less than 10 years of growth 
trees_with_min_growth <- trees_data %>% 
  filter(years_recorded >= 10) 

print(trees_with_min_growth)  # Print results

# Step 3: Compute the total growth, average growth rate, and standard deviation of growth by species 
growth_summary <- trees_with_min_growth %>% 
  group_by(species) %>% 
  summarize(total_growth = sum (growth_rate, na.rm = TRUE), 
            avg_growth = mean (growth_rate, na.rm = TRUE), 
            sd_growth = sd (growth_rate, na.rm = TRUE), 
            .groups = "drop") 

# Step 4: Identify the 3 species with the fastest and slowest growth
# Get top 3 fastest-growing species
fastest_growth_species <- growth_summary %>%
  arrange(desc(avg_growth)) %>%
  head(3)
# Get top 3 slowest-growing species
slowest_growth_species <- growth_summary %>%
  arrange(avg_growth) %>%
  head(3)
# Print results
print("Fastest Growing Species:")
print (fastest_growth_species) 

print("Slowest Growing Species:")
print (slowest_growth_species)

# Answer: PIRE, POTR, PIBA 
# Answer: LALA, THOC, QURU

dir.create("images") 
# Red Pine "Pinus Resinosa" Image
