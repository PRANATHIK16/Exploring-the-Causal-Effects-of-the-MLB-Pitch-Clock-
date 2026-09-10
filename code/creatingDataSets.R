# Creating the Data
## Loading necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)

## Loading the raw data of MLB games from 2020-2024 
mlb_data <- read_csv("MLB2020-2024GameInfo.csv",
                     col_types = cols(
                       Date = col_character(),
                       HT = col_character(),
                       `Spectator Count` = col_character(),
                       `Length of Game` = col_character(),
                       .default = col_guess()
                     ))

## Process and tidy the raw data 
final_data <- mlb_data %>%
  mutate(
    # Selecting desired columns
    Date = ymd(Date),
    Year = year(Date),
    `Spectator Count` = as.numeric(str_replace_all(`Spectator Count`, "[^0-9]", "")),
    `Length of Game` = as.numeric(`Length of Game`)
  ) %>%
  filter(
    between(Year, 2021, 2024), #CHoosing year range and ignoring 2020 year
    !is.na(`Length of Game`)  
  ) %>%
  select(
    `Stadium` = 'Park ID',
    Year,
    `Spectator Count`,
    `Length of Game`
  )


## Saving the Dataset
write_csv(final_data, "mlb_games_2020-2024.csv")


# Renaming the dataset stadiums and matching codes

## Read the CSV file for ballpark data and saved final data
mlb_games <- read.csv("mlb_games_2020-2024.csv")  # Contains game data
ballparks <- read.csv("ballparks.csv")  # Contains stadiumnames and codes

# Clean column names (remove spaces and special characters)
names(mlb_games) <- gsub("[ .]", "_", names(mlb_games))
names(ballparks) <- gsub("[ .]", "_", names(ballparks))

# Merge the datasets (match Stadium to PARKID)
final_data <- mlb_games %>%
  left_join(ballparks, by = c("Stadium" = "PARKID")) %>%
  select(
    'Stadium' = NAME,
    Year,
    'Spectator Count' = Spectator_Count,
    'Duration of Game' = Length_of_Game
  )

# Save to new CSV
write.csv(final_data, "mlb_games_with_stadium_names.csv", row.names = FALSE)


# Creating Secondary Data

## Importing Secondary html

Stadiums <- read_html("http://www.stadiumdude.com/mlb-stadium-data/") %>%
  html_elements(css = "table") %>%
  html_table()

## Tidying stadium data
stadiumData <- (Stadiums[[1]]) %>%
  select("Name", "Capacity") %>% # choosing desired attributes
  rename(
    'Stadium' = 'Name', # renaming column to stadiums for coherency
  )



# Merging the Primary and Secondary Datasets

## Load required package
library(dplyr)
library(rvest) 

## Step 1: Read in the primary dataset, which contains game-level information such as stadium names, years, and spectator counts
mlb_data <- read.csv("mlb_games_with_stadium_names.csv", stringsAsFactors = FALSE)

## Step 2: Clean and standardize stadium names to ensure they match with the secondary dataset
# This includes trimming extra whitespace and updating renamed or inconsistently spelled stadiums
mlb_data_clean <- mlb_data %>%
  mutate(
    Stadium = trimws(Stadium),  # Remove leading/trailing whitespace
    Stadium = case_when(
      Stadium == "Great American Ballpark" ~ "Great American Ball Park",
      Stadium == "Marlins Park" ~ "loanDepot Park",
      Stadium == "Minute Maid Park" ~ "Daikin Park",
      Stadium == "Safeco Field" ~ "T-Mobile Park",
      Stadium == "Miller Park" ~ "American Family Field",
      Stadium == "Yankee Stadium II" ~ "Yankee Stadium",
      Stadium == "Busch Stadium III" ~ "Busch Stadium",
      Stadium == "Globe Life Field in Arlington" ~ "Globe Life Field",
      Stadium == "Angel Stadium of Anaheim" ~ "Angel Stadium",
      Stadium == "Guaranteed Rate Field;U.S. Cellular Field" ~ "Guaranteed Rate Field",
      Stadium == "Oakland-Alameda County Coliseum" ~ "Sutter Health Park",
      TRUE ~ Stadium  # Keep all other names as-is
    )
  )

## Step 3: Create the secondary dataset manually with official stadium capacities
# Includes Tropicana Field, which was missing from the public dataset but had valid games in 2024
stadium_capacity <- data.frame(
  Stadium = c(
    "Dodger Stadium", "Chase Field", "T-Mobile Park", "Coors Field", "Yankee Stadium",
    "Angel Stadium", "Oriole Park at Camden Yards", "Busch Stadium", "Great American Ball Park",
    "Citizens Bank Park", "Citi Field", "American Family Field", "Wrigley Field", "Rogers Centre",
    "Nationals Park", "Truist Park", "Comerica Park", "Daikin Park", "Oracle Park", "Target Field",
    "Petco Park", "Guaranteed Rate Field", "Globe Life Field", "PNC Park", "Kauffman Stadium",
    "Fenway Park", "loanDepot Park", "Progressive Field", "Sutter Health Park",
    "George M. Steinbrenner Field", "Tropicana Field"  # Manually added
  ),
  Capacity = c(
    56000, 48330, 47929, 46897, 46537, 45517, 44970, 44383, 43500, 42901,
    41922, 41900, 41649, 41500, 41373, 41084, 41083, 41168, 41331, 38544,
    39860, 40615, 40300, 38747, 37903, 37755, 36742, 34830, 14014, 11026,
    25025  # Manually entered capacity for Tropicana Field
  ),
  stringsAsFactors = FALSE
)

## Step 4: Perform a left join to merge the primary and secondary datasets by Stadium
# This keeps all game data and brings in matching capacity info where available
merged_data <- merge(mlb_data_clean, stadium_capacity, by = "Stadium", all.x = TRUE)

## Step 5: Calculate the spectator percentage for each game
# This is the proportion of seats filled: (Spectator Count / Capacity)
merged_data <- merged_data %>%
  mutate(Spectator_Percentage = Spectator.Count / Capacity)

## Step 6: Filter for Clean Visualization Data
# Remove rows with missing, 0%, or over 115% capacity (likely errors)
filtered_data <- merged_data %>%
  filter(
    !is.na(Spectator_Percentage),
    Spectator_Percentage > 0,
    Spectator_Percentage <= 1.15
  )
