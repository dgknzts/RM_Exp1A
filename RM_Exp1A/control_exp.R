#paketler
library(readr)
library(dplyr)
library(tidyr)
library(writexl)

### IMPORT DATA ###-----

# Set working path
setwd("D:/Projects/RM_Exp1A/raw_control_data/")

# Initialize an empty list to store data frames
raw_dfs <- list()

# List all files in the directory with the control pattern
files <- list.files(pattern = "^[0-9]+_control_.*\\.csv$")

# Load data files
for (i in seq_along(files)) {
  file_name <- files[i]
  # Read the CSV file and store it in the list
  raw_dfs[[i]] <- read.csv(file_name, header = TRUE)
  # Assign names to each data frame in the list for easier access
  names(raw_dfs)[i] <- file_name
}

# remove temporary variables
rm(files, file_name, i)


### SPLIT CONTROL EXP1 AND EXP2 AND MERGE PARTICIPANTS ###-----

# Initialize empty data frames to store combined results
control_exp1 <- data.frame()
control_exp2 <- data.frame()

# Loop through each data frame in raw_dfs
for (y in seq_along(raw_dfs)) {
  
  df <- raw_dfs[[y]]
  
  # Define start, split, and end points based on responen on breaks
  start_point <- as.numeric(which(!is.na(df$key_resp_2.rt)))
  split_point <- as.numeric(which(!is.na(df$key_resp_3.rt)))
  end_point <- ifelse(nrow(df) == 129, as.numeric(which(!is.na(df$text.started))) - 1, nrow(df))
  
  # Extract part_1
  part_1 <- df[(start_point + 1):(split_point - 1), ] %>% 
    select(participant, posx, w, starting_width_deg, response_width_degree, next.rt) %>%
    mutate(width_deviation = response_width_degree - starting_width_deg)
  
  # Extract part_2
  part_2 <- df[(split_point + 1):(end_point), ] %>% 
    select(participant, w, amount, posx1, posx2, eccentricity, setsize_cobdi, key_resp.keys) %>%
    mutate(key_resp.keys = as.numeric(gsub("num_", "", key_resp.keys)),
           number_deviation = key_resp.keys - amount)
  
  # Combine results for each part across all iterations
  control_exp1 <- bind_rows(control_exp1, part_1)
  control_exp2 <- bind_rows(control_exp2, part_2)
}

rm(df, end_point, part_1, part_2, raw_dfs, split_point, start_point, y)

### Saving ###----- 
write_xlsx(control_exp1, "control_experiment_width.xlsx")
write_xlsx(control_exp2, "control_experiment_number.xlsx")

### Participant performances ###-----

control_exp1 <- readxl::read_xlsx('control_experiment_width.xlsx')
control_exp2 <- readxl::read_xlsx('control_experiment_number.xlsx')

perf_table_1 <- control_exp1 %>%
  group_by(participant) %>%
  summarize(mean_deviation = mean(width_deviation),
            sd_deviation = sd(width_deviation))

perf_table_2 <- control_exp2 %>%
  group_by(participant) %>%
  summarize(mean_deviation = mean(number_deviation),
            sd_deviation = sd(number_deviation))



