#paketler
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)

### IMPORT DATA ###-----

# Set working path
setwd("D:/Projects/RM_Exp1B/raw_data/")

# Initialize an empty list to store data frames
raw_dfs <- list()

# List all files in the directory with the control pattern
files <- list.files(pattern = "^[0-9]+_exp1_.*\\.csv$")

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


### CLEAN AND MERGE ###-----

# Initialize empty data frames to store combined results
data <- data.frame()



# Loop through each data frame in raw_dfs
for (y in seq_along(raw_dfs)) {
  
  df <- raw_dfs[[y]] 
  
  # Define start and end points of the main part
  start_point <- as.numeric(which(!is.na(df$key_resp_4.rt)))
  end_point <- ifelse(nrow(df) == 129, as.numeric(which(!is.na(df$text.started))) - 1, as.numeric(nrow(df)))

  
  
  df <- df[(start_point + 1):(end_point - 1), ]
  
  df <- df %>%
    select(
      participant,
      stim_length,
      amount,
      center_to_center,
      w,
      trial_type,
      loop.thisN,
      response,
      key_resp.rt,
      starting_width_deg,
      starting_space_deg,
      response_width_degree,
      response_spacing_degree,
      next_5.rt,
      resp_group
    ) %>%
    na.omit() %>%
    mutate(response_width_degree = abs(response_width_degree)) %>%
    mutate(response = as.numeric(gsub("num_", "", response))) %>%
    mutate(
      response_spacing_degree = case_when(
        response_spacing_degree == "9999" ~ NA_real_,
        as.numeric(response_spacing_degree) >= 0 ~ as.numeric(response_spacing_degree)
      )
    ) %>%
    mutate(
      starting_space_deg = case_when(
        starting_space_deg > 3000 ~ NA_real_,
        starting_space_deg < 3000 ~ starting_space_deg
      )
    ) %>%
    mutate(
      dev_amount = (response - amount),
      dev_width_deg = (response_width_degree - w),
      dev_spacing_deg = (response_spacing_degree - center_to_center)
    ) %>%
    mutate(RM = case_when(
      #IS THERE RM OR NOT VARIABLE
      dev_amount == 0 ~ "no RM",
      dev_amount < 0 ~ "RM",
      dev_amount > 0 ~ "Overestimates"
    ))
    
  
  # Combine results for each part across all iterations
  data <- bind_rows(data, df)

}

rm(df, end_point, start_point, y, raw_dfs)

col_order <- c("participant", "RM", "resp_group", "trial_type", "loop.thisN", "amount","response" , "key_resp.rt", "center_to_center", "response_spacing_degree", "starting_space_deg", "w", "response_width_degree", "starting_width_deg", "next_5.rt", "stim_length")
data <- data[, col_order]
remove(col_order)

data$loop.thisN <- data$loop.thisN + 1 #adding plus one to get rid of zeros

#renaming columns
colnames(data) <- c("subID", "RM", "keyboard_condition", "trial_type", "trial_number", "correct_num", "response_num", "response_rt", "correct_space", "response_space", "probe_space", "correct_width", "response_width", "probe_width", "adjustment_duration", "stim_length")

data$subID <- as.factor(data$subID)
data$subID <- paste("S", data$subID, sep = "") #adding S adjent to subject num

data$trial_number <- factor(data$trial_number, levels = 1:52)
data$subID <- factor(data$subID, levels = c("S701", "S702", "S703", "S704", "S705", "S706", "S707", "S708", "S709", "S710", "S711", "S712", "S713"))

#turning NA spacings as zeros
data$response_space[is.na(data$response_space)] <- 0
data$probe_space[is.na(data$probe_space)] <- 0

#adding deviation variables
data <- data %>% mutate(
  number_deviation = response_num - correct_num,
  width_deviation = response_width - correct_width,
  space_deviation = response_space - correct_space,
  response_stim_length = (response_space*(response_num-1)) + response_width,
  width_deviation_rate = correct_width/ response_width,
  space_deviation_rate = correct_space/ response_space,
  stim_length_deviation = response_stim_length - stim_length
)

### Saving ###----- 
write_xlsx(data, "exp1_df.xlsx")




