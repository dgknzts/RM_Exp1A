#paketler
library(readr)
library(dplyr)
library(tidyr)
library(writexl)



raw_dfs = c()
#load datas
for (i in 1:20) {
  file_name <- paste0(i,".csv")
  var_name <- paste0("raw_",i)
  assign(var_name, read.csv(file_name, header= TRUE))
  raw_dfs[i] = var_name
}
remove(file_name,i,var_name)


#detecting column numbers
columns <- c("age", "gender" ,"stim_length","amount", "center_to_center", "w", "trial_type", "loop.thisN", "response", "key_resp.rt", "starting_width_deg","starting_space_deg","response_width_degree","response_spacing_degree", "next_5.rt", "participant", "resp_group")
column_no <- c()
a <- 1
for (x in columns) {
  no <- which(colnames(raw_1)== x)
  column_no[a] <- no
  a <- a + 1
}
print(column_no)

#defining cleaning function
cleaning <- function(veri) {
  veri <- veri %>%
    filter(row_number() > 11) %>% #removing practice trials
    select(column_no) %>% #selecting neccesary collumns
    mutate(response_width_degree = abs(response_width_degree)) %>% #there are negative values due to adjusting to opposite ways but widths are the same 
  return(veri)
}

#applying to all raw data
for (i in 1:20) {
  raw_data <- get(paste0("raw_",i))
  new_name <- paste0("clean_",i)
  assign(new_name, cleaning(raw_data))
}
#removing unnec
remove(raw_13, raw_14, raw_15, raw_16, raw_17, raw_18, raw_19, raw_20,raw_10,raw_11,raw_12,raw_1,raw_2,raw_3,raw_4,raw_5,raw_6,raw_7,raw_8,raw_9,raw_data,raw_dfs,i,new_name)

#combining for one
data <- rbind(clean_13, clean_14, clean_15, clean_16, clean_17, clean_18, clean_19, clean_20,clean_1,clean_2,clean_3,clean_4,clean_5,clean_6,clean_7,clean_8,clean_9,clean_10,clean_11,clean_12)
#removing unneccesary
remove(clean_13, clean_14, clean_15, clean_16, clean_17, clean_18, clean_19, clean_20, clean_10,clean_11,clean_12,clean_1,clean_2,clean_3,clean_4,clean_5,clean_6,clean_7,cleaning,clean_8,clean_9)

data <- na.omit(data) #deleting NA rows


#adding new obs
#editing response as numeric.
data <- data %>%
  mutate(response = case_when(
    response == "num_1" ~ 1,
    response == "num_2" ~ 2,
    response == "num_3" ~ 3,
    response == "num_4" ~ 4,
    response == "num_5" ~ 5,
    response == "num_6" ~ 6,
    response == "num_7" ~ 7,
    response == "num_8" ~ 8,
    response == "num_9" ~ 9,
  )) %>% 
 mutate(response_spacing_degree = case_when(
    response_spacing_degree == "9999" ~ NA_real_, #this numbers exist because experiment codes non-existing line as far away from the screen
    as.numeric(response_spacing_degree) >= 0 ~ as.numeric(response_spacing_degree) #changing those to the zeros becaus there no spacing in those responses just one lines
  ))%>% 
  mutate(starting_space_deg = case_when( 
    starting_space_deg > 3000 ~NA_real_, #same reason as above. when there is one line there is no spacing.
    starting_space_deg < 3000 ~ starting_space_deg
  )) %>% 
  mutate(dev_amount = (response-amount)) %>% #deviation scores. BUT MAYBE WE SHOULD DO IT IN RATIO
  mutate(dev_width_deg = (response_width_degree - w)) %>%
  mutate(dev_spacing_deg = (response_spacing_degree - center_to_center)) %>%
  mutate(RM= case_when( #IS THERE RM OR NOT VARIABLE
    dev_amount == 0 ~ "no RM",
    dev_amount < 0 ~ "RM",
    dev_amount > 0 ~ "Overestimates"))


#removing unnec variableS
remove(a,column_no,columns,no,x)

#reordering columns
col_order <- c("participant", "RM", "resp_group", "trial_type", "loop.thisN", "amount","response" , "key_resp.rt", "center_to_center", "response_spacing_degree", "starting_space_deg", "w", "response_width_degree", "starting_width_deg", "next_5.rt", "stim_length")
data <- data[, col_order]
remove(col_order)

data$loop.thisN <- data$loop.thisN + 1 #adding plus one to get rid of zeros

#renaming columns
colnames(data) <- c("age","gender", "subID", "RM", "keyboard_condition", "trial_type", "trial_number", "correct_num", "response_num", "response_rt", "correct_space", "response_space", "probe_space", "correct_width", "response_width", "probe_width", "adjustment_duration", "stim_length")

data$subID <- as.factor(data$subID)
data$subID <- paste("S", data$subID, sep = "") #adding S adjent to subject num

data$trial_number <- factor(data$trial_number, levels = 1:54)
data$subID <- factor(data$subID, levels = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18", "S19", "S20"))

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







write.csv(data, "data.csv")
write_xlsx(data,
"data_raw.xlsx")