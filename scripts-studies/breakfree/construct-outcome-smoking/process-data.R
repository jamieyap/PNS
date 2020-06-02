library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.breakfree.cc1.input_data <- Sys.getenv("path.breakfree.cc1.input_data")
path.breakfree.cc2.input_data <- Sys.getenv("path.breakfree.cc2.input_data")
path.breakfree.staged_data <- Sys.getenv("path.breakfree.staged_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.breakfree.code, "construct-outcome-smoking/read-data.R"))

df <- df %>% 
  group_by(user.id) %>%
  do(GetPastRecords(df.this.group = ., cols.today = "prompt.unixts", h=1, this.numeric = TRUE)) %>%
  rename(previous.prompts.unixts = prompt.unixts_shift.minus.1) %>%
  mutate(time.between.hours = (prompt.unixts-previous.prompts.unixts)/(60*60)) %>%
  select(-previous.prompts.unixts)

# Create smoking outcome variables
df <- df %>%
  mutate(item.40 = as.numeric(item.40)) %>%
  mutate(smoking.qty = NA_real_, 
         smoking.delta.hours.recentcig.text = NA_character_, smoking.delta.hours.firstcig.text = NA_character_,
         smoking.delta.hours.recentcig = NA_real_, smoking.delta.hours.firstcig = NA_real_) %>%
  mutate(smoking.qty = case_when(
    item.39=="No" ~ 0,
    item.39=="Yes" ~ item.40,
    TRUE ~ smoking.qty
  )) %>%
  mutate(smoking.delta.hours.recentcig.text = case_when(
    item.39=="Yes" & item.40==1 ~ item.41,
    item.39=="Yes" & item.40>1 ~ item.42,
    TRUE ~ smoking.delta.hours.recentcig.text
  )) %>%
  mutate(smoking.delta.hours.firstcig.text = case_when(
    item.39=="Yes" & item.40==1 ~ item.41,
    item.39=="Yes" & item.40>1 ~ item.43,
    TRUE ~ smoking.delta.hours.firstcig.text
  )) %>%
  mutate(smoking.delta.hours.recentcig = case_when(
    smoking.delta.hours.recentcig.text=="0 - 2 hrs" ~ 0,
    smoking.delta.hours.recentcig.text=="2 hrs - 4 hrs" ~ 2,
    smoking.delta.hours.recentcig.text=="4 hrs - 6 hrs" ~ 4,
    smoking.delta.hours.recentcig.text=="6 hrs - 8 hrs" ~ 6,
    smoking.delta.hours.recentcig.text=="8 hrs - 10 hrs" ~ 8,
    smoking.delta.hours.recentcig.text=="10 hrs - 12 hrs" ~ 10,
    smoking.delta.hours.recentcig.text=="More than 12 hrs" ~ 12,
    TRUE ~ smoking.delta.hours.recentcig
  )) %>%
  mutate(smoking.delta.hours.firstcig = case_when(
    smoking.delta.hours.firstcig.text=="0 - 2 hrs" ~ 2,
    smoking.delta.hours.firstcig.text=="2 hrs - 4 hrs" ~ 4,
    smoking.delta.hours.firstcig.text=="4 hrs - 6 hrs" ~ 6,
    smoking.delta.hours.firstcig.text=="6 hrs - 8 hrs" ~ 8,
    smoking.delta.hours.firstcig.text=="8 hrs - 10 hrs" ~ 10,
    smoking.delta.hours.firstcig.text=="10 hrs - 12 hrs" ~ 12,
    smoking.delta.hours.firstcig.text=="More than 12 hrs" ~ time.between.hours,
    TRUE ~ smoking.delta.hours.firstcig
  )) %>%
  select(-smoking.delta.hours.recentcig.text, -smoking.delta.hours.firstcig.text)


