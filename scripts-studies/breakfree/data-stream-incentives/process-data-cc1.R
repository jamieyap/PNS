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
load(file.path(path.breakfree.staged_data, "raw.incentive.cc1.RData"))

# Select relevant columns and ensure columns are of correct type
for(i in 1:length(list.df.raw.incentive.cc1)){
  this.df <- list.df.raw.incentive.cc1[[i]]
  tot.cols <- ncol(this.df)
  
  if(tot.cols==11){
    this.df <- this.df %>% 
      select(user.id, 
             time.unixts = V1, 
             incentive.now = V5,
             incentive.cumulative = V9,
             ema.type = V8)
  }else{ # tot.cols==10
    this.df <- this.df %>% 
      select(user.id, 
             time.unixts = V1, 
             incentive.now = V4, 
             incentive.cumulative = V8, 
             ema.type = V7)
  }
  
  this.df <- this.df  %>%
    mutate(user.id = as.character(user.id),
           time.unixts = as.numeric(time.unixts),
           incentive.now = as.character(incentive.now),
           incentive.cumulative = as.character(incentive.cumulative),
           ema.type = as.character(ema.type)) 
  
  list.df.raw.incentive.cc1[[i]] <- this.df
}

# Create data frame from list
df.raw.incentive.cc1 <- bind_rows(list.df.raw.incentive.cc1)

# Clean up columns
df.raw.incentive.cc1 <- df.raw.incentive.cc1  %>%
  mutate(incentive.now = substring(incentive.now, first=12),
         incentive.cumulative = substring(incentive.cumulative, first=17),
         ema.type = substring(ema.type, first=9)) %>%
  mutate(incentive.now = as.numeric(incentive.now),
         incentive.cumulative = as.numeric(incentive.cumulative)) %>%
  mutate(ema.type = if_else(ema.type == "STRESS_EMA", "stress", ema.type)) %>%
  mutate(ema.type = if_else(ema.type == "SMOKING_EMA", "smoking", ema.type)) %>%
  mutate(ema.type = if_else(ema.type == "RANDOM_EMA", "random", ema.type)) 

# Clean up time variable
df.raw.incentive.cc1 <- df.raw.incentive.cc1 %>%
  mutate(time.unixts = time.unixts/1000) %>%
  mutate(time.hrts = as.POSIXct(time.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(time.hrts = strftime(time.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"))  %>% 
  mutate(time.hrts = as.character(time.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT")

# Rearrange columns
df.raw.incentive.cc1 <- df.raw.incentive.cc1 %>%
  arrange(user.id, time.unixts) %>%
  mutate(ones=1) %>%
  group_by(user.id) %>%
  mutate(incentive.id = cumsum(ones)) %>%
  select(-ones) %>%
  select(user.id, incentive.id, time.hrts, timezone.hrts, time.unixts, everything())

# Save cleaned data
write.csv(df.raw.incentive.cc1, 
          file.path(path.breakfree.output_data, "incentive_cc1.csv"), 
          row.names=FALSE, na="")

