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
load(file.path(path.breakfree.staged_data, "raw.sleep.cc1.RData"))

# Create data frame from list
df.raw.sleep.cc1 <- bind_rows(list.df.raw.sleep.cc1)

# Clean up columns
# V3 is in milliseconds elapsed since the 0th hour of the day
# Hence, to ceonvert V3 into 24-hour time format, use the calculation
# (V3/1000)/(60*60)

df.raw.sleep.cc1 <- df.raw.sleep.cc1 %>%
  mutate(setsleeptime.unixts = V1/1000,
         sleep.hour = (V3/1000)/(60*60)) %>%
  mutate(sleep.hour = round(sleep.hour, digits=2)) %>%
  mutate(setsleeptime.hrts = as.POSIXct(setsleeptime.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(setsleeptime.hrts = strftime(setsleeptime.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"))  %>% 
  mutate(setsleeptime.hrts = as.character(setsleeptime.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT") %>%
  select(user.id, setsleeptime.hrts, timezone.hrts, setsleeptime.unixts, sleep.hour)


# It is possible for sleep time to be changed by study coordinators
# during the course of the study. So, we index sleep time/s recorded for each
# individual
df.raw.sleep.cc1 <- df.raw.sleep.cc1 %>% 
  arrange(user.id, setsleeptime.unixts) %>%
  mutate(ones=1) %>%
  group_by(user.id) %>%
  mutate(setsleeptime.id = cumsum(ones)) %>%
  select(-ones)

# Select relevant columns
df.raw.sleep.cc1 <- df.raw.sleep.cc1 %>%
  select(user.id, setsleeptime.id,
         setsleeptime.hrts, timezone.hrts, setsleeptime.unixts, sleep.hour)

# Save cleaned data
write.csv(df.raw.sleep.cc1, 
          file.path(path.breakfree.output_data, "sleep_cc1.csv"), 
          row.names=FALSE, na="")

