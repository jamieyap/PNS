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
load(file.path(path.breakfree.staged_data, "raw.wakeup.cc1.RData"))

# Create data frame from list
df.raw.wakeup.cc1 <- bind_rows(list.df.raw.wakeup.cc1)

# Clean up columns
# V3 is in milliseconds elapsed since the 0th hour of the day
# Hence, to ceonvert V3 into 24-hour time format, use the calculation
# (V3/1000)/(60*60)

df.raw.wakeup.cc1 <- df.raw.wakeup.cc1 %>%
  mutate(setwakeup.unixts = V1/1000,
         wakeup.hour = (V3/1000)/(60*60)) %>%
  mutate(wakeup.hour = round(wakeup.hour, digits=2)) %>%
  mutate(setwakeup.hrts = as.POSIXct(setwakeup.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(setwakeup.hrts = strftime(setwakeup.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"))  %>% 
  mutate(setwakeup.hrts = as.character(setwakeup.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT") %>%
  select(user.id, setwakeup.hrts, timezone.hrts, setwakeup.unixts, wakeup.hour)


# It is possible for wakeup time to be changed by study coordinators
# during the course of the study. So, we index wakeup time/s recorded for each
# individual
df.raw.wakeup.cc1 <- df.raw.wakeup.cc1 %>% 
  arrange(user.id, setwakeup.unixts) %>%
  mutate(ones=1) %>%
  group_by(user.id) %>%
  mutate(setwakeup.id = cumsum(ones)) %>%
  select(-ones)

# Select relevant columns
df.raw.wakeup.cc1 <- df.raw.wakeup.cc1 %>%
  select(user.id, setwakeup.id,
         setwakeup.hrts, timezone.hrts, setwakeup.unixts, wakeup.hour)

# Save cleaned data
write.csv(df.raw.wakeup.cc1, 
          file.path(path.breakfree.output_data, "wakeup.cc1.csv"), 
          row.names=FALSE)

