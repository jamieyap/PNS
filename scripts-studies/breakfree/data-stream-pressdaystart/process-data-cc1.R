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
load(file.path(path.breakfree.staged_data, "raw.pressdaystart.cc1.RData"))

# Create data frame from list
df.raw.pressdaystart.cc1 <- bind_rows(list.df.raw.pressdaystart.cc1)


# Clean up columns
df.raw.pressdaystart.cc1 <- df.raw.pressdaystart.cc1 %>%
  mutate(pressdaystart.unixts = V3/1000) %>%
  mutate(pressdaystart.hrts = as.POSIXct(pressdaystart.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(pressdaystart.hrts = strftime(pressdaystart.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT")) %>%
  mutate(pressdaystart.hrts = as.character(pressdaystart.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT")

# Create id's for each start of day button press
df.raw.pressdaystart.cc1 <- df.raw.pressdaystart.cc1 %>%
  arrange(user.id, pressdaystart.unixts) %>%
  mutate(ones=1) %>%
  group_by(user.id) %>%
  mutate(pressdaystart.id = cumsum(ones)) %>%
  select(-ones)

# Select relevant columns
df.raw.pressdaystart.cc1 <- df.raw.pressdaystart.cc1 %>%
  select(user.id, pressdaystart.id, pressdaystart.hrts, timezone.hrts, pressdaystart.unixts)

# Save cleaned data
write.csv(df.raw.pressdaystart.cc1, 
          file.path(path.breakfree.output_data, "pressdaystart.cc1.csv"), 
          row.names=FALSE)

