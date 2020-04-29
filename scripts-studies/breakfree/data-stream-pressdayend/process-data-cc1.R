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
load(file.path(path.breakfree.staged_data, "raw.pressdayend.cc1.RData"))

# Create data frame from list
df.raw.pressdayend.cc1 <- bind_rows(list.df.raw.pressdayend.cc1)


# Clean up columns
df.raw.pressdayend.cc1 <- df.raw.pressdayend.cc1 %>%
  mutate(pressdayend.unixts = V3/1000) %>%
  mutate(pressdayend.hrts = as.POSIXct(pressdayend.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(pressdayend.hrts = strftime(pressdayend.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT")) %>%
  mutate(pressdayend.hrts = as.character(pressdayend.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT")

# Create id's for each start of day button press
df.raw.pressdayend.cc1 <- df.raw.pressdayend.cc1 %>%
  arrange(user.id, pressdayend.unixts) %>%
  mutate(ones=1) %>%
  group_by(user.id) %>%
  mutate(pressdayend.id = cumsum(ones)) %>%
  select(-ones)

# Select relevant columns
df.raw.pressdayend.cc1 <- df.raw.pressdayend.cc1 %>%
  select(user.id, pressdayend.id, pressdayend.hrts, timezone.hrts, pressdayend.unixts)

# Save cleaned data
write.csv(df.raw.pressdayend.cc1, 
          file.path(path.breakfree.output_data, "pressdayend.cc1.csv"), 
          row.names=FALSE)

