library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.breakfree.cc2.input_data <- Sys.getenv("path.breakfree.cc2.input_data")
path.breakfree.staged_data <- Sys.getenv("path.breakfree.staged_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))
load(file.path(path.breakfree.staged_data, "raw.incentive.cc2.RData"))

df.raw.incentive.cc2 <- bind_rows(list.df.raw.incentive.cc2)
colnames(df.raw.incentive.cc2) <- c("time.unixts", 
                                    "offset",
                                    "incentive.now",
                                    "incentive.cumulative",
                                    "user.id")

df.raw.incentive.cc2 <- df.raw.incentive.cc2 %>%
  select(-offset) %>%
  mutate(time.unixts = time.unixts/1000) %>%
  mutate(time.hrts = as.POSIXct(time.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(time.hrts = strftime(time.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"))  %>% 
  mutate(time.hrts = as.character(time.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT")

df.raw.incentive.cc2 <- df.raw.incentive.cc2 %>%
  arrange(user.id, time.unixts) %>%
  mutate(ones=1) %>%
  group_by(user.id) %>%
  mutate(incentive.id = cumsum(ones)) %>%
  select(-ones) %>%
  select(user.id, incentive.id, time.hrts, timezone.hrts, time.unixts, everything())

write.csv(df.raw.incentive.cc2, 
          file.path(path.breakfree.output_data, "incentive.cc2.csv"), 
          row.names=FALSE)
