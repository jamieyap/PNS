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
colnames(df.raw.incentive.cc2) <- c("mCerebrum.ts", 
                                    "mCerebrum.offset",
                                    "incentive.now",
                                    "incentive.cumulative",
                                    "user.id")
df.raw.incentive.cc2 <- df.raw.incentive.cc2 %>%
  select(-mCerebrum.offset) %>%
  mutate(mCerebrum.ts = mCerebrum.ts/1000) %>%
  mutate(mCerebrum.hrts = as.POSIXct(mCerebrum.ts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(mCerebrum.hrts = strftime(mCerebrum.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"))  %>% 
  mutate(mCerebrum.hrts = as.character(mCerebrum.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT") %>%
  select(user.id, mCerebrum.hrts, timezone.hrts, mCerebrum.ts, everything())

write.csv(df.smoking.items.cc2, 
          file.path(path.breakfree.output_data, "df.incentive.cc2.csv"), 
          row.names=FALSE)
