# This is a sample script on how merging processed CC1 and CC2 data can be done

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

###############################################################################
# The following files are saved in path.breakfree.output_data
# random.ema.items.cc1.csv
# random.ema.items.cc2.csv
# resp.random.ema.cc1.csv
# resp.random.ema.cc2.csv
###############################################################################
random.ema.items.cc1 <- read.csv(file.path(path.breakfree.output_data, "random.ema.items.cc1.csv"), header = TRUE)
random.ema.items.cc2 <- read.csv(file.path(path.breakfree.output_data, "random.ema.items.cc2.csv"), header = TRUE)
resp.random.ema.cc1 <- read.csv(file.path(path.breakfree.output_data, "resp.random.ema.cc1.csv"), header = TRUE)
resp.random.ema.cc2 <- read.csv(file.path(path.breakfree.output_data, "resp.random.ema.cc2.csv"), header = TRUE)

# Merge affect items from CC1 data and CC2 data
df.cc1 <- resp.random.ema.cc1 %>%
  select(user.id, ema.id, status, 
         prompt.ts, begin.ts, end.ts, mCerebrum.ts, with.any.response,
         active = item.4, angry = item.5, ashamed = item.6, calm = item.7, 
         determined = item.8, disgusted = item.9, enthusiastic = item.10,
         grateful = item.11, guilty = item.12, happy = item.13, 
         irritable = item.14, lonely = item.15, proud = item.16, 
         nervous = item.17, sad = item.18, restless = item.19,
         tired = item.20, hopeless = item.21, scared = item.22,
         bored = item.23, joyful = item.24, attentive = item.25, 
         relaxed = item.26, motivation = item.27, urge = item.28)

df.cc2 <- resp.random.ema.cc2 %>%
  select(user.id, ema.id, status, 
         prompt.ts, begin.ts, end.ts, mCerebrum.ts, with.any.response,
         active = item.4, angry = item.5, ashamed = item.6, calm = item.7, 
         determined = item.8, disgusted = item.9, enthusiastic = item.10,
         grateful = item.11, guilty = item.12, happy = item.13, 
         irritable = item.14, lonely = item.15, proud = item.16, 
         nervous = item.17, sad = item.18, restless = item.19,
         tired = item.20, hopeless = item.21, scared = item.22,
         bored = item.23, joyful = item.24, attentive = item.25, 
         relaxed = item.26, motivation = item.27, urge = item.28)

df.merged <- rbind(df.cc1, df.cc2)

# Save output
write.csv(df.merged, file.path(path.breakfree.output_data, "resp.random.ema.merged.csv"), row.names = FALSE)
