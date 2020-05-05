###############################################################################
# ABOUT:
# * Creates a curated dataset for baseline variables in the PNS study
# * Only rows corresponding to individuals with recorded quit dates are included
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.pns.code, "data-manip-utils.R"))
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

#------------------------------------------------------------------------------
# Read in data
#------------------------------------------------------------------------------
quit.dates <- read.csv(file.path(path.pns.output_data, "quit_dates.csv"))
df.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"))

#------------------------------------------------------------------------------
# Implement decision rules on constructing time frame
#------------------------------------------------------------------------------
df.time.frame <- SetPostQuitTimeFrame(df.quit.dates = quit.dates, postquit.duration = 21, addtime = 0)
df.time.frame$id <- as.character(df.time.frame$id)
df.time.frame$callnumr <- as.character(df.time.frame$callnumr)
df.time.frame <- df.time.frame %>% 
  select(id, callnumr) %>% 
  mutate(id = as.character(id),
         callnumr = as.character(callnumr))

#------------------------------------------------------------------------------
# Remove rows corresponding to individuals with no recorded quit date
# in df.time.frame
#------------------------------------------------------------------------------
df.baseline <- df.baseline %>% select(-quitday) %>% mutate(callnumr = as.character(callnumr))
df.baseline <- left_join(x = df.time.frame, y = df.baseline, by = "callnumr")

#------------------------------------------------------------------------------
# Write out data
#------------------------------------------------------------------------------
write.csv(df.baseline, file.path(path.pns.output_data, "baseline.csv"), row.names = FALSE)

