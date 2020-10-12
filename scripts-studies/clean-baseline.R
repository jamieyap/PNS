###############################################################################
# ABOUT:
# * Exclude rows corresponding to baseline participants who will not be
#   included in any analysis
# * Prior to running clean-ema.R, run the following scripts:
#   - calc-candidate-dates.R
#   - calc-quit-dates.R
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.pns.code, "data-manip-utils.R"))

# Read in final quit date file and retain a given row of the baseline data file 
# only if it corresponds to a participant having exclude==0
df.quit.dates <- read.csv(file.path(path.pns.output_data, "quit_dates_final.csv"), stringsAsFactors = FALSE)
df.ids <- df.quit.dates %>% filter(exclude==0) %>% select(id, callnumr)

# Read in raw data file and only retain those rows corresponding to IDs in df.ids
raw.data.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"), stringsAsFactors = FALSE)
subset.raw.data.baseline <- left_join(x = df.ids, y = raw.data.baseline, by = "callnumr")

write.csv(subset.raw.data.baseline, file.path(path.pns.output_data, "pns_curated_baseline_data.csv"), row.names=FALSE, na="")

