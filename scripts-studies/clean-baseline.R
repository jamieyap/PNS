###############################################################################
# ABOUT:
# * Exclude rows corresponding to baseline participants who will not be
#   included in any analysis
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

# Read in final quit date file
df.quit.dates <- readRDS(file.path(path.pns.staged_data, "quit_dates_final.RData"))

# Read baseline raw data file
raw.data.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"), stringsAsFactors = FALSE)

# Exclude individuals who will be not be included in all data analysis
df.ids <- df.quit.dates %>% filter(exclude==0) %>% select(id, callnumr)
subset.raw.data.baseline <- left_join(x = df.ids, y = raw.data.baseline, by = c("callnumr"))

saveRDS(subset.raw.data.baseline, file.path(path.pns.staged_data, "baseline.RData"))
