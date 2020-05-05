###############################################################################
# ABOUT:
# * Cleans up the Post Quit Already Slipped EMA raw data file
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
# Read in raw data
#------------------------------------------------------------------------------
quit.dates <- read.csv(file.path(path.pns.output_data, "quit_dates.csv"))
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Already_Slipped.csv"))

#------------------------------------------------------------------------------
# Tasks common to all analyses using PNS study data
#------------------------------------------------------------------------------
# Implement decision rules on constructing time frame
df.time.frame <- SetPostQuitTimeFrame(df.quit.dates = quit.dates, postquit.duration = 21, addtime = 0)
df.time.frame$id <- as.character(df.time.frame$id)

# Implement decision rules to include only "valid EMAs"
df.processed <- SetUpPostQuit(df.raw = df.raw)

# Exclude rows before or after start.clock and end.clock in df.time.frame
df.processed <- left_join(x = df.processed, y = df.time.frame, by = "id")
df.processed <- df.processed %>% filter(delivered.unixts>=start.clock.unixts & delivered.unixts<=end.clock.unixts)

#------------------------------------------------------------------------------
# Tasks specific to this dataset for analysis
#------------------------------------------------------------------------------
# Read in item names of the EMA
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_alreadyslipped.csv"), header = TRUE)
ema.item.names$name.codebook <- as.character(ema.item.names$name.codebook)

# Create a new variable so that for each row in the raw data is checked: the 
# new variable with.any.response equals 1 if response to any question was 
# recorded and equals 0 if no response to any question was recorded
df.processed <- CheckAnyResponse(df = df.processed, keep.cols = ema.item.names$name.codebook)

#------------------------------------------------------------------------------
# Format date and time variables
#------------------------------------------------------------------------------
df.processed[["quit.date"]] <- strftime(df.processed[["quit.date"]], format = "%Y-%m-%d")
df.processed[["start.clock.hrts"]] <- strftime(df.processed[["start.clock.hrts"]], format = "%Y-%m-%d %H:%M:%S")
df.processed[["end.clock.hrts"]] <- strftime(df.processed[["end.clock.hrts"]], format = "%Y-%m-%d %H:%M:%S")
df.processed[["delivered.hrts"]] <- strftime(df.processed[["delivered.hrts"]], format = "%Y-%m-%d %H:%M:%S")
df.processed[["begin.hrts"]] <- strftime(df.processed[["begin.hrts"]], format = "%Y-%m-%d %H:%M:%S")
df.processed[["end.hrts"]] <- strftime(df.processed[["end.hrts"]], format = "%Y-%m-%d %H:%M:%S")

#------------------------------------------------------------------------------
# Check whether there are any missing begin.unixts/begin.hrts timestamps
#------------------------------------------------------------------------------
df.processed[["begin.hrts"]] <- if_else(df.processed[["with.any.response"]]==1 & is.na(df.processed[["begin.hrts"]]),
                                        df.processed[["delivered.hrts"]], 
                                        df.processed[["begin.hrts"]])

#------------------------------------------------------------------------------
# Clean up data frame and write out processed data
#------------------------------------------------------------------------------
df.processed <- df.processed %>% 
  select(id, callnumr, 
         quit.date,
         start.clock.hrts, end.clock.hrts, 
         start.clock.unixts, end.clock.unixts,
         record.id, assessment.type,
         delivered.hrts, begin.hrts, end.hrts,
         delivered.unixts, begin.unixts, end.unixts,
         with.any.response,
         ema.item.names$name.codebook)


write.csv(df.processed, file.path(path.pns.output_data, "post_quit_alreadyslipped.csv"), row.names = FALSE)
