###############################################################################
# ABOUT:
# * Cleans up Post Quit EMA raw data files
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
# Dates
#------------------------------------------------------------------------------
df.dates <- read.csv(file.path(path.pns.output_data, "dates.csv"), stringsAsFactors = FALSE)

#------------------------------------------------------------------------------
# Post Quit Random EMA
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Random.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_random.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-postquit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "postquit_random.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

#------------------------------------------------------------------------------
# Post Quit Urge EMA
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Urge.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_urge.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-postquit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "postquit_urge.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

#------------------------------------------------------------------------------
# Post Quit About to Slip Part One EMA
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_abouttoslippartone.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-postquit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "postquit_abouttoslippartone.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

#------------------------------------------------------------------------------
# Post Quit About to Slip Part Two EMA
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip_Part2.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_abouttoslipparttwo.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-postquit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "postquit_abouttoslipparttwo.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

#------------------------------------------------------------------------------
# Post Quit Already Slipped EMA
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Already_Slipped.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_alreadyslipped.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-postquit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "postquit_alreadyslipped.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)


