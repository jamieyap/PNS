###############################################################################
# ABOUT:
# * Cleans up Pre Quit EMA raw data files
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
# Pre Quit Random EMA
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Random.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_random.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-prequit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "prequit_random.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

#------------------------------------------------------------------------------
# Pre Quit Urge EMA
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Urge.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_urge.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-prequit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "prequit_urge.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

#------------------------------------------------------------------------------
# Pre Quit About to Slip Part 1
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_about2slip1.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-prequit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "prequit_about2slip1.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

#------------------------------------------------------------------------------
# Pre Quit About to Slip Part 2
#------------------------------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking_Part2.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_about2slip2.csv"), header = TRUE, stringsAsFactors = FALSE)
source(file.path(path.pns.code, "prepare-prequit-ema.R"))
write.csv(df.processed, file.path(path.pns.output_data, "prequit_about2slip2.csv"), row.names = FALSE, na = "")
remove(df.processed, df.raw, ema.item.names)

