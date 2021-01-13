###############################################################################
# ABOUT:
# * Identify column names in raw data where responses to EMA items are recorded
# * For each of the original column names, new names are given;
#   the format used in name_new enables end-users to determine the particular 
#   kind of EMA in which the value in the merged dataset (having data from two
#   or more kinds of EMA; there are nine kinds of EMA) was originally provided
###############################################################################

library(dplyr)
path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")

# Collect item names for all types of pre-quit and post-quit EMA into one list
list.collect <- list()

# name.codebook refers to item names in the raw data files & codebook
# new.item.names refers to new names that will be assigned to each item
# the new names were created so that items belonging to specific EMA types
# could be easily referenced when working from within a file merging data
# from across all EMA types

# Post-Quit Already Slipped EMA -----------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Already_Slipped.csv"))
ema.item.names <- colnames(df.raw[,32:101])
new.item.names <- paste("postquit_alreadyslipped_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit Already Slipped",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Post-Quit Random EMA --------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Random.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("postquit_random_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1, 
                            assessment.type = "Post-Quit Random", 
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Post-Quit Urge EMA ----------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Urge.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("postquit_urge_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit Urge",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Post-Quit About to Slip Part 1 EMA ------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip.csv"))
ema.item.names <- colnames(df.raw[,32:95])
new.item.names <- paste("postquit_partone_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit About to Slip Part One",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Post-Quit About to Slip Part 2 EMA ------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip_Part2.csv"))
ema.item.names <- colnames(df.raw[,32:105])
new.item.names <- paste("postquit_parttwo_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit About to Slip Part Two",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Random EMA ---------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Random.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("prequit_random_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Random",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Urge EMA -----------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Urge.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("prequit_urge_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Urge",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Smoking Part 1 EMA -------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking.csv"))
ema.item.names <- colnames(df.raw[,32:95])
new.item.names <- paste("prequit_partone_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Smoking Part One",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Smoking Part 2 EMA -------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking_Part2.csv"))
ema.item.names <- colnames(df.raw[,32:100])
new.item.names <- paste("prequit_parttwo_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Smoking Part Two",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names,
                            stringsAsFactors = FALSE)

list.collect <- append(list.collect, list(df.item.names))

# Prepare output --------------------------------------------------------------
df.collect <- do.call(rbind, list.collect)
ema.item.names <- df.collect
saveRDS(ema.item.names, file = file.path(path.pns.staged_data, "ema_item_names.RData"))




