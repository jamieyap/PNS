library(dplyr)

# ABOUT:
# * Creates a file with a list of item names in each EMA raw data file;
#   these item names correspond to that recorded in the codebooks

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")

# Collect item names for all types of pre-quit and post-quit EMA into one list
list.collect <- list()

# name.codebook refers to item names in the raw data files & codebook
# new.item.names refers to new names that will be assigned to each item
# the new names were created so that items belonging to specific EMA types
# could be easily referenced when working from within a file merging data
# from across all EMA types

# Post-Quit Random EMA --------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Random.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("postquit_random_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1, 
                            assessment.type = "Post-Quit Random", 
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Post-Quit Urge EMA ----------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Urge.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("postquit_urge_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit Urge",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Post-Quit About to Slip Part 1 EMA ------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip.csv"))
ema.item.names <- colnames(df.raw[,32:95])
new.item.names <- paste("postquit_abouttoslippartone_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit About to Slip Part One",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Post-Quit About to Slip Part 2 EMA ------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip_Part2.csv"))
ema.item.names <- colnames(df.raw[,32:105])
new.item.names <- paste("postquit_abouttoslipparttwo_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit About to Slip Part Two",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Post-Quit Already Slipped EMA -----------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Already_Slipped.csv"))
ema.item.names <- colnames(df.raw[,32:101])
new.item.names <- paste("postquit_alreadyslipped_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 1,
                            assessment.type = "Post-Quit Already Slipped",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Random EMA ---------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Random.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("prequit_random_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Random",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Urge EMA -----------------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Urge.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("prequit_urge_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Urge",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Smoking Part 1 EMA -------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking.csv"))
ema.item.names <- colnames(df.raw[,32:95])
new.item.names <- paste("prequit_smokingpartone_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Smoking Part One",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Pre-Quit Smoking Part 2 EMA -------------------------------------------------
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking_Part2.csv"))
ema.item.names <- colnames(df.raw[,32:100])
new.item.names <- paste("prequit_smokingparttwo_item_", 1:length(ema.item.names), sep="")
df.item.names <- data.frame(is.postquit.assessment.type = 0,
                            assessment.type = "Pre-Quit Smoking Part Two",
                            name.codebook = ema.item.names, 
                            name.new = new.item.names)
list.collect <- append(list.collect, list(df.item.names))

# Prepare output --------------------------------------------------------------
df.collect <- do.call(rbind, list.collect)

write.csv(df.collect, file.path(path.pns.output_data, "ema_item_names.csv"), row.names = FALSE)


#------------------------------------------------------------------------------
# Change periods in column names to underscrores
# This is mostly to accomodate a wider range of end-users of the curated data
# who may use a varied range of data analysis software, some of which
# make it easier to work with column names with an underscore (rather than
# dots) than others
#------------------------------------------------------------------------------
df.collect <- df.collect %>%
  rename(is_postquit_assessment_type = is.postquit.assessment.type,
         assessment_type = assessment.type,
         name_codebook = name.codebook,
         name_new = name.new)

write.csv(df.collect, file.path(path.pns.output_data, "ema_item_names_reformatted_column_names.csv"), row.names = FALSE)

