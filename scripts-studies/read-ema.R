###############################################################################
# ABOUT:
# * Reads in EMA raw data files
# * Constuct variables for time when EMA was delivered, begun, completed
# * Construct variable indicating whether a response to any item was recorded
# * Save to intermediate file in preparation for further data processing
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

# Names of each raw data file
raw.data.file.names <- c("Pre_Quit_Random.csv", "Pre_Quit_Urge.csv", "Pre_Quit_Smoking.csv", "Pre_Quit_Smoking_Part2.csv",
                         "Post_Quit_Random.csv", "Post_Quit_Urge.csv", "Post_Quit_About_to_Slip.csv", "Post_Quit_About_to_Slip_Part2.csv",
                         "Post_Quit_Already_Slipped.csv")

# Names of each EMA type in the curated data will differ in formatting from raw data
# e.g., "Post-Quit About to Slip - Part2" in the raw data will be "Post-Quit About to Slip Part Two" in the curated data
reformatted.ema.type.names <- c("Pre-Quit Random", "Pre-Quit Urge", "Pre-Quit Smoking Part One", "Pre-Quit Smoking Part Two",
                                "Post-Quit Random", "Post-Quit Urge", "Post-Quit About to Slip Part One", "Post-Quit About to Slip Part Two",
                                "Post-Quit Already Slipped")

old.ema.type.names <- c("Pre-Quit Random", "Pre-Quit Urge", "Pre-Quit Smoking", "Pre-Quit Smoking - Part2",
                        "Post-Quit Random", "Post-Quit Urge", "Post-Quit About to Slip", "Post-Quit About to Slip - Part2",
                        "Post-Quit Already Slipped")

# Now combine the above into a data frame
df.all.names <- data.frame(raw.data.file.names = raw.data.file.names, 
                           old.ema.type.names = old.ema.type.names,
                           reformatted.ema.type.names = reformatted.ema.type.names,
                           stringsAsFactors = FALSE)

# Read in item names from all EMA types
ema.item.names <- read.csv(file.path(path.pns.output_data, "ema_item_names.csv"), header = TRUE, stringsAsFactors = FALSE)

# List will contain data frames corresponding to each EMA type
list.all <- list()

# Loop through all items
for(i in 1:length(raw.data.file.names)){
  current.raw.data.file.name <- df.all.names[["raw.data.file.names"]][i]
  current.old.ema.type.name <- df.all.names[["old.ema.type.names"]][i]
  current.reformatted.ema.type.name <- df.all.names[["reformatted.ema.type.names"]][i]
  
  # Read in raw data from one type of EMA
  df.raw <- read.csv(file.path(path.pns.input_data, current.raw.data.file.name), stringsAsFactors = FALSE)
  
  # Obtain item names corresponding to this type of EMA
  these.ema.colnames <- ema.item.names %>% filter(assessment.type==current.reformatted.ema.type.name) %>% extract2("name.codebook")
  
  # Create time variables, when EMAs were delivered, begun, and completed
  # Also, create an indicator for whether an EMA was successfully delivered
  df.processed <- CreateEMATimeVars(df.raw = df.raw)
  
  # Create a variable indicating whether an EMA has a recorded response to any item
  df.processed <- CheckAnyResponse(df = df.processed, keep.cols = these.ema.colnames)
  
  # If for some reason, an EMA has a recorded response to any item, 
  # but does not have time when participant began completing the EMA recorded
  # (note: in the raw data, begin.hrts=AssessmentBegin) then set time when
  # EMA was delivered as the begin time
  df.processed <- df.processed %>% 
    mutate(begin.hrts = if_else(with.any.response==1 & is.na(begin.hrts), delivered.hrts, begin.hrts),
           begin.unixts = if_else(with.any.response==1 & is.na(begin.unixts), delivered.unixts, begin.unixts))
  
  # In data analyses, the analyst may want to only refer to a single time variable
  # We integrate time information when with.any.response=1 and when with.any.response=0
  # into one variable. If with.any.response=1, set this time variable to be the time
  # when an individual began completing the EMA but if with.any.response=0
  # then set this time variable to be the time when the EMA was delivered
  df.processed <- df.processed %>% 
    mutate(time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts),
           time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts))
  
  # Get reformatted EMA type names
  df.processed <- df.processed %>% 
    mutate(assessment.type = replace(assessment.type, 
                                     assessment.type==current.old.ema.type.name, 
                                     current.reformatted.ema.type.name))
  
  # Reorder columns
  df.processed <- df.processed %>%
    select(id, record.id, assessment.type, 
           with.any.response, record.status,
           delivered.hrts, begin.hrts, end.hrts, time.hrts,
           delivered.unixts, begin.unixts, end.unixts, time.unixts, 
           these.ema.colnames)
  
  # Append data frame corresponding to this specific EMA type to list
  # that will contain data frames from each EMA type
  list.all <- append(list.all, list(df.processed))
}

names(list.all) <- reformatted.ema.type.names

#------------------------------------------------------------------------------
# Save list.all as an .RData object
#------------------------------------------------------------------------------

save(list.all, file = file.path(path.pns.staged_data, "all_ema_processed.RData"))


