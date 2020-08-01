###############################################################################
# ABOUT:
# * Cleans up EMA raw data files:
#   - exclude EMAs with some indication of unsuccessful delivery
#   - constuct variables for time when EMA was delivered, begun, completed
#   - construct variable indicating whether a response to any item was recorded
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

# Item names from all EMA types
ema.item.names <- read.csv(file.path(path.pns.output_data, "ema_item_names.csv"), header = TRUE, stringsAsFactors = FALSE)

# List will contain data frames corresponding to each EMA type
list.all <- list()

#------------------------------------------------------------------------------
# Construct EMA time variables
#------------------------------------------------------------------------------

# Pre-Quit Random EMAs ########################################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Random.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Pre-Quit Random") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Pre-Quit Random` = df.processed))

# Pre-Quit Urge EMAs ##########################################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Urge.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Pre-Quit Urge") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Pre-Quit Urge` = df.processed))

# Pre-Quit Smoking Part One EMAs ##########################################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Pre-Quit Smoking Part One") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

df.processed <- df.processed %>% mutate(assessment.type = replace(assessment.type, assessment.type=="Pre-Quit Smoking", "Pre-Quit Smoking Part One"))

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Pre-Quit Smoking Part One` = df.processed))

# Pre-Quit Smoking Part Two EMAs ##########################################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking_Part2.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Pre-Quit Smoking Part Two") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

df.processed <- df.processed %>% mutate(assessment.type = replace(assessment.type, assessment.type=="Pre-Quit Smoking - Part2", "Pre-Quit Smoking Part Two"))

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Pre-Quit Smoking Part Two` = df.processed))

# Post-Quit Random EMAs #######################################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Random.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Post-Quit Random") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Post-Quit Random` = df.processed))

# Post-Quit Urge EMAs #########################################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Urge.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Post-Quit Urge") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Post-Quit Urge` = df.processed))

# Post-Quit About to Slip Part One EMAs ##############################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Post-Quit About to Slip Part One") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

df.processed <- df.processed %>% mutate(assessment.type = replace(assessment.type, assessment.type=="Post-Quit About to Slip", "Post-Quit About to Slip Part One"))

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Post-Quit About to Slip Part One` = df.processed))

# Post-Quit About to Slip Part Two EMAs ##############################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip_Part2.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Post-Quit About to Slip Part Two") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

df.processed <- df.processed %>% mutate(assessment.type = replace(assessment.type, assessment.type=="Post-Quit About to Slip - Part2", "Post-Quit About to Slip Part Two"))

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Post-Quit About to Slip Part Two` = df.processed))

# Post-Quit Already Slipped EMAs ##############################################

# Read in raw data from one type of EMA
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Already_Slipped.csv"), stringsAsFactors = FALSE)

# Obtain item names corresponding to this type of EMA
these.ema.colnames <- ema.item.names %>% filter(assessment.type=="Post-Quit Already Slipped") %>% extract2("name.codebook")

# Create time variables, when EMAs were delivered, begun, and completed
# Also, create an indicator for whether an EMA was successfully delivered
df.processed <- CreateEMATimeVars(df.raw = df.raw)
df.processed <- df.processed %>% filter(is.delivered==1)

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

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts, 
         these.ema.colnames)

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Post-Quit Already Slipped` = df.processed))

#------------------------------------------------------------------------------
# Save list.all as an .RData object
#------------------------------------------------------------------------------

save(list.all, file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

