###############################################################################
# ABOUT:
# * Cleans up EMA raw data files:
#   - exclude EMAs with some indication of unsuccessful delivery
#   - constuct variables for time when EMA was delivered, begun, completed
#   - construct variable indicating whether a response to any item was recorded
#   - clean up smoking counts variable
# * Apply new column names for EMA item (in ema_item_names.csv)
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
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

# Create placeholder variables for smoking items
df.processed <- df.processed %>% 
  mutate(rawdata.indicator = NA_real_, rawdata.qty = NA_real_, rawdata.timing = NA_real_, smoking.qty = NA_real_, smoking.delta.minutes = NA_real_)

# Reorder columns
df.processed <- df.processed %>%
  select(id, record.id, assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         rawdata.indicator, rawdata.qty, rawdata.timing, smoking.qty, smoking.delta.minutes,
         these.ema.colnames)

# Append data frame corresponding to this specific EMA type to list
# that will contain data frames from each EMA type
list.all <- append(list.all, list(`Post-Quit Already Slipped` = df.processed))

#------------------------------------------------------------------------------
# Identify smoking-related items in each EMA
#------------------------------------------------------------------------------

# Pre-Quit EMAs ###############################################################
list.all[["Pre-Quit Random"]] <- list.all[["Pre-Quit Random"]] %>% 
  mutate(rawdata.indicator = PreQRSmoking1, 
         rawdata.qty = Smoking2_PreQ_Random, 
         rawdata.timing = Smoking3)

list.all[["Pre-Quit Urge"]] <- list.all[["Pre-Quit Urge"]] %>% 
  mutate(rawdata.indicator = SmPQU1, 
         rawdata.qty = Smoking2_PreQ_Urge, 
         rawdata.timing = Smoking3)

list.all[["Pre-Quit Smoking Part One"]] <- list.all[["Pre-Quit Smoking Part One"]] %>% 
  mutate(rawdata.indicator = NA, 
         rawdata.qty = NA, 
         rawdata.timing = NA)

list.all[["Pre-Quit Smoking Part Two"]] <- list.all[["Pre-Quit Smoking Part Two"]] %>% 
  mutate(rawdata.qty = CigJustNow) %>%
  mutate(rawdata.indicator = NA, 
         rawdata.timing = NA)

# Post-Quit EMAs ##############################################################
list.all[["Post-Quit Random"]] <- list.all[["Post-Quit Random"]] %>% 
  mutate(rawdata.indicator = PostQRSmoking1, 
         rawdata.qty = Smoking2_PostQ_Random, 
         rawdata.timing = Smoking3)

list.all[["Post-Quit Urge"]] <- list.all[["Post-Quit Urge"]] %>%  
  mutate(rawdata.indicator = SmPostQU1, 
         rawdata.qty = Smoking2_PostQ_Urge, 
         rawdata.timing = Smoking3)

list.all[["Post-Quit About to Slip Part One"]] <- list.all[["Post-Quit About to Slip Part One"]] %>%  
  mutate(rawdata.indicator = NA, 
         rawdata.qty = NA, 
         rawdata.timing = NA)

list.all[["Post-Quit About to Slip Part Two"]] <- list.all[["Post-Quit About to Slip Part Two"]] %>%  
  mutate(rawdata.qty = CigJustNow_PostQ_Slip2) %>%
  mutate(rawdata.indicator = NA, 
         rawdata.timing = NA)

list.all[["Post-Quit Already Slipped"]] <- list.all[["Post-Quit Already Slipped"]] %>%  
  mutate(rawdata.qty = HowManyCig, 
         rawdata.timing = LastCig) %>%
  mutate(rawdata.indicator = NA)

#------------------------------------------------------------------------------
# Clean up smoking counts variable
#------------------------------------------------------------------------------

# Pre-Quit EMAs ###############################################################

list.all[["Pre-Quit Random"]] <- list.all[["Pre-Quit Random"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Pre-Quit Random" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Random" & rawdata.qty==1 ~ 1,
    assessment.type=="Pre-Quit Random" & rawdata.qty==2 ~ 2,
    assessment.type=="Pre-Quit Random" & rawdata.qty==3 ~ 4,
    assessment.type=="Pre-Quit Random" & rawdata.qty==4 ~ 6,
    assessment.type=="Pre-Quit Random" & rawdata.qty==5 ~ 8,
    assessment.type=="Pre-Quit Random" & rawdata.qty==6 ~ 10,
    assessment.type=="Pre-Quit Random" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Pre-Quit Urge"]] <- list.all[["Pre-Quit Urge"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Pre-Quit Urge" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==1 ~ 1,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==2 ~ 2,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==3 ~ 4,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==4 ~ 6,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==5 ~ 8,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==6 ~ 10,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Pre-Quit Smoking Part Two"]] <- list.all[["Pre-Quit Smoking Part Two"]] %>% 
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==3 ~ 2,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==4 ~ 3,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==5 ~ 4,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==6 ~ 5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==7 ~ 6,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==1 ~ 1,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==2 ~ 2,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==3 ~ 4,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==4 ~ 6,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==5 ~ 8,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==6 ~ 10,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) 

# Post-Quit EMAs ###############################################################
list.all[["Post-Quit Random"]] <- list.all[["Post-Quit Random"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Post-Quit Random" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit Random" & rawdata.qty==1 ~ 1,
    assessment.type=="Post-Quit Random" & rawdata.qty==2 ~ 2,
    assessment.type=="Post-Quit Random" & rawdata.qty==3 ~ 4,
    assessment.type=="Post-Quit Random" & rawdata.qty==4 ~ 6,
    assessment.type=="Post-Quit Random" & rawdata.qty==5 ~ 8,
    assessment.type=="Post-Quit Random" & rawdata.qty==6 ~ 10,
    assessment.type=="Post-Quit Random" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Post-Quit Urge"]] <- list.all[["Post-Quit Urge"]] %>% 
  mutate(smoking.qty = case_when(
  assessment.type=="Post-Quit Urge" & rawdata.qty==0 ~ 0,
  assessment.type=="Post-Quit Urge" & rawdata.qty==1 ~ 1,
  assessment.type=="Post-Quit Urge" & rawdata.qty==2 ~ 2,
  assessment.type=="Post-Quit Urge" & rawdata.qty==3 ~ 4,
  assessment.type=="Post-Quit Urge" & rawdata.qty==4 ~ 6,
  assessment.type=="Post-Quit Urge" & rawdata.qty==5 ~ 8,
  assessment.type=="Post-Quit Urge" & rawdata.qty==6 ~ 10,
  assessment.type=="Post-Quit Urge" & rawdata.qty==7 ~ 11,
  TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Post-Quit About to Slip Part Two"]] <- list.all[["Post-Quit About to Slip Part Two"]] %>% 
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==3 ~ 2,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==4 ~ 3,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==5 ~ 4,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==6 ~ 5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==7 ~ 6,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==1 ~ 1,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==2 ~ 2,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==3 ~ 4,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==4 ~ 6,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==5 ~ 8,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==6 ~ 10,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty))

list.all[["Post-Quit Already Slipped"]] <- list.all[["Post-Quit Already Slipped"]] %>% 
  mutate(smoking.qty = case_when(
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==0 ~ 0,
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==1 ~ 1,
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==2 ~ 2,
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==3 ~ 4,
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==4 ~ 6,
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==5 ~ 8,
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==6 ~ 10,
  assessment.type=="Post-Quit Already Slipped" & rawdata.qty==7 ~ 11,
  TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

#------------------------------------------------------------------------------
# Change column names in each data frame within list.all to new column names 
# in ema.item.names
#------------------------------------------------------------------------------

for(k in 1:length(list.all)){
  current.assessment.type <- names(list.all)[k]
  df.processed <- list.all[[current.assessment.type]]
  old.column.names <- colnames(df.processed)
  these.old <- ema.item.names %>% filter(assessment.type==current.assessment.type) %>% extract2("name.codebook")
  these.new <- ema.item.names %>% filter(assessment.type==current.assessment.type) %>% extract2("name.new")
  
  for(i in 1:length(old.column.names)){
    if(old.column.names[i] %in% these.old){
      idx <- which(these.old==old.column.names[i])
      colnames(df.processed)[i] <- these.new[idx]
    }else{
      next
    }
  }
  # Update df.processed with new column names
  list.all[[current.assessment.type]] <- df.processed
}

#------------------------------------------------------------------------------
# Save list.all as an .RData object
#------------------------------------------------------------------------------

save(list.all, file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

