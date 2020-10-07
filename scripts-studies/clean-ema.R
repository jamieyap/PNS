###############################################################################
# ABOUT:
# * Complete preparaton by type of EMA
# * Focus of this script is not on individual items within an EMA but on
#   exclusion/inclusion criteria and date formatting
# * Prior to running clean-ema.R, run the following scripts:
#   - read-ema.R
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

#------------------------------------------------------------------------------
# Create time variables
#------------------------------------------------------------------------------
df.quit.dates <- read.csv(file.path(path.pns.output_data, "quit_dates_final.csv"), stringsAsFactors = FALSE)
df.quit.dates <- df.quit.dates %>% rename(start.study.hrts = start.study.date, end.study.hrts = end.study.date, quit.hrts = quit.date) 

# Note that since start.study.hrts and end.study.hrts are both at midnight,
# the hour-minute-second component of the timestamp will not be displayed by default
# but this info is still stored in the POSIXct object
df.quit.dates[["start.study.hrts"]] <- as.POSIXct(strptime(df.quit.dates[["start.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
df.quit.dates[["end.study.hrts"]] <- as.POSIXct(strptime(df.quit.dates[["end.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
df.quit.dates[["quit.hrts"]] <- as.POSIXct(strptime(df.quit.dates[["quit.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# Convert human-readable timestamps to UNIX timestamps
df.quit.dates <- df.quit.dates %>%
  mutate(start.study.unixts = as.numeric(start.study.hrts),
         end.study.unixts = as.numeric(end.study.hrts),
         quit.unixts = as.numeric(quit.hrts)) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts,
         start.study.unixts, quit.unixts, end.study.unixts, everything())

#------------------------------------------------------------------------------
# Read in raw data (already contains time variables at the EMA-level)
# Loading this RData file will add list.all to the global environment
#------------------------------------------------------------------------------
load(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

# Before proceeding, we create an empty list to store timestamps of 
# 'button presses'. This includes moments when an individual performs a
# 'button press' to indicate that they were about to smoke, but the
# smart phone's software did not deliver either a Pre-Quit Smoking Part One EMA
# or a Post-Quit About to Slip Part One EMA; in this case, the smart phone's
# software decides to deliver either of these EMAs based on a calculated
# sampling probability
list.bp <- list()

#------------------------------------------------------------------------------
# Merge quit date data with raw data; quit date data contains time variables
# at the person-level
#------------------------------------------------------------------------------
list.all <- lapply(list.all, function(this.df, use.quit.dates = df.quit.dates){
  this.df <- left_join(x = use.quit.dates, y = this.df, by = "id")
  return(this.df)
})

#------------------------------------------------------------------------------
# Implement inclusion/exclusion criteria applicable to all datasets
# and create variables to use as filtering criteria in all datasets
#------------------------------------------------------------------------------
list.all <- lapply(list.all, function(this.df){
  
  this.df <- this.df %>%
    # Exclude all of a participant's data if they are tagged as exclude==1
    filter(exclude==0) %>% 
    # Exclude EMAs delivered before start study time and EMAs delivered after
    # end study time
    filter((delivered.unixts>=start.study.unixts) & (delivered.unixts<=end.study.unixts)) %>% 
    # Exclude EMAs having any indication of a tech issue
    filter(!(with.any.response==0 & record.status=="Completed")) %>%
    filter(!(with.any.response==0 & record.status=="FRAGMENT RECORD"))
  
  this.df <- this.df %>%
    # Tag each EMA as being viewed as delivered during the Pre-Quit period
    # or delivered during the Post-Quit Period
    mutate(use.as.postquit = if_else(delivered.unixts>=quit.unixts, 1, 0))
  
  # Clean up
  this.df <- this.df %>% 
    select(-exclude) %>%
    select(id, callnumr, 
           start.study.hrts, quit.hrts, end.study.hrts, 
           start.study.unixts, quit.unixts, end.study.unixts,
           record.id, assessment.type, 
           use.as.postquit, sensitivity,
           delivered.hrts, begin.hrts, end.hrts, time.hrts,
           delivered.unixts, begin.unixts, end.unixts, time.unixts,
           record.status, with.any.response,
           everything())
  
  return(this.df)
})

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Post-Quit About to Slip Part One
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Post-Quit About to Slip Part One`

# Get the button presses
df.buttonpress <- current.clean.df %>%
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.bp <- append(list.bp, list(`Post-Quit About to Slip Part One` = df.buttonpress))

# Retain rows having a response to any item
df.ema <- current.clean.df %>% filter(with.any.response==1) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

# Update cleaned data
list.all$`Post-Quit About to Slip Part One` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Pre-Quit Smoking Part One
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Pre-Quit Smoking Part One`

# Get the button presses
df.buttonpress <- current.clean.df %>%
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.bp <- append(list.bp, list(`Pre-Quit Smoking Part One` = df.buttonpress))

# Retain rows having a response to any item
df.ema <- current.clean.df %>% filter(with.any.response==1) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

# Update cleaned data
list.all$`Pre-Quit Smoking Part One` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Post-Quit About to Slip Part Two
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Post-Quit About to Slip Part Two`

df.ema <- current.clean.df %>% filter(with.any.response==1) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

list.all$`Post-Quit About to Slip Part Two` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Pre-Quit About to Slip Part Two
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Pre-Quit Smoking Part Two`

df.ema <- current.clean.df %>% filter(with.any.response==1) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

list.all$`Pre-Quit Smoking Part Two` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Post-Quit Urge
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Post-Quit Urge`

df.ema <- current.clean.df %>% filter(with.any.response==1) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

list.all$`Post-Quit Urge` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Pre-Quit Urge
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Pre-Quit Urge`

df.ema <- current.clean.df %>% filter(with.any.response==1) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

list.all$`Pre-Quit Urge` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Post-Quit Already Slipped
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Post-Quit Already Slipped`

df.ema <- current.clean.df %>% filter(with.any.response==1) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

list.all$`Post-Quit Already Slipped` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Post-Quit Random EMA
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Post-Quit Random`

df.ema <- current.clean.df %>% 
  filter(!(with.any.response==0 & record.status=="COMPLETED")) %>%
  filter(!(with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         with.any.response, record.status,
         everything())

list.all$`Post-Quit Random` <- df.ema

#------------------------------------------------------------------------------
# Go to specific types of EMA:
# Pre-Quit Random EMA
#------------------------------------------------------------------------------
current.clean.df <- list.all$`Pre-Quit Random`

df.ema <- current.clean.df %>% 
  filter(!(with.any.response==0 & record.status=="COMPLETED")) %>%
  filter(!(with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response,
         everything())

list.all$`Pre-Quit Random` <- df.ema

#------------------------------------------------------------------------------
# Format data frames prior to writing to csv file: EMA data
#------------------------------------------------------------------------------

# Display time variables in year-month-day hour-minute-second format
for(i in 1:length(list.all)){
  
  this.df <- list.all[[i]]
  
  # Use argument tz="UTC" or else %H:%M:%S will not be displayed as 00:00:00 for start.study.hrts
  # and end.study.hrts, and 04:00:00 will not be displayed for quit.hrts
  # This trick prevents R from performing an automatic adjustment of these time variables
  # to local time of machine in the output file
  this.df[["start.study.hrts"]] <- strftime(this.df[["start.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["end.study.hrts"]] <- strftime(this.df[["end.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["quit.hrts"]] <- strftime(this.df[["quit.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  
  this.df[["begin.hrts"]] <- strftime(this.df[["begin.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["end.hrts"]] <- strftime(this.df[["end.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["time.hrts"]] <- strftime(this.df[["time.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  
  list.all[[i]] <- this.df
}

# Change periods in column names to underscrores
# This is mostly to accomodate a wider range of end-users of the curated data
# who may use a varied range of data analysis software, some of which
# make it easier to work with column names with an underscore (rather than
# dots) than others

for(i in 1:length(list.all)){
  
  this.df <- list.all[[i]]
  
  this.df <- this.df %>%
    rename(start_study_hrts = start.study.hrts,
           end_study_hrts = end.study.hrts,
           quit_hrts = quit.hrts,
           start_study_unixts = start.study.unixts,
           end_study_unixts = end.study.unixts,
           quit_unixts = quit.unixts,
           record_id = record.id,
           assessment_type = assessment.type,
           use_as_postquit = use.as.postquit,
           with_any_response = with.any.response,
           record_status = record.status,
           delivered_hrts = delivered.hrts,
           begin_hrts = begin.hrts,
           end_hrts = end.hrts,
           time_hrts = time.hrts,
           delivered_unixts = delivered.unixts,
           begin_unixts = begin.unixts,
           end_unixts = end.unixts,
           time_unixts = time.unixts)
  
  list.all[[i]] <- this.df
}

#------------------------------------------------------------------------------
# Format data frames prior to writing to csv file: button press data
#------------------------------------------------------------------------------

# Display time variables in year-month-day hour-minute-second format
for(i in 1:length(list.bp)){
  
  this.df <- list.bp[[i]]
  
  this.df[["start.study.hrts"]] <- strftime(this.df[["start.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["end.study.hrts"]] <- strftime(this.df[["end.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["quit.hrts"]] <- strftime(this.df[["quit.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  
  this.df[["begin.hrts"]] <- strftime(this.df[["begin.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["end.hrts"]] <- strftime(this.df[["end.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["time.hrts"]] <- strftime(this.df[["time.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  
  list.bp[[i]] <- this.df
}

# Combine all button press data frames into one big data frame
longdf.bp <- bind_rows(list.bp)
longdf.bp <- longdf.bp %>% arrange(id, start.study.unixts, time.unixts)

# Change periods in column names to underscrores
# This is mostly to accomodate a wider range of end-users of the curated data
# who may use a varied range of data analysis software, some of which
# make it easier to work with column names with an underscore (rather than
# dots) than others
longdf.bp <- longdf.bp %>%
  rename(start_study_hrts = start.study.hrts,
         end_study_hrts = end.study.hrts,
         quit_hrts = quit.hrts,
         start_study_unixts = start.study.unixts,
         end_study_unixts = end.study.unixts,
         quit_unixts = quit.unixts,
         record_id = record.id,
         assessment_type = assessment.type,
         use_as_postquit = use.as.postquit,
         with_any_response = with.any.response,
         record_status = record.status,
         delivered_hrts = delivered.hrts,
         begin_hrts = begin.hrts,
         end_hrts = end.hrts,
         time_hrts = time.hrts,
         delivered_unixts = delivered.unixts,
         begin_unixts = begin.unixts,
         end_unixts = end.unixts,
         time_unixts = time.unixts)

write.csv(longdf.bp, file.path(path.pns.output_data, "button_press_smoking.csv"), na="", row.names = FALSE)

#------------------------------------------------------------------------------
# Save data corresponding to each EMA type into a csv file of its own
#------------------------------------------------------------------------------

write.csv(list.all[["Pre-Quit Random"]], file.path(path.pns.output_data, "pre_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Urge"]], file.path(path.pns.output_data, "pre_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Smoking Part One"]], file.path(path.pns.output_data, "pre_quit_smoking_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Smoking Part Two"]], file.path(path.pns.output_data, "pre_quit_smoking_part_two_ema.csv"), na="", row.names = FALSE)

write.csv(list.all[["Post-Quit Random"]], file.path(path.pns.output_data, "post_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit Urge"]], file.path(path.pns.output_data, "post_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit About to Slip Part One"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit About to Slip Part Two"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_two_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit Already Slipped"]], file.path(path.pns.output_data, "post_quit_already_slipped_ema.csv"), na="", row.names = FALSE)

#------------------------------------------------------------------------------
# Reformat column names of quit_dates_final from using periods to underscores
#------------------------------------------------------------------------------

df.quit.dates[["start.study.hrts"]] <- strftime(df.quit.dates[["start.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.quit.dates[["end.study.hrts"]] <- strftime(df.quit.dates[["end.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.quit.dates[["quit.hrts"]] <- strftime(df.quit.dates[["quit.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)

df.quit.dates <- df.quit.dates %>%
  rename(start_study_hrts = start.study.hrts,
         end_study_hrts = end.study.hrts,
         quit_hrts = quit.hrts,
         start_study_unixts = start.study.unixts,
         end_study_unixts = end.study.unixts,
         quit_unixts = quit.unixts)

write.csv(df.quit.dates, file.path(path.pns.output_data, "quit_dates_final_reformatted_column_names.csv"), row.names=FALSE, na="")

