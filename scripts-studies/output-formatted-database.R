###############################################################################
# ABOUT:
# * Format and save RData files into stand-alone or merged databases
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")

#------------------------------------------------------------------------------
# Format output of: create-dictionary.R
# Output was: ema_item_names.RData
#
# Change periods in column names to underscrores
# This is mostly to accomodate a wider range of end-users of the curated data
# who may use a varied range of data analysis software, some of which
# make it easier to work with column names with an underscore (rather than
# dots) than others
#------------------------------------------------------------------------------

ema.item.names <- readRDS(file.path(path.pns.staged_data, "ema_item_names.RData"))

ema.item.names <- ema.item.names %>%
  rename(is_postquit_assessment_type = is.postquit.assessment.type,
         assessment_type = assessment.type,
         name_codebook = name.codebook,
         name_new = name.new)

write.csv(ema.item.names, file.path(path.pns.output_data, "ema_item_names.csv"), row.names = FALSE)



#------------------------------------------------------------------------------
# Format output of: calc-quit-dates.R
# Output was: quit_dates_final.RData
#
# Format quit dates prior to writing to csv file
# Use argument tz="UTC" or else %H:%M:%S will not be displayed as 00:00:00 for start.study.date
# and end.study.date, and 04:00:00 will not be displayed for quit.date in quit_dates_final.csv
# This trick prevents R from performing an automatic adjustment of these time variables
# to local time of machine in the output file quit_dates_final.csv
#------------------------------------------------------------------------------

df.quit.dates <- readRDS(file.path(path.pns.staged_data, "quit_dates_final.RData"))

df.quit.dates[["start.study.date"]] <- strftime(df.quit.dates[["start.study.date"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.quit.dates[["end.study.date"]] <- strftime(df.quit.dates[["end.study.date"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.quit.dates[["quit.date"]] <- strftime(df.quit.dates[["quit.date"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)

df.quit.dates <- df.quit.dates %>%
  rename(start.study.hrts = start.study.date, end.study.hrts = end.study.date, quit.hrts = quit.date) %>%
  rename(start_study_hrts = start.study.hrts,
         end_study_hrts = end.study.hrts,
         quit_hrts = quit.hrts)

write.csv(df.quit.dates, file.path(path.pns.output_data, "quit_dates_final.csv"), row.names=FALSE, na="")



#------------------------------------------------------------------------------
# Format output of: clean-ema.R
# Output was: clean_launched.RData, dirty_launched.RData, buttonpress.RData
#------------------------------------------------------------------------------
list.clean.launched <- readRDS(file.path(path.pns.staged_data, "clean_launched.RData"))
df.dirty.launched <- readRDS(file.path(path.pns.staged_data, "dirty_launched.RData"))
df.buttonpress <- readRDS(file.path(path.pns.staged_data, "buttonpress.RData"))

#------------------------------------------------------------------------------
# Work with list.clean.launched
#------------------------------------------------------------------------------
# Display time variables in year-month-day hour-minute-second format
for(i in 1:length(list.clean.launched)){
  
  this.df <- list.clean.launched[[i]]
  
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
  
  list.clean.launched[[i]] <- this.df
}

for(i in 1:length(list.clean.launched)){
  
  this.df <- list.clean.launched[[i]]
  
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
  
  list.clean.launched[[i]] <- this.df
}

write.csv(list.clean.launched[["Pre-Quit Random"]], file.path(path.pns.output_data, "pre_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.clean.launched[["Pre-Quit Urge"]], file.path(path.pns.output_data, "pre_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.clean.launched[["Pre-Quit Smoking Part One"]], file.path(path.pns.output_data, "pre_quit_smoking_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.clean.launched[["Pre-Quit Smoking Part Two"]], file.path(path.pns.output_data, "pre_quit_smoking_part_two_ema.csv"), na="", row.names = FALSE)

write.csv(list.clean.launched[["Post-Quit Random"]], file.path(path.pns.output_data, "post_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.clean.launched[["Post-Quit Urge"]], file.path(path.pns.output_data, "post_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.clean.launched[["Post-Quit About to Slip Part One"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.clean.launched[["Post-Quit About to Slip Part Two"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_two_ema.csv"), na="", row.names = FALSE)
write.csv(list.clean.launched[["Post-Quit Already Slipped"]], file.path(path.pns.output_data, "post_quit_already_slipped_ema.csv"), na="", row.names = FALSE)



#------------------------------------------------------------------------------
# Work with df.buttonpress
#------------------------------------------------------------------------------
this.df <- df.buttonpress

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

# Update df.buttonpress
df.buttonpress <- this.df

this.df <- df.buttonpress

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

# Partition out records attributed to button press
df.buttonpress <- this.df %>% filter(assessment_type == "Pre-Quit Smoking Part One" | assessment_type == "Post-Quit About to Slip Part One")
write.csv(df.buttonpress, file.path(path.pns.output_data, "buttonpress_smoking.csv"), na="", row.names = FALSE)

# Partition out records attributed to unknown tech issue
df.unknown.techissue <- this.df %>% filter(!(assessment_type == "Pre-Quit Smoking Part One" | assessment_type == "Post-Quit About to Slip Part One"))
write.csv(df.unknown.techissue, file.path(path.pns.output_data, "unknown_techissue.csv"), na="", row.names = FALSE)



#------------------------------------------------------------------------------
# Format output of: create-database-smoking.R
# Output was: curated_smoking_database.RData
#------------------------------------------------------------------------------

df.smoking <- readRDS(file.path(path.pns.staged_data, "curated_smoking_database.RData"))

df.smoking <- df.smoking %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts,
         start.study.unixts, quit.unixts, end.study.unixts,
         sensitivity, record.id, assessment.type, use.as.postquit,
         with.any.response, delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.id_shift_minus_1, assessment.type_shift_minus_1, 
         time.hrts_shift_minus_1, time.unixts_shift_minus_1,
         hours.between.past.and.present,
         everything()) %>%
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
         time_unixts = time.unixts,
         rawdata_indicator = rawdata.indicator,
         rawdata_qty = rawdata.qty,
         rawdata_timing = rawdata.timing,
         smoking_qty = smoking.qty,
         smoking_indicator = smoking.indicator,
         smoking_delta_minutes = smoking.delta.minutes,
         ema_order = ema.order,
         time_hrts_shift_minus_1 = time.hrts_shift_minus_1,
         time_unixts_shift_minus_1 = time.unixts_shift_minus_1,
         assessment_type_shift_minus_1 = assessment.type_shift_minus_1,
         record_id_shift_minus_1 = record.id_shift_minus_1,
         hours_between_past_and_present = hours.between.past.and.present) %>%
  select(-rawdata_indicator, -rawdata_qty, -rawdata_timing)

write.csv(df.smoking, file.path(path.pns.output_data, "smoking.csv"), row.names=FALSE, na="")



