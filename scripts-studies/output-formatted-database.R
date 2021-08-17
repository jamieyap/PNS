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
# Work with df.baseline
#------------------------------------------------------------------------------

df.baseline <- readRDS(file.path(path.pns.staged_data, "baseline.RData"))
write.csv(df.baseline, file.path(path.pns.output_data, "baseline.csv"), na="", row.names = FALSE)

#------------------------------------------------------------------------------
# Format output of: create-dictionary.R
# Output was: ema_item_names.RData
#
# Change periods in column names to underscores
# This is mostly to accommodate a wider range of end-users of the curated data
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
  select(-rawdata_indicator, -rawdata_qty, -rawdata_timing,
         -time_hrts_shift_minus_1, -time_unixts_shift_minus_1, -assessment_type_shift_minus_1,
         -record_id_shift_minus_1, -hours_between_past_and_present) %>%
  select(-start_study_unixts, -end_study_unixts, -quit_unixts, -delivered_unixts, -begin_unixts, -end_unixts, -time_unixts)

write.csv(df.smoking, file.path(path.pns.output_data, "smoking.csv"), row.names=FALSE, na="")



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
           time_unixts = time.unixts) %>%
    select(-start_study_unixts, -end_study_unixts, -quit_unixts, -delivered_unixts, -begin_unixts, -end_unixts, -time_unixts)
  
  list.clean.launched[[i]] <- this.df
}


# Merging step within each kind of EMA: match curated smoking data with EMA data via record_id

for(i in 1:length(list.clean.launched)){
  
  this.df <- list.clean.launched[[i]]
  
  subset.df.smoking <- df.smoking %>% select(id, callnumr, record_id, assessment_type, 
                                             smoking_qty, smoking_indicator, smoking_delta_minutes,
                                             ema_order)
  
  this.df <- left_join(x = this.df, y = subset.df.smoking, by = c("id", "callnumr", "record_id", "assessment_type"))
  this.df <- this.df %>% 
    select(id, callnumr, start_study_hrts, quit_hrts, end_study_hrts, 
           record_id,
           assessment_type, use_as_postquit, sensitivity, 
           delivered_hrts, begin_hrts, end_hrts, time_hrts,
           record_status, with_any_response, 
           ema_order, smoking_qty, smoking_indicator, smoking_delta_minutes,
           everything())
  
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
         time_unixts = time.unixts) %>%
  select(-start_study_unixts, -end_study_unixts, -quit_unixts, -delivered_unixts, -begin_unixts, -end_unixts, -time_unixts)

# Partition out records attributed to button press
df.buttonpress <- this.df %>% filter(assessment_type == "Pre-Quit Smoking Part One" | assessment_type == "Post-Quit About to Slip Part One")
write.csv(df.buttonpress, file.path(path.pns.output_data, "buttonpress_smoking.csv"), na="", row.names = FALSE)

# Partition out records attributed to unknown tech issue
df.unknown.techissue <- this.df %>% filter(!(assessment_type == "Pre-Quit Smoking Part One" | assessment_type == "Post-Quit About to Slip Part One"))
write.csv(df.unknown.techissue, file.path(path.pns.output_data, "unknown_techissue.csv"), na="", row.names = FALSE)


#------------------------------------------------------------------------------
# Merging step across different kinds of EMA
#------------------------------------------------------------------------------


# Prepare to merge data:
# Change column names in each data frame within list.clean.launched 
# to new column names in ema.item.names

list.all <- list.clean.launched

for(k in 1:length(list.all)){
  current.assessment.type <- names(list.all)[k]
  df.processed <- list.all[[current.assessment.type]]
  old.column.names <- colnames(df.processed)
  these.old <- ema.item.names %>% filter(assessment_type==current.assessment.type) %>% extract2("name_codebook")
  these.new <- ema.item.names %>% filter(assessment_type==current.assessment.type) %>% extract2("name_new")
  
  for(i in 1:length(old.column.names)){
    if(old.column.names[i] %in% these.old){  # if TRUE, then old.column.names[i] corresponds to an EMA item
      idx <- which(these.old==old.column.names[i])  # search for the new column name based on the dictionary of old and new column names
      colnames(df.processed)[i] <- these.new[idx]  # rename ith column of df.processed
    }else{
      next  # do NOT rename ith column of df.processed
    }
  }
  # Update df.processed with new column names
  list.all[[current.assessment.type]] <- df.processed
}


# Transform list.all from a list into a data frame

collect.reshaped.df <- list()
all.names <- ema.item.names$name_new

for(i in 1:length(list.all)){
  current.df <- list.all[[i]]
  
  # Which names in all.names are not current.df column names
  add.these.colnames <- setdiff(all.names, colnames(current.df))
  current.df[,add.these.colnames] <- NA
  
  # Reorder columns
  current.df <- current.df %>% 
    select(id, callnumr, 
           start_study_hrts, quit_hrts, end_study_hrts, 
           sensitivity,
           record_id, assessment_type, 
           use_as_postquit, with_any_response,
           delivered_hrts, begin_hrts, end_hrts, time_hrts,
           ema_order, smoking_qty, smoking_indicator, smoking_delta_minutes,
           all_of(all.names))
  
  # Collect reshaped data frame
  collect.reshaped.df <- append(collect.reshaped.df, list(current.df))
}


BIG.df <- do.call(rbind, collect.reshaped.df)

# Save big merged dataset to csv file
write.csv(BIG.df, file.path(path.pns.output_data, "merged.csv"), row.names = FALSE, na="")




