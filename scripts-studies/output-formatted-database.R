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

# Add documentation on column names
docs <- data.frame(dataset = "ema_item_names.csv", columns = colnames(ema.item.names), description = NA, stringsAsFactors = FALSE)
docs <- docs %>%
  mutate(description = replace(description, columns == "is_postquit_assessment_type", "1: Post-Quit Mode type of EMA; 0: Pre-Quit Mode type of EMA")) %>%
  mutate(description = replace(description, columns == "assessment_type", "kind of EMA; there are a total of 9 kinds of EMA")) %>%
  mutate(description = replace(description, columns == "name_codebook", paste("original variable name in the raw data;",
                                                                              " can be cross-referenced against variable name in codebook", sep=""))) %>%
  mutate(description = replace(description, columns == "name_new", paste("new names given to variables in curated datasets where data from two or more kinds of EMA are merged;",
                                                                         " the format used in name_new enables end-users to determine the particular kind of EMA",
                                                                         " in which the value in the merged dataset was originally provided", sep="")))

write.csv(docs, file.path(path.pns.output_data, "docs_ema_item_names.csv"), row.names = FALSE)
rm(docs)

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

# Add documentation on column names
docs <- data.frame(dataset = "quit_dates_final.csv", columns = colnames(df.quit.dates), description = NA, stringsAsFactors = FALSE)
docs <- docs %>%
  mutate(description = replace(description, columns == "id", "Participant ID utilized in EMA raw data")) %>%
  mutate(description = replace(description, columns == "callnumr", "Participant ID utilized in baseline raw data")) %>%
  mutate(description = replace(description, columns == "start_study_hrts", paste("Start Date and Time to be used in data analysis;",
                                                                                 " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                                 sep=""))) %>%
  mutate(description = replace(description, columns == "quit_hrts", paste("Quit Date and Time to be used in data analysis;",
                                                                          " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                          sep=""))) %>%
  mutate(description = replace(description, columns == "end_study_hrts", paste("End Date and Time to be used in data analysis;",
                                                                               " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                               sep=""))) %>%
  mutate(description = replace(description, columns == "exclude", "equal to 1 if participant is to be excluded from all data analysis; 0 otherwise")) %>%
  mutate(description = replace(description, columns == "sensitivity", "equal to 1 if participant will be utilized in sensitivity analysis; 0 otherwise"))

write.csv(docs, file.path(path.pns.output_data, "docs_quit_dates_final.csv"), row.names = FALSE)
rm(docs)

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

# Add documentation on column names

for(i in 1:length(list.clean.launched)){
  curr.df <- list.clean.launched[[i]]
  curr.ema.type <- names(list.clean.launched)[i]
  curr.df.name <- case_when(
    # Pre-Quit EMA Types
    curr.ema.type == "Pre-Quit Random" ~ "pre_quit_random_ema.csv",
    curr.ema.type == "Pre-Quit Urge" ~ "pre_quit_urge_ema.csv",
    curr.ema.type == "Pre-Quit Smoking Part One" ~ "pre_quit_smoking_part_one_ema.csv",
    curr.ema.type == "Pre-Quit Smoking Part Two" ~ "pre_quit_smoking_part_two_ema.csv",
    # Post-Quit EMA types
    curr.ema.type == "Post-Quit Random" ~ "post_quit_random_ema.csv",
    curr.ema.type == "Post-Quit Urge" ~ "post_quit_urge_ema.csv",
    curr.ema.type == "Post-Quit About to Slip Part One" ~ "post_quit_about_to_slip_part_one_ema.csv",
    curr.ema.type == "Post-Quit About to Slip Part Two" ~ "post_quit_about_to_slip_part_two_ema.csv",
    curr.ema.type == "Post-Quit Already Slipped" ~ "post_quit_already_slipped_ema.csv",
    TRUE ~ NA_character_
  )
  
  docs <- data.frame(dataset = curr.df.name, columns = colnames(curr.df), description = NA, stringsAsFactors = FALSE)
  docs <- docs %>%
    mutate(description = replace(description, columns == "id", "Participant ID utilized in EMA raw data")) %>%
    mutate(description = replace(description, columns == "callnumr", "Participant ID utilized in baseline raw data")) %>%
    # Describe time variables relating to time in study in human-readable format
    mutate(description = replace(description, columns == "start_study_hrts", paste("Start Date and Time to be used in data analysis;",
                                                                                   " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                                   sep=""))) %>%
    mutate(description = replace(description, columns == "quit_hrts", paste("Quit Date and Time to be used in data analysis;",
                                                                            " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                            sep=""))) %>%
    mutate(description = replace(description, columns == "end_study_hrts", paste("End Date and Time to be used in data analysis;",
                                                                                 " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                                 sep=""))) %>%
    mutate(description = replace(description, columns == "start_study_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                                     " time in UNIX timestamp format",
                                                                                     " (no. seconds elapsed since January 1, 1970);",
                                                                                     " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
    mutate(description = replace(description, columns == "quit_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                              " time in UNIX timestamp format",
                                                                              " (no. seconds elapsed since January 1, 1970);",
                                                                              " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
    mutate(description = replace(description, columns == "end_study_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                                   " time in UNIX timestamp format",
                                                                                   " (no. seconds elapsed since January 1, 1970);",
                                                                                   " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
    # Describe variable relating to participant
    mutate(description = replace(description, columns == "sensitivity", "equal to 1 if participant will be utilized in sensitivity analysis; 0 otherwise")) %>%
    # Describe variables relating to EMA questionnaire
    mutate(description = replace(description, columns == "record_id", "unique identifier for EMA questionnaire")) %>%
    mutate(description = replace(description, columns == "assessment_type", "kind of EMA questionnaire; there are nine kinds of questionnaires in total")) %>%
    mutate(description = replace(description, columns == "use_as_postquit", "equal to 1 if questionnaire is to be used as post-quit observation in data analysis; 0 if questionnaire is to be used as pre-quit observation in data analysis")) %>%
    # Describe variables relating to items within EMA questionnaire
    mutate(description = replace(description, columns == "delivered_hrts", paste("Generally, this refers to the time when an EMA questionnaire was delivered/initiated to/by a participant;",
                                                                                 " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                                 " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                                 sep=""))) %>%
    mutate(description = replace(description, columns == "begin_hrts", paste("Generally, this refers to the time when participant began responding to items in the EMA questionnaire;",
                                                                             " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                             " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                             sep=""))) %>%
    mutate(description = replace(description, columns == "end_hrts", paste("Generally, this refers to the time when participant finished responding to all items in the EMA questionnaire;",
                                                                           " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                           " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                           sep=""))) %>%
    mutate(description = replace(description, columns == "time_hrts", paste("Generally: equal to begin_hrts if with_any_response=1; equal to delivered_hrts if with_any_response=0;",
                                                                            " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                            " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                            sep=""))) %>%
    mutate(description = replace(description, columns == "delivered_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                                   " time in UNIX timestamp format",
                                                                                   " (no. seconds elapsed since January 1, 1970);",
                                                                                   " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
    mutate(description = replace(description, columns == "begin_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                               " time in UNIX timestamp format",
                                                                               " (no. seconds elapsed since January 1, 1970);",
                                                                               " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
    mutate(description = replace(description, columns == "end_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                             " time in UNIX timestamp format",
                                                                             " (no. seconds elapsed since January 1, 1970);",
                                                                             " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
    mutate(description = replace(description, columns == "time_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                              " time in UNIX timestamp format",
                                                                              " (no. seconds elapsed since January 1, 1970);",
                                                                              " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
    mutate(description = replace(description, columns == "record_status", paste("A variable in the raw data used to determine whether a particular EMA questionnaire",
                                                                                " should be retained or excluded in curated database; See documentation for more details",
                                                                                sep=""))) %>%
    mutate(description = replace(description, columns == "with_any_response", paste("Equal to 1 if there is a record of a response to at least one item in EMA questionnaire;",
                                                                                    "Equal to 0 if there is no record of a response to any of the items in EMA questionnaire",
                                                                                    sep=""))) %>%
    mutate(description = replace(description, is.na(description), "Refer to the raw data codebook file PNS EMA Codebook 07202010.docx; values in this column are left unmodified from the raw data"))
  
  write.csv(docs, file.path(path.pns.output_data, paste("docs_", curr.df.name, sep="")), row.names = FALSE)
  rm(docs)
}

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

# Update df.buttonpress
df.buttonpress <- this.df %>% filter(assessment_type == "Pre-Quit Smoking Part One" | assessment_type == "Post-Quit About to Slip Part One")
write.csv(df.buttonpress, file.path(path.pns.output_data, "buttonpress_smoking_part_one.csv"), na="", row.names = FALSE)

# Add documentation on column names

docs <- data.frame(dataset = "buttonpress_smoking_part_one.csv", columns = colnames(df.buttonpress), description = NA, stringsAsFactors = FALSE)
docs <- docs %>%
  mutate(description = replace(description, columns == "id", "Participant ID utilized in EMA raw data")) %>%
  mutate(description = replace(description, columns == "callnumr", "Participant ID utilized in baseline raw data")) %>%
  # Describe time variables relating to time in study in human-readable format
  mutate(description = replace(description, columns == "start_study_hrts", paste("Start Date and Time to be used in data analysis;",
                                                                                 " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                                 sep=""))) %>%
  mutate(description = replace(description, columns == "quit_hrts", paste("Quit Date and Time to be used in data analysis;",
                                                                          " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                          sep=""))) %>%
  mutate(description = replace(description, columns == "end_study_hrts", paste("End Date and Time to be used in data analysis;",
                                                                               " Date is in the human-readable timestamp format YYY-MM-DD H:M:S",
                                                                               sep=""))) %>%
  mutate(description = replace(description, columns == "start_study_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                                   " time in UNIX timestamp format",
                                                                                   " (no. seconds elapsed since January 1, 1970);",
                                                                                   " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
  mutate(description = replace(description, columns == "quit_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                            " time in UNIX timestamp format",
                                                                            " (no. seconds elapsed since January 1, 1970);",
                                                                            " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
  mutate(description = replace(description, columns == "end_study_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                                 " time in UNIX timestamp format",
                                                                                 " (no. seconds elapsed since January 1, 1970);",
                                                                                 " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
  # Describe variable relating to participant
  mutate(description = replace(description, columns == "sensitivity", "equal to 1 if participant will be utilized in sensitivity analysis; 0 otherwise")) %>%
  # Describe variables relating to EMA questionnaire
  mutate(description = replace(description, columns == "record_id", "unique identifier for EMA questionnaire")) %>%
  mutate(description = replace(description, columns == "assessment_type", "kind of EMA questionnaire; there are nine kinds of questionnaires in total")) %>%
  mutate(description = replace(description, columns == "use_as_postquit", "equal to 1 if questionnaire is to be used as post-quit observation in data analysis; 0 if questionnaire is to be used as pre-quit observation in data analysis")) %>%
  # Describe variables relating to items within EMA questionnaire
  mutate(description = replace(description, columns == "delivered_hrts", paste("Generally, this refers to the time when an EMA questionnaire was delivered/initiated to/by a participant;",
                                                                               " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                               " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                               sep=""))) %>%
  mutate(description = replace(description, columns == "begin_hrts", paste("Generally, this refers to the time when participant began responding to items in the EMA questionnaire;",
                                                                           " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                           " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                           sep=""))) %>%
  mutate(description = replace(description, columns == "end_hrts", paste("Generally, this refers to the time when participant finished responding to all items in the EMA questionnaire;",
                                                                         " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                         " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                         sep=""))) %>%
  mutate(description = replace(description, columns == "time_hrts", paste("Generally: equal to begin_hrts if with_any_response=1; equal to delivered_hrts if with_any_response=0;",
                                                                          " Date is in the human-readable timestamp format YYY-MM-DD H:M:S;",
                                                                          " Refer to documentation for details on possible exceptions to this rule of thumb description.",
                                                                          sep=""))) %>%
  mutate(description = replace(description, columns == "delivered_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                                 " time in UNIX timestamp format",
                                                                                 " (no. seconds elapsed since January 1, 1970);",
                                                                                 " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
  mutate(description = replace(description, columns == "begin_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                             " time in UNIX timestamp format",
                                                                             " (no. seconds elapsed since January 1, 1970);",
                                                                             " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
  mutate(description = replace(description, columns == "end_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                           " time in UNIX timestamp format",
                                                                           " (no. seconds elapsed since January 1, 1970);",
                                                                           " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
  mutate(description = replace(description, columns == "time_unixts", paste("Same as similarly-named variable suffixed by _unixts;",
                                                                            " time in UNIX timestamp format",
                                                                            " (no. seconds elapsed since January 1, 1970);",
                                                                            " e.g., if UNIX time is 1610564920, use the following R code to recover start_study_hrts column: as.POSIXct(x=1610564920, origin=\"1970-01-01\", tz=\"GMT\")"))) %>%
  mutate(description = replace(description, columns == "record_status", paste("A variable in the raw data used to determine whether a particular EMA questionnaire",
                                                                              " should be retained or excluded in curated database; See documentation for more details",
                                                                              sep=""))) %>%
  mutate(description = replace(description, columns == "with_any_response", paste("Equal to 1 if there is a record of a response to at least one item in EMA questionnaire;",
                                                                                  "Equal to 0 if there is no record of a response to any of the items in EMA questionnaire",
                                                                                  sep="")))

write.csv(docs, file.path(path.pns.output_data, paste("docs_", "buttonpress_smoking_part_one.csv", sep="")), row.names = FALSE)
rm(docs)

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
         hours_between_past_and_present = hours.between.past.and.present)

write.csv(df.smoking, file.path(path.pns.output_data, "smoking.csv"), row.names=FALSE, na="")



