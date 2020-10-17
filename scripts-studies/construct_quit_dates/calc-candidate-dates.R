###############################################################################
# ABOUT:
# * For each individual calculate the following:
#   (a) timestamp corresponding to when the very first EMA in 
#       Post-Quit Mode was initiated/delivered
#   (b) timestamp corresponding to when the very last EMA in
#       Pre-Quit Mode was initiated/delivered
#   (c) date recorded in the variable 'quitday' in baseline raw data file
#   (d) date recorded in the variable 'EMA_Qday' in records of study staff
#
# * Perform the following checks on the time variables above:
#   - check whether all three dates in (a), (c), (d) fall on the same day
#   - check whether time timestamps in (a) and (b) fall on the same date;
#     perform a sanity check as to whether (a) is indeed behind (b)
#
# * Output file contains a row for each individual; new columns corresponding
#   to time variables (a), (b), (c), (d) are created; 
#   time variables (a) and (b) are provided in two formats: 
#   a 'short format' where only the year-month-day is displayed and a
#   'long format' where, in addition to displaying the year-month-day,
#   hour-minute-second information is also displayed
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

#------------------------------------------------------------------------------
# Get dates from Post-Quit raw data files
#------------------------------------------------------------------------------
postquit.files <- c("Post_Quit_Random.csv",
                    "Post_Quit_Urge.csv",
                    "Post_Quit_About_to_Slip.csv",
                    "Post_Quit_About_to_Slip_Part2.csv",
                    "Post_Quit_Already_Slipped.csv")

list.collect.postquit <- list()

for(i in 1:length(postquit.files)){
  this.file <- postquit.files[i]
  
  df.raw <- read.csv(file.path(path.pns.input_data, this.file), stringsAsFactors = FALSE)
  df.raw.quit.dates <- df.raw %>% 
    select(id = Part_ID, initiated=Initiated, assessment.type=Asse_Name) %>%
    # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
    mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p", tz = "UTC"))) %>%
    arrange(id, initiated) %>%
    group_by(id, assessment.type) %>%
    # earliest.date is of class POSIXct
    summarise(earliest.date = min(initiated))
  
  list.collect.postquit <- append(list.collect.postquit, list(df.raw.quit.dates))
  
  # Remove used variables from environment
  remove(df.raw, df.raw.quit.dates, this.file)
}

collect.postquit <- bind_rows(list.collect.postquit)
collect.postquit <- collect.postquit %>% group_by(id) %>% summarise(postquit.earliest.date=min(earliest.date))

#------------------------------------------------------------------------------
# Get dates from Pre-Quit raw data files
#------------------------------------------------------------------------------
prequit.files <- c("Pre_Quit_Random.csv",
                   "Pre_Quit_Urge.csv",
                   "Pre_Quit_Smoking.csv",
                   "Pre_Quit_Smoking_Part2.csv")

list.collect.prequit <- list()

for(i in 1:length(prequit.files)){
  this.file <- prequit.files[i]
  
  df.raw <- read.csv(file.path(path.pns.input_data, this.file), stringsAsFactors = FALSE)
  df.raw.quit.dates <- df.raw %>% 
    select(id = Part_ID, initiated=Initiated, assessment.type=Asse_Name) %>%
    # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
    mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p", tz = "UTC"))) %>%
    arrange(id, initiated) %>%
    group_by(id, assessment.type) %>%
    # latest.date is of class POSIXct
    summarise(latest.date = max(initiated))
  
  list.collect.prequit <- append(list.collect.prequit, list(df.raw.quit.dates))
  
  # Remove used variables from environment
  remove(df.raw, df.raw.quit.dates, this.file)
}

collect.prequit <- bind_rows(list.collect.prequit)
collect.prequit <- collect.prequit %>% group_by(id) %>% summarise(prequit.latest.date=max(latest.date))

#------------------------------------------------------------------------------
# Dates in records from study staff
#------------------------------------------------------------------------------
staff.recorded.dates <- read.csv(file.path(path.pns.input_data, "staff_recorded_dates.csv"), stringsAsFactors = FALSE)
staff.recorded.dates <- staff.recorded.dates %>% mutate(EMA_Qday = as.POSIXct(strptime(EMA_Qday, "%m/%d/%Y", tz = "UTC")))

#------------------------------------------------------------------------------
# Dates in baseline raw data file
#------------------------------------------------------------------------------
df.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"), stringsAsFactors = FALSE)
df.baseline.dates <- df.baseline %>% 
  select(callnumr, quitday) %>%
  mutate(quitday = as.POSIXct(strptime(quitday, "%m/%d/%Y", tz = "UTC")))

#------------------------------------------------------------------------------
# Merge several streams of data into one data frame
#------------------------------------------------------------------------------
df.alldates <- left_join(x = staff.recorded.dates, y = df.baseline.dates, by = "callnumr")
df.alldates <- left_join(x = df.alldates, y = collect.prequit, by = "id")
df.alldates <- left_join(x = df.alldates, y = collect.postquit, by = "id")

df.alldates <- df.alldates %>%
  mutate(prequit.latest.longformatdate = prequit.latest.date,
         postquit.earliest.longformatdate = postquit.earliest.date) %>%
  rename(prequit.latest.shortformatdate = prequit.latest.date,
         postquit.earliest.shortformatdate = postquit.earliest.date) %>%
  mutate(prequit.latest.shortformatdate = as.POSIXct(strftime(prequit.latest.shortformatdate, "%Y-%m-%d", tz = "UTC", usetz=FALSE)),
         postquit.earliest.shortformatdate = as.POSIXct(strftime(postquit.earliest.shortformatdate, "%Y-%m-%d", tz = "UTC", usetz=FALSE))) %>%
  mutate(prequit.latest.shortformatdate = as.character(prequit.latest.shortformatdate),
         postquit.earliest.shortformatdate = as.character(postquit.earliest.shortformatdate)) %>%
  mutate(prequit.latest.shortformatdate = as.POSIXct(strptime(prequit.latest.shortformatdate, "%Y-%m-%d", tz = "UTC")),
         postquit.earliest.shortformatdate = as.POSIXct(strptime(postquit.earliest.shortformatdate, "%Y-%m-%d", tz = "UTC")))

#------------------------------------------------------------------------------
# Compare time variables created above
#------------------------------------------------------------------------------

# CHECK: Are the values of the following three time variables identical?
# EMA_Qday, quitday, and postquit.earliest.shortformatdate
df.alldates <- df.alldates %>% 
  mutate(is.equal = case_when(
    is.na(postquit.earliest.shortformatdate) ~ NA_real_,
    EMA_Qday==quitday & EMA_Qday==postquit.earliest.shortformatdate ~ 1,
    TRUE ~ 0)) %>%
  arrange(is.equal)

# Among those individuals for whom the above three time variables
# do not match up, compare the date when the very last EMA in Pre-Quit Mode
# was delivered/initiated against the date when the very first EMA in
# Post-Quit Mode was initiated
df.alldates <- df.alldates %>%
  mutate(prepost.is.equal = if_else(is.equal==0 & !is.na(is.equal), 0, NA_real_)) %>%
  mutate(prepost.is.equal = replace(prepost.is.equal, (!is.na(prepost.is.equal)) & (prequit.latest.shortformatdate==postquit.earliest.shortformatdate), 1))

df.alldates <- df.alldates %>%
  mutate(prepost.is.lessthan = if_else(is.equal==0 & !is.na(is.equal), 0, NA_real_)) %>%
  mutate(prepost.is.lessthan = replace(prepost.is.lessthan, (!is.na(prepost.is.lessthan)) & (prequit.latest.shortformatdate < postquit.earliest.shortformatdate), 1))

df.alldates <- df.alldates %>%
  mutate(prepost.is.greaterthan = if_else(is.equal==0 & !is.na(is.equal), 0, NA_real_)) %>%
  mutate(prepost.is.greaterthan = replace(prepost.is.greaterthan, (!is.na(prepost.is.greaterthan)) & (prequit.latest.shortformatdate > postquit.earliest.shortformatdate), 1))

df.alldates <- df.alldates %>% arrange(is.equal, desc(prepost.is.equal), desc(prepost.is.lessthan), desc(prepost.is.greaterthan))
df.alldates <- df.alldates %>% mutate(num = 1:nrow(df.alldates)) %>% select(num, everything())

#------------------------------------------------------------------------------
# Write out to file
#------------------------------------------------------------------------------
saveRDS(df.alldates, file.path(path.pns.staged_data, "candidate_dates.RData"))


