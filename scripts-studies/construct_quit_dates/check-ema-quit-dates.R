###############################################################################
# ABOUT:
# * There are 5 raw data files corresponding to each type of EMA in 
#   Post-Quit Mode, and 4 raw data files corresponding to each type of EMA
#   in Pre-Quit Mode. Each of these raw data files has five variables
#   potentially providing information on quit date. These variables are
#   Quit_Date1, Quit_Date2, Quit_Date3, Quit_Date4, Quit_Date5
#
# * In this script, we perform sanity checks on these five variables.
#   Specifically,
#      - Are there missing values?
#      - For each individual, do the dates across all five variables differ 
#        within and across raw data files?
#      - Do these five date variables differ or match up with the
#        'EMA_Qday' variable in study staff recorded dates?
#      - Do these five date variables differ or match up with the
#        'quitday' variable in the baseline raw data file?
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
  df.raw.quit.dates <- df.raw %>% select(id = Part_ID, Quit_Date1, Quit_Date2, Quit_Date3, Quit_Date4, Quit_Date5)
  
  list.collect.postquit <- append(list.collect.postquit, list(df.raw.quit.dates))
}

df.collect.postquit <- bind_rows(list.collect.postquit)
df.collect.postquit.unique <- df.collect.postquit[!duplicated(df.collect.postquit),]

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
  df.raw.quit.dates <- df.raw %>% select(id = Part_ID, Quit_Date1, Quit_Date2, Quit_Date3, Quit_Date4, Quit_Date5)
  
  list.collect.prequit <- append(list.collect.prequit, list(df.raw.quit.dates))
}

df.collect.prequit <- bind_rows(list.collect.prequit)
df.collect.prequit.unique <- df.collect.prequit[!duplicated(df.collect.prequit),]

#------------------------------------------------------------------------------
# Combine dates from Pre-Quit raw data files and Post-Quit raw data files
#------------------------------------------------------------------------------
df.collect <- rbind(df.collect.prequit.unique, df.collect.postquit.unique)
df.collect <- df.collect[!duplicated(df.collect),]

#------------------------------------------------------------------------------
# CHECK:
#     - For each individual, are values of the five dates consistent across all 
#       nine raw data files?
#     - Are there missing values within each of the five date variables?
#     - For each individual, how do the dates in these five variables differ?
#------------------------------------------------------------------------------
nrow(df.collect)
sum(duplicated(df.collect$id))
sum(is.na(df.collect$Quit_Date1))
sum(is.na(df.collect$Quit_Date2))  
sum(is.na(df.collect$Quit_Date3)) 
sum(is.na(df.collect$Quit_Date4)) 
sum(is.na(df.collect$Quit_Date5)) 

# The above diagnostic shows that unlike Quit_Date1, the variables 
# Quit_Date2, Quit_Date3, Quit_Date4, Quit_Date5 do not have any recorded value
# at all in any of the raw data files

#------------------------------------------------------------------------------
# In preparation for further checking:
#   - Reformat Quit_Date1 to year-month-day format, matching the format
#     of 'quitday' and 'EMA_Qday'
#   - Read in 'quitday' from baseline raw data file
#   - Read in EMA_Qday from records of study staff
#------------------------------------------------------------------------------
df.collect <- df.collect %>% 
  mutate(Quit_Date1 = as.POSIXct(strptime(Quit_Date1, "%m/%d/%Y %H:%M:%S", tz = "UTC"))) %>%
  mutate(Quit_Date1 = strftime(Quit_Date1, "%m/%d/%Y", tz = "UTC")) %>%
  mutate(Quit_Date1 = as.POSIXct(strptime(Quit_Date1, "%m/%d/%Y", tz = "UTC")))

# Dates in records from study staff
staff.recorded.dates <- read.csv(file.path(path.pns.input_data, "staff_recorded_dates.csv"), stringsAsFactors = FALSE)
staff.recorded.dates <- staff.recorded.dates %>% mutate(EMA_Qday = as.POSIXct(strptime(EMA_Qday, "%m/%d/%Y", tz = "UTC")))

# Dates in baseline raw data file
df.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"), stringsAsFactors = FALSE)
df.baseline.dates <- df.baseline %>% 
  select(callnumr, quitday) %>%
  mutate(quitday = as.POSIXct(strptime(quitday, "%m/%d/%Y", tz = "UTC")))

# Merge several streams of data into one data frame
df.alldates <- left_join(x = staff.recorded.dates, y = df.baseline.dates, by = "callnumr")
df.alldates <- left_join(x = df.alldates, y = df.collect, by = "id")
df.alldates <- df.alldates %>% select(-Quit_Date2, -Quit_Date3, -Quit_Date4, -Quit_Date5)

#------------------------------------------------------------------------------
# CHECK:
#   For each individual,
#   - check whether EMA_Qday is equal to Quit_Date1
#   - check whether quitday is equal to quitday
#------------------------------------------------------------------------------
sum(df.alldates$EMA_Qday != df.alldates$Quit_Date1, na.rm=TRUE)
sum(df.alldates$quitday != df.alldates$Quit_Date1, na.rm=TRUE)

# The above diagnostic shows that:
#   - There are individuals whose Quit_Date1 differs from quitday
#     but for most individuals Quit_Date1 is identical to quitday
#   - For ALL individuals, EMA_Qday is identical to Quit_Date1


