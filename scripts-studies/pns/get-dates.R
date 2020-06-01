###############################################################################
# ABOUT:
# * Several candidate dates in the raw data from which to determine Quit Date
# * Calculate the earliest recorded date among timestamps recorded in 
#   Post Quit raw data (all types of EMA)
# * Calculate the latest recorded date among timestamps recorded in 
#   Pre Quit raw data (all types of EMA)
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")

#------------------------------------------------------------------------------
# Get dates from Post Quit
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
    mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
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
# Get dates from Pre Quit
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
    mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
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
staff.recorded.dates <- staff.recorded.dates %>% mutate(EMA_Qday = as.POSIXct(strptime(EMA_Qday, "%m/%d/%Y")))

#------------------------------------------------------------------------------
# Dates in baseline raw data file
#------------------------------------------------------------------------------
df.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"), stringsAsFactors = FALSE)
df.baseline.dates <- df.baseline %>% 
  select(callnumr, quitday) %>%
  mutate(quitday = as.POSIXct(strptime(quitday, "%m/%d/%Y")))

# Remove from environment
remove(df.baseline)

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
  mutate(prequit.latest.shortformatdate = as.POSIXct(strftime(prequit.latest.shortformatdate, "%Y-%m-%d")),
         postquit.earliest.shortformatdate = as.POSIXct(strftime(postquit.earliest.shortformatdate, "%Y-%m-%d")))
  
#------------------------------------------------------------------------------
# Set Quit Date
#------------------------------------------------------------------------------
df.alldates <- df.alldates %>% 
  mutate(is.equal = case_when(
    EMA_Qday==quitday & EMA_Qday==postquit.earliest.shortformatdate ~ 1,
    is.na(EMA_Qday) | is.na(quitday) | is.na(postquit.earliest.shortformatdate) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  arrange(is.equal)

df.alldates <- df.alldates %>%
  mutate(use.quit.date = case_when(
    is.equal==1 ~ "EMA_Qday",
    is.equal==0 & prequit.latest.shortformatdate==postquit.earliest.shortformatdate ~ "postquit.earliest.shortformatdate",
    TRUE ~ NA_character_
  )) %>%
  arrange(is.equal, use.quit.date)

#------------------------------------------------------------------------------
# Set Quit Time
#------------------------------------------------------------------------------
df.alldates <- df.alldates %>%
  mutate(quit.hour = case_when(
    is.equal==1 & prequit.latest.shortformatdate==postquit.earliest.shortformatdate ~ 0.5*(postquit.earliest.longformatdate-prequit.latest.longformatdate)/60,
    is.equal==0 & prequit.latest.shortformatdate==postquit.earliest.shortformatdate ~ 0.5*(postquit.earliest.longformatdate-prequit.latest.longformatdate)/60,
    is.equal==1 & prequit.latest.shortformatdate<postquit.earliest.shortformatdate ~ 4,
    is.equal==1 & is.na(prequit.latest.shortformatdate) ~ 4,
    TRUE ~ NA_real_
  )) %>% 
  mutate(quit.hour = round(as.numeric(quit.hour), digits=1))


write.csv(df.alldates, file.path(path.pns.output_data, "dates.csv"), row.names = FALSE, na="")
