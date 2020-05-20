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
df <- left_join(x = staff.recorded.dates, y = df.baseline.dates, by = "callnumr")
df <- left_join(x = df, y = collect.prequit, by = "id")
df <- left_join(x = df, y = collect.postquit, by = "id")
df$prequit.latest.longformatdate <- df$prequit.latest.date
df$postquit.earliest.longformatdate <- df$postquit.earliest.date
df$prequit.latest.date <- as.POSIXct(strftime(df$prequit.latest.date, "%Y-%m-%d"))
df$postquit.earliest.date <- as.POSIXct(strftime(df$postquit.earliest.date, "%Y-%m-%d"))

#------------------------------------------------------------------------------
# Do checks
#------------------------------------------------------------------------------
df <- df %>% 
  mutate(is.equal = case_when(
    EMA_Qday==quitday & EMA_Qday==postquit.earliest.date ~ 1,
    is.na(EMA_Qday) | is.na(quitday) | is.na(postquit.earliest.date) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  arrange(is.equal)

df <- df %>%
  mutate(use.quit.date = case_when(
    is.equal==1 ~ "EMA_Qday",
    is.equal==0 & postquit.earliest.date>=EMA_Qday & postquit.earliest.date>=quitday & postquit.earliest.date-EMA_Qday<=postquit.earliest.date-quitday & EMA_Qday>=prequit.latest.date ~ "EMA_Qday",
    is.equal==0 & postquit.earliest.date>=EMA_Qday & postquit.earliest.date>=quitday & postquit.earliest.date-EMA_Qday>postquit.earliest.date-quitday & quitday>=prequit.latest.date ~ "quitday",
    is.equal==0 & postquit.earliest.date<EMA_Qday & postquit.earliest.date<quitday ~ "postquit.earliest.date",
    TRUE ~ NA_character_
  )) %>%
  arrange(is.equal, use.quit.date)

pns.key.id01 <- as.numeric(Sys.getenv("pns.key.id01"))
pns.key.id02 <- as.numeric(Sys.getenv("pns.key.id02"))
pns.key.id03 <- as.numeric(Sys.getenv("pns.key.id03"))
pns.key.id04 <- as.numeric(Sys.getenv("pns.key.id04"))

df <- df %>%
  mutate(use.quit.date = replace(use.quit.date, id==pns.key.id01, "EMA_Qday")) %>%
  mutate(use.quit.date = replace(use.quit.date, id==pns.key.id02, "postquit.earliest.date")) %>%
  mutate(use.quit.date = replace(use.quit.date, id==pns.key.id03, "postquit.earliest.date")) %>%
  mutate(use.quit.date = replace(use.quit.date, id==pns.key.id04, "EMA_Qday")) %>%
  arrange(is.equal, use.quit.date)

df <- df %>%
  mutate(use.quit.date.value = case_when(
    use.quit.date=="EMA_Qday" ~ EMA_Qday,
    use.quit.date=="postquit.earliest.date" ~ postquit.earliest.date,
    use.quit.date=="quitday" ~ quitday,
    TRUE~as.POSIXct(NA)
  ))

df <- df %>%
  mutate(use.quit.time.value = case_when(
    is.equal==1 & is.na(prequit.latest.date) ~ "04:00:00",
    is.equal==1 ~ strftime(prequit.latest.longformatdate, "%H:%M:%S"),
    is.equal==0 & postquit.earliest.date==prequit.latest.date ~ strftime(prequit.latest.longformatdate, "%H:%M:%S"),
    is.equal==0 & postquit.earliest.date!=prequit.latest.date ~ "04:00:00",
    is.na(is.equal) & !is.na(postquit.earliest.date) ~ "04:00:00",
    TRUE~NA_character_
  ))

#------------------------------------------------------------------------------
# Infer first day of pre-quit period and last day of post-quit period
#------------------------------------------------------------------------------
df <- df %>%
  mutate(use.begin.date.value = use.quit.date.value-7*24*60*60,
         use.begin.time.value = if_else(!is.na(use.begin.date.value), "00:00:00", NA_character_),
         use.end.date.value = use.quit.date.value+21*24*60*60,
         use.end.time.value = if_else(!is.na(use.end.date.value), "00:00:00", NA_character_)) %>%
  mutate(use.begin.date.value = as.POSIXct(strftime(use.begin.date.value, "%Y-%m-%d")),
         use.end.date.value = as.POSIXct(strftime(use.end.date.value, "%Y-%m-%d")))

#------------------------------------------------------------------------------
# Save file
#------------------------------------------------------------------------------
df <- df %>% 
  mutate(prequit.latest.date = prequit.latest.longformatdate, 
         postquit.earliest.date = postquit.earliest.longformatdate) %>%
  select(-prequit.latest.longformatdate, -postquit.earliest.longformatdate)

write.csv(df, file.path(path.pns.output_data, "dates.csv"), row.names = FALSE, na="")

