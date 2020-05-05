###############################################################################
# ABOUT:
# * Determines an individual's Quit Date by taking the earliest recorded date
#   among dates obtained from several streams of data
# * Final Quit Dates for each individual are saved in a csv file
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")

#------------------------------------------------------------------------------
# Get dates from several streams of data
#------------------------------------------------------------------------------

###############################################################################
# 1. Dates in records from study staff
###############################################################################
df.candidate.quit.dates <- read.csv(file.path(path.pns.input_data, "candidate_quit_dates.csv"))
df.candidate.quit.dates <- df.candidate.quit.dates %>%
  mutate(id = as.character(id),
         callnumr = as.character(callnumr),
         candidate.quit.date = as.character(candidate.quit.date)) %>%
  mutate(candidate.quit.date = if_else(candidate.quit.date=="", NA_character_, candidate.quit.date)) %>%
  mutate(candidate.quit.date = as.POSIXct(strptime(candidate.quit.date, "%m/%d/%Y")))

###############################################################################
# 2. Timestamp of Quit Date as recorded in Baseline Variables raw data file
###############################################################################
df.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"))
df.baseline.quit.dates <- df.baseline %>% 
  select(callnumr, baseline.quit.date=quitday) %>%
  mutate(callnumr = as.character(callnumr),
         baseline.quit.date = as.character(baseline.quit.date)) %>%
  mutate(baseline.quit.date = as.POSIXct(strptime(baseline.quit.date, "%m/%d/%Y")))

# Remove from environment
remove(df.baseline)

###############################################################################
# 3. Timestamp of first random EMA delivered to an individual during the
#    post-quit period, as recorded in the Post Quit Random EMA raw data file
###############################################################################
this.file <- "Post_Quit_Random.csv"

df.raw <- read.csv(file.path(path.pns.input_data, this.file))
df.raw.quit.dates <- df.raw %>% 
  select(id = Part_ID, initiated=Initiated) %>%
  mutate(id = as.character(id),
         initiated = as.character(initiated)) %>%
  # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
  mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
  arrange(id, initiated) %>%
  group_by(id) %>%
  # earliest.date is of class POSIXct
  # latest.date is of class POSIXct
  summarise(earliest.date = min(initiated),
            latest.date = max(initiated))

# Save set of dates to new variable
df.random.quit.dates <- df.raw.quit.dates %>% 
  rename(random.earliest.date = earliest.date,
         random.latest.date = latest.date)

# Remove used variables from environment
remove(df.raw, df.raw.quit.dates, this.file)

###############################################################################
# 4. Timestamp of first Urge EMA initiated by an individual during the
#    post-quit period, as recorded in the Post Quit Urge EMA raw data file
###############################################################################
this.file <- "Post_Quit_Urge.csv"

df.raw <- read.csv(file.path(path.pns.input_data, this.file))
df.raw.quit.dates <- df.raw %>% 
  select(id = Part_ID, initiated=Initiated) %>%
  mutate(id = as.character(id),
         initiated = as.character(initiated)) %>%
  # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
  mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
  arrange(id, initiated) %>%
  group_by(id) %>%
  # earliest.date is of class POSIXct
  # latest.date is of class POSIXct
  summarise(earliest.date = min(initiated),
            latest.date = max(initiated))

# Save set of dates to new variable
df.urge.quit.dates <- df.raw.quit.dates %>% 
  rename(urge.earliest.date = earliest.date,
         urge.latest.date = latest.date)

# Remove used variables from environment
remove(df.raw, df.raw.quit.dates, this.file)

###############################################################################
# 5. Timestamp of first About to Slip Part 1 EMA initiated by an individual during
#    the post-quit period, as recorded in the About to Slip Part 1 EMA raw data file
###############################################################################
this.file <- "Post_Quit_About_to_Slip.csv"

df.raw <- read.csv(file.path(path.pns.input_data, this.file))
df.raw.quit.dates <- df.raw %>% 
  select(id = Part_ID, initiated=Initiated) %>%
  mutate(id = as.character(id),
         initiated = as.character(initiated)) %>%
  # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
  mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
  arrange(id, initiated) %>%
  group_by(id) %>%
  # earliest.date is of class POSIXct
  # latest.date is of class POSIXct
  summarise(earliest.date = min(initiated),
            latest.date = max(initiated))

# Save set of dates to new variable
df.about2slip1.quit.dates <- df.raw.quit.dates %>% 
  rename(about2slip1.earliest.date = earliest.date,
         about2slip1.latest.date = latest.date)

# Remove used variables from environment
remove(df.raw, df.raw.quit.dates, this.file)

###############################################################################
# 6. Timestamp of first About to Slip Part 2 EMA initiated by an individual during
#    the post-quit period, as recorded in the About to Slip Part 2 EMA raw data file
###############################################################################
this.file <- "Post_Quit_About_to_Slip_Part2.csv"

df.raw <- read.csv(file.path(path.pns.input_data, this.file))
df.raw.quit.dates <- df.raw %>% 
  select(id = Part_ID, initiated=Initiated) %>%
  mutate(id = as.character(id),
         initiated = as.character(initiated)) %>%
  # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
  mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
  arrange(id, initiated) %>%
  group_by(id) %>%
  # earliest.date is of class POSIXct
  # latest.date is of class POSIXct
  summarise(earliest.date = min(initiated),
            latest.date = max(initiated))

# Save set of dates to new variable
df.about2slip2.quit.dates <- df.raw.quit.dates %>% 
  rename(about2slip2.earliest.date = earliest.date,
         about2slip2.latest.date = latest.date)

# Remove used variables from environment
remove(df.raw, df.raw.quit.dates, this.file)

###############################################################################
# 7. Timestamp of first Already Slipped EMA initiated by an individual during
#    the post-quit period, as recorded in the Already Slipped EMA raw data file
###############################################################################
this.file <- "Post_Quit_Already_Slipped.csv"

df.raw <- read.csv(file.path(path.pns.input_data, this.file))
df.raw.quit.dates <- df.raw %>% 
  select(id = Part_ID, initiated=Initiated) %>%
  mutate(id = as.character(id),
         initiated = as.character(initiated)) %>%
  # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
  mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
  arrange(id, initiated) %>%
  group_by(id) %>%
  # earliest.date is of class POSIXct
  # latest.date is of class POSIXct
  summarise(earliest.date = min(initiated),
            latest.date = max(initiated))

# Save set of dates to new variable
df.alreadyslipped.quit.dates <- df.raw.quit.dates %>% 
  rename(alreadyslipped.earliest.date = earliest.date,
         alreadyslipped.latest.date = latest.date)

# Remove used variables from environment
remove(df.raw, df.raw.quit.dates, this.file)

#------------------------------------------------------------------------------
# Merge from several streams of data into one data frame
#------------------------------------------------------------------------------
df.candidate.quit.dates <- left_join(x = df.candidate.quit.dates, y = df.baseline.quit.dates, by = "callnumr")
df.candidate.quit.dates <- left_join(x = df.candidate.quit.dates, y = df.random.quit.dates, by = "id")
df.candidate.quit.dates <- left_join(x = df.candidate.quit.dates, y = df.urge.quit.dates, by = "id")
df.candidate.quit.dates <- left_join(x = df.candidate.quit.dates, y = df.about2slip1.quit.dates, by = "id")
df.candidate.quit.dates <- left_join(x = df.candidate.quit.dates, y = df.about2slip2.quit.dates, by = "id")
df.candidate.quit.dates <- left_join(x = df.candidate.quit.dates, y = df.alreadyslipped.quit.dates, by = "id")

#------------------------------------------------------------------------------
# Format dates
#------------------------------------------------------------------------------
df.candidate.quit.dates <- df.candidate.quit.dates %>%
  mutate(candidate.quit.date = as.POSIXct(strftime(candidate.quit.date, "%Y-%m-%d")),
         baseline.quit.date = as.POSIXct(strftime(baseline.quit.date, "%Y-%m-%d"))) %>%
  mutate(random.earliest.date = as.POSIXct(strftime(random.earliest.date, "%Y-%m-%d")),
         urge.earliest.date = as.POSIXct(strftime(urge.earliest.date, "%Y-%m-%d")),
         about2slip1.earliest.date = as.POSIXct(strftime(about2slip1.earliest.date, "%Y-%m-%d")),
         about2slip2.earliest.date = as.POSIXct(strftime(about2slip2.earliest.date, "%Y-%m-%d")),
         alreadyslipped.earliest.date = as.POSIXct(strftime(alreadyslipped.earliest.date, "%Y-%m-%d"))) %>%
  mutate(random.latest.date = as.POSIXct(strftime(random.latest.date, "%Y-%m-%d")),
         urge.latest.date = as.POSIXct(strftime(urge.latest.date, "%Y-%m-%d")),
         about2slip1.latest.date = as.POSIXct(strftime(about2slip1.latest.date, "%Y-%m-%d")),
         about2slip2.latest.date = as.POSIXct(strftime(about2slip2.latest.date, "%Y-%m-%d")),
         alreadyslipped.latest.date = as.POSIXct(strftime(alreadyslipped.latest.date, "%Y-%m-%d"))) 

#------------------------------------------------------------------------------
# Calculate earliest and latest EMA timestamps
#------------------------------------------------------------------------------
df.candidate.quit.dates <- df.candidate.quit.dates %>%
  mutate(ema.earliest.date = pmin(random.earliest.date, urge.earliest.date,
                                  about2slip1.earliest.date, about2slip2.earliest.date,
                                  alreadyslipped.earliest.date,
                                  na.rm=TRUE)) %>%
  mutate(ema.latest.date = pmax(random.latest.date, urge.latest.date,
                                about2slip1.latest.date, about2slip2.latest.date,
                                alreadyslipped.latest.date,
                                na.rm=TRUE))

#------------------------------------------------------------------------------
# Temporarily set value of use.quit.date variable
#------------------------------------------------------------------------------
df.quit.dates <- df.candidate.quit.dates %>%
  mutate(tmpvec = pmin(ema.earliest.date, candidate.quit.date)) %>%
  # Note: if ema.earliest.date is available then candidate.quit.date is available
  mutate(use.quit.date = if_else(is.na(ema.earliest.date), as.POSIXct(NA), tmpvec)) %>%
  select(-tmpvec)

#------------------------------------------------------------------------------
# Compare whether current value of use.quit.date equals baseline.quit.date
#------------------------------------------------------------------------------
df.quit.dates <- df.quit.dates %>% 
  mutate(is.equal = 1*(use.quit.date == baseline.quit.date),
         is.less.than = 1*(baseline.quit.date < use.quit.date),
         diffdays = baseline.quit.date - ema.latest.date) %>%
  mutate(diffdays = as.numeric(diffdays/(60*60*24)))

#------------------------------------------------------------------------------
# Implement more decision rules
#------------------------------------------------------------------------------
df.quit.dates <- df.quit.dates %>% 
  mutate(final.quit.date = if_else(is.equal==0 & is.less.than==1 & diffdays<0 & (diffdays>=-21), 
                                   baseline.quit.date, 
                                   use.quit.date))

#------------------------------------------------------------------------------
# Clean up data frame
#------------------------------------------------------------------------------
df.quit.dates <- df.quit.dates %>% 
  mutate(quit.date = as.character(final.quit.date)) %>%
  select(id, callnumr, quit.date)

#------------------------------------------------------------------------------
# Write out quit dates to csv file
#------------------------------------------------------------------------------
write.csv(df.quit.dates, file.path(path.pns.output_data, "quit_dates.csv"), row.names=FALSE)

