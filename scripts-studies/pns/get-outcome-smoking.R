###############################################################################
# ABOUT:
# * Creates the smoking outcome from the output csv files of all the
#   following scripts:
#   postquit-ema-random.R
#   postquit-ema-urge.R
#   postquit-ema-about2slip1.R
#   postquit-ema-about2slip2.R
#   postquit-ema-alreadyslipped.R
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.pns.code, "data-manip-utils.R"))
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

#------------------------------------------------------------------------------
# Read in data
#------------------------------------------------------------------------------
df.postquit.random <- read.csv(file.path(path.pns.output_data, "post_quit_random.csv"))
df.postquit.urge <- read.csv(file.path(path.pns.output_data, "post_quit_urge.csv"))
df.postquit.about2slip1 <- read.csv(file.path(path.pns.output_data, "post_quit_about2slip1.csv"))
df.postquit.about2slip2 <- read.csv(file.path(path.pns.output_data, "post_quit_about2slip2.csv"))
df.postquit.alreadyslipped <- read.csv(file.path(path.pns.output_data, "post_quit_alreadyslipped.csv"))

#------------------------------------------------------------------------------
# Identify smoking items across different types of EMAs
# and create common variable names across different types of EMAs
#------------------------------------------------------------------------------
df.postquit.random <- df.postquit.random %>% 
  rename(smoking.indicator = PostQRSmoking1, 
         smoking.qty = Smoking2_PostQ_Random, 
         smoking.timing = Smoking3)

df.postquit.urge <- df.postquit.urge %>% 
  rename(smoking.indicator = SmPostQU1, 
         smoking.qty = Smoking2_PostQ_Urge, 
         smoking.timing = Smoking3)

df.postquit.about2slip1 <- df.postquit.about2slip1 %>% 
  mutate(smoking.indicator = NA, 
         smoking.qty = NA, 
         smoking.timing = NA)

df.postquit.about2slip2 <- df.postquit.about2slip2 %>% 
  rename(smoking.qty = CigJustNow_PostQ_Slip2) %>%
  mutate(smoking.indicator = NA, 
         smoking.timing = NA)

df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% 
  rename(smoking.qty = HowManyCig, 
         smoking.timing = LastCig) %>%
  mutate(smoking.indicator = NA)

#------------------------------------------------------------------------------
# Preparatory steps to create smoking outcome variable:
# Obtain subset of columns from the original set of EMA items
# and exclude all rows with with.any.response==0
#------------------------------------------------------------------------------
df.postquit.random <- df.postquit.random %>% filter(with.any.response==1)
df.postquit.urge <- df.postquit.urge %>% filter(with.any.response==1)
df.postquit.about2slip1 <- df.postquit.about2slip1 %>% filter(with.any.response==1)
df.postquit.about2slip2 <- df.postquit.about2slip2 %>% filter(with.any.response==1)
df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% filter(with.any.response==1)

#------------------------------------------------------------------------------
# Select columns to retain
#------------------------------------------------------------------------------
# Column names with reference information
keep.cols <- c("id","record.id","assessment.type",
               "start.clock.unixts","end.clock.unixts","begin.unixts",
               "smoking.indicator","smoking.qty","smoking.timing")

# Take a selection of columns for each type of EMA
df.postquit.random <- df.postquit.random %>% select(keep.cols)
df.postquit.urge <- df.postquit.urge %>% select(keep.cols)
df.postquit.about2slip1 <- df.postquit.about2slip1 %>% select(keep.cols)
df.postquit.about2slip2 <- df.postquit.about2slip2 %>% select(keep.cols)
df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% select(keep.cols)

#------------------------------------------------------------------------------
# Merge all data from different types of post quit EMA into one data frame
#------------------------------------------------------------------------------
outcomedf.all <- rbind(df.postquit.random,
                       df.postquit.urge,
                       df.postquit.about2slip1,
                       df.postquit.about2slip2,
                       df.postquit.alreadyslipped)

# Clean up data types
outcomedf.all <- outcomedf.all %>% 
  rename(time.unixts = begin.unixts) %>%
  mutate(id = as.character(id),
         record.id = as.character(record.id),
         assessment.type = as.character(assessment.type),
         start.clock.unixts = as.numeric(start.clock.unixts),
         end.clock.unixts = as.numeric(end.clock.unixts),
         time.unixts = as.numeric(time.unixts),
         smoking.indicator = as.numeric(smoking.indicator),
         smoking.qty = as.numeric(smoking.qty),
         smoking.timing = as.character(smoking.timing))

#------------------------------------------------------------------------------
# More preparatory steps to create smoking outcome variable:
# Create end points of time inetrval
#------------------------------------------------------------------------------
outcomedf.all <- outcomedf.all %>%
  # Change scale of time.unixts to seconds elapsed insce start of clock
  mutate(time.unixts = time.unixts - start.clock.unixts) %>%
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., 
                    cols.today = c("assessment.type", "time.unixts"), 
                    h = c(1,1),
                    this.numeric = c(FALSE, TRUE))
  )

# Count number of EMAs per participant in outcomedf.all
outcomedf.all  <- outcomedf.all %>%
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  do(.data = ., mutate(.data = ., ema.id = 1:nrow(.)))

# Identify which column names corresopnd to lower bound and upper bound of time intervals
outcomedf.all <- outcomedf.all %>% 
  mutate(LB.ts = time.unixts_shift.minus.1, UB.ts = time.unixts,
         LB.type = assessment.type_shift.minus.1, UB.type = assessment.type)

# Create new time-related variables
outcomedf.all <- outcomedf.all %>% 
  # Since LB.ts and UB.ts are in seconds elapsed ince start.clock.unixts
  mutate(LB.ts = if_else(ema.id == 1, start.clock.unixts, LB.ts)) %>%
  mutate(interval.duration.secs = UB.ts - LB.ts) %>%
  mutate(interval.duration.hours = interval.duration.secs/(60*60))

#------------------------------------------------------------------------------
# Clean up raw responses to EMA smoking items and create smoking outcome
#------------------------------------------------------------------------------
outcomedf.all <- outcomedf.all %>% 
  group_by(UB.type) %>% 
  do(PNSCleanSmokingCount(df = .))

outcomedf.all <- outcomedf.all %>% 
  arrange(id, time.unixts) %>% 
  group_by(id) %>% 
  do(PNSCleanSmokingTime(df = .))

outcomedf.all <- outcomedf.all %>% 
  arrange(id, time.unixts) %>% 
  group_by(id) %>% 
  do(PNSRefineSmokingTime(df = .))
