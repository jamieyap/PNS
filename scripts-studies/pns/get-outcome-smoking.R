###############################################################################
# ABOUT:
# * Creates the smoking outcome from the output csv files of all the
#   following scripts: get-postquit-ema.R and get-prequit-ema.R
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
source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.pns.code, "data-manip-utils.R"))
#source(file.path(path.pns.code, "smoking-utils.R"))

#------------------------------------------------------------------------------
# Read in EMA data
#------------------------------------------------------------------------------

# Post Quit EMAs
df.postquit.random <- read.csv(file.path(path.pns.output_data, "postquit_random.csv"), stringsAsFactors = FALSE)
df.postquit.urge <- read.csv(file.path(path.pns.output_data, "postquit_urge.csv"), stringsAsFactors = FALSE)
df.postquit.abouttoslippartone <- read.csv(file.path(path.pns.output_data, "postquit_abouttoslippartone.csv"), stringsAsFactors = FALSE)
df.postquit.abouttoslipparttwo <- read.csv(file.path(path.pns.output_data, "postquit_abouttoslipparttwo.csv"), stringsAsFactors = FALSE)
df.postquit.alreadyslipped <- read.csv(file.path(path.pns.output_data, "postquit_alreadyslipped.csv"), stringsAsFactors = FALSE)

# Pre Quit EMAs
df.prequit.random <- read.csv(file.path(path.pns.output_data, "prequit_random.csv"), stringsAsFactors = FALSE)
df.prequit.urge <- read.csv(file.path(path.pns.output_data, "prequit_urge.csv"), stringsAsFactors = FALSE)
df.prequit.smokingpartone <- read.csv(file.path(path.pns.output_data, "prequit_smokingpartone.csv"), stringsAsFactors = FALSE)
df.prequit.smokingparttwo <- read.csv(file.path(path.pns.output_data, "prequit_smokingparttwo.csv"), stringsAsFactors = FALSE)

#------------------------------------------------------------------------------
# Identify smoking items across different types of EMAs
# and create common variable names across different types of EMAs
#------------------------------------------------------------------------------

# Post Quit EMAs
df.postquit.random <- df.postquit.random %>% 
  rename(smoking.indicator = PostQRSmoking1, 
         smoking.qty = Smoking2_PostQ_Random, 
         smoking.timing = Smoking3)

df.postquit.urge <- df.postquit.urge %>% 
  rename(smoking.indicator = SmPostQU1, 
         smoking.qty = Smoking2_PostQ_Urge, 
         smoking.timing = Smoking3)

df.postquit.abouttoslippartone <- df.postquit.abouttoslippartone %>% 
  mutate(smoking.indicator = NA, 
         smoking.qty = NA, 
         smoking.timing = NA)

df.postquit.abouttoslipparttwo <- df.postquit.abouttoslipparttwo %>% 
  rename(smoking.qty = CigJustNow_PostQ_Slip2) %>%
  mutate(smoking.indicator = NA, 
         smoking.timing = NA)

df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% 
  rename(smoking.qty = HowManyCig, 
         smoking.timing = LastCig) %>%
  mutate(smoking.indicator = NA)

# Pre Quit EMAs
df.prequit.random <- df.prequit.random %>% 
  rename(smoking.indicator = PreQRSmoking1, 
         smoking.qty = Smoking2_PreQ_Random, 
         smoking.timing = Smoking3)

df.prequit.urge <- df.prequit.urge %>% 
  rename(smoking.indicator = SmPQU1, 
         smoking.qty = Smoking2_PreQ_Urge, 
         smoking.timing = Smoking3)

df.prequit.smokingpartone <- df.prequit.smokingpartone %>% 
  mutate(smoking.indicator = NA, 
         smoking.qty = NA, 
         smoking.timing = NA)

df.prequit.smokingparttwo <- df.prequit.smokingparttwo %>% 
  rename(smoking.qty = CigJustNow) %>%
  mutate(smoking.indicator = NA, 
         smoking.timing = NA)

#------------------------------------------------------------------------------
# Preparatory steps to create smoking outcome variable:
# Obtain subset of columns from the original set of EMA items
# and exclude all rows with with.any.response==0
#------------------------------------------------------------------------------

# Post Quit EMA
df.postquit.random <- df.postquit.random %>% filter(with.any.response==1)
df.postquit.urge <- df.postquit.urge %>% filter(with.any.response==1)
df.postquit.abouttoslippartone <- df.postquit.abouttoslippartone %>% filter(with.any.response==1)
df.postquit.abouttoslipparttwo <- df.postquit.abouttoslipparttwo %>% filter(with.any.response==1)
df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% filter(with.any.response==1)

# Pre Quit EMA
df.prequit.random <- df.prequit.random %>% filter(with.any.response==1)
df.prequit.urge <- df.prequit.urge %>% filter(with.any.response==1)
df.prequit.smokingpartone <- df.prequit.smokingpartone %>% filter(with.any.response==1)
df.prequit.smokingparttwo <- df.prequit.smokingparttwo %>% filter(with.any.response==1)

#------------------------------------------------------------------------------
# Select columns to retain and merge data from different types of EMA into
# one big data frame
#------------------------------------------------------------------------------
# Column names with reference information
keep.cols <- c("id","record.id","assessment.type", 
               "begin.hrts", "begin.unixts", 
               "smoking.indicator","smoking.qty","smoking.timing")

# Post Quit EMA
df.postquit.random <- df.postquit.random %>% select(keep.cols)
df.postquit.urge <- df.postquit.urge %>% select(keep.cols)
df.postquit.abouttoslippartone <- df.postquit.abouttoslippartone %>% select(keep.cols)
df.postquit.abouttoslipparttwo <- df.postquit.abouttoslipparttwo %>% select(keep.cols)
df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% select(keep.cols)

# Pre Quit EMA
df.prequit.random <- df.prequit.random %>% select(keep.cols)
df.prequit.urge <- df.prequit.urge %>% select(keep.cols)
df.prequit.smokingpartone <- df.prequit.smokingpartone %>% select(keep.cols)
df.prequit.smokingparttwo <- df.prequit.smokingparttwo %>% select(keep.cols)

# Merge all smoking outcome data
outcomedf.all <- rbind(df.postquit.random,
                       df.postquit.urge,
                       df.postquit.abouttoslippartone,
                       df.postquit.abouttoslipparttwo,
                       df.postquit.alreadyslipped,
                       df.prequit.random,
                       df.prequit.urge,
                       df.prequit.smokingpartone,
                       df.prequit.smokingparttwo)

outcomedf.all <- outcomedf.all %>%   
  arrange(id, begin.unixts) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., cols.today = c("begin.unixts","begin.hrts"), h = c(1,1), this.numeric = c(TRUE,FALSE)))

outcomedf.all <- outcomedf.all %>% 
  arrange(id, begin.unixts) %>%
  group_by(id) %>%
  mutate(ones=1) %>%
  mutate(current.order.in.sequence = cumsum(ones)) %>%
  select(-ones) %>%
  mutate(time.between.hours = (begin.unixts - begin.unixts_shift.minus.1)/(60*60)) %>%
  rename(current.record.id = record.id,
         current.assessment.type = assessment.type,
         current.begin.hrts = begin.hrts,
         current.begin.unixts = begin.unixts,
         previous.begin.hrts = begin.hrts_shift.minus.1,
         previous.begin.unixts = begin.unixts_shift.minus.1) %>%
  select(id, previous.begin.hrts, previous.begin.unixts,
         current.record.id, current.assessment.type, 
         current.order.in.sequence,
         current.begin.hrts, current.begin.unixts,
         time.between.hours, everything())

#------------------------------------------------------------------------------
# Save data
#------------------------------------------------------------------------------
write.csv(outcomedf.all, file.path(path.pns.output_data, "smoking.csv"), row.names=FALSE, na="")

