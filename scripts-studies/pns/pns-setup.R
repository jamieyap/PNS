# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the PNS study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"data-manip-utils.R"))

#------------------------------------------------------------------------------
# Read in post-quit raw data from PNS study
#------------------------------------------------------------------------------

post.quit.about.to.slip <- read.csv(file.path(path.input_data, "PNS/Post_Quit_About_to_Slip.csv"))
post.quit.about.to.slip.part2 <- read.csv(file.path(path.input_data, "PNS/Post_Quit_About_to_Slip_Part2.csv"))
post.quit.already.slipped <- read.csv(file.path(path.input_data, "PNS/Post_Quit_Already_Slipped.csv"))
post.quit.random <- read.csv(file.path(path.input_data, "PNS/Post_Quit_Random.csv"))
post.quit.urge <- read.csv(file.path(path.input_data, "PNS/Post_Quit_Urge.csv"))

#------------------------------------------------------------------------------
# Create a masterlist for the start and end of clock for each participant
# using their quit dates
#------------------------------------------------------------------------------

pns.quit.dates <- read.csv(file.path(path.input_data, "PNS/pns_quit_dates.csv"))  
pns.quit.dates <- SetClock(df.quit.dates = pns.quit.dates, study.duration = 21, addtime = 0) 
#out_pns_quit_dates.csv is an input to test-file-01.R
write.csv(pns.quit.dates, file.path(path.output_data, "PNS/out_quit_dates.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Create covariates: 
# *  with.any.response=1 if a response to at least one 
#    EMA question was recorded and with.any.response=0 otherwise
# *  engaged.yes=1 if with.any.response=1 or FRAGMENT RECORD
#    and 0 otherwise
#------------------------------------------------------------------------------

# Assessment type: Post quit about to slip
post.quit.about.to.slip <- CheckAnyResponse(df = post.quit.about.to.slip, drop.cols.index = c(1:31, 96)) %>%
  mutate(engaged.yes = (with.any.response==1) | (with.any.response==0 & Record_Status=="FRAGMENT RECORD")) %>%
  mutate(engaged.yes = as.numeric(engaged.yes))

# Assessment type: Post quit about to slip part 2
post.quit.about.to.slip.part2 <- CheckAnyResponse(df = post.quit.about.to.slip.part2, drop.cols.index = c(1:31, 106)) %>%
  mutate(engaged.yes = (with.any.response==1) | (with.any.response==0 & Record_Status=="FRAGMENT RECORD")) %>%
  mutate(engaged.yes = as.numeric(engaged.yes)) 

# Assessment type: Post quit already slipped
post.quit.already.slipped <- CheckAnyResponse(df = post.quit.already.slipped, drop.cols.index = c(1:31, 102)) %>%
  mutate(engaged.yes = (with.any.response==1) | (with.any.response==0 & Record_Status=="FRAGMENT RECORD")) %>%
  mutate(engaged.yes = as.numeric(engaged.yes))

# Assessment type: Post quit random
post.quit.random <- CheckAnyResponse(df = post.quit.random, drop.cols.index = c(1:31, 99)) %>%
  mutate(engaged.yes = (with.any.response==1) | (with.any.response==0 & Record_Status=="FRAGMENT RECORD")) %>%
  mutate(engaged.yes = as.numeric(engaged.yes)) 

# Assessment type: Post quit urge
post.quit.urge <- CheckAnyResponse(df = post.quit.urge, drop.cols.index = c(1:31, 99)) %>%
  mutate(engaged.yes = (with.any.response==1) | (with.any.response==0 & Record_Status=="FRAGMENT RECORD")) %>%
  mutate(engaged.yes = as.numeric(engaged.yes)) 

#------------------------------------------------------------------------------
# Only retain rows whose data is recorded between start.clock and end.clock
# and rename columns that will serve as primary keys
#------------------------------------------------------------------------------

post.quit.about.to.slip <- post.quit.about.to.slip %>%
  rename(id = Part_ID, 
         record.status = Record_Status,
         assessment.type = Asse_Name, 
         delivered.hrts = Initiated,
         assessment.hrts = AssessmentBegin) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  mutate(delivered.unixts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(delivered.unixts = as.numeric(delivered.unixts)) %>%
  mutate(assessment.unixts = as.POSIXct(strptime(assessment.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(assessment.unixts = as.numeric(assessment.unixts)) %>%
  mutate(delay = assessment.unixts - delivered.unixts) %>%
  left_join(x = ., y = pns.quit.dates, by = "id") %>%
  filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock) %>%
  arrange(id, delivered.unixts)

post.quit.about.to.slip.part2 <- post.quit.about.to.slip.part2 %>%
  rename(id = Part_ID, 
         record.status = Record_Status,
         assessment.type = Asse_Name, 
         delivered.hrts = Initiated,
         assessment.hrts = AssessmentBegin) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  mutate(delivered.unixts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(delivered.unixts = as.numeric(delivered.unixts)) %>%
  mutate(assessment.unixts = as.POSIXct(strptime(assessment.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(assessment.unixts = as.numeric(assessment.unixts)) %>%
  mutate(delay = assessment.unixts - delivered.unixts) %>%
  left_join(x = ., y = pns.quit.dates, by = "id") %>%
  filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock) %>%
  arrange(id, delivered.unixts)

post.quit.already.slipped <- post.quit.already.slipped %>%
  rename(id = Part_ID, 
         record.status = Record_Status, 
         assessment.type = Asse_Name, 
         delivered.hrts = Initiated,
         assessment.hrts = AssessmentBegin) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  mutate(delivered.unixts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(delivered.unixts = as.numeric(delivered.unixts)) %>%
  mutate(assessment.unixts = as.POSIXct(strptime(assessment.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(assessment.unixts = as.numeric(assessment.unixts)) %>%
  mutate(delay = assessment.unixts - delivered.unixts) %>%
  left_join(x = ., y = pns.quit.dates, by = "id") %>%
  filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock) %>%
  arrange(id, delivered.unixts)

post.quit.random <- post.quit.random %>%
  rename(id = Part_ID, 
         record.status = Record_Status, 
         assessment.type = Asse_Name,
         delivered.hrts = Initiated,
         assessment.hrts = AssessmentBegin) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  mutate(delivered.unixts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(delivered.unixts = as.numeric(delivered.unixts)) %>%
  mutate(assessment.unixts = as.POSIXct(strptime(assessment.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(assessment.unixts = as.numeric(assessment.unixts)) %>%
  mutate(delay = assessment.unixts - delivered.unixts) %>%
  left_join(x = ., y = pns.quit.dates, by = "id") %>%
  filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock) %>%
  arrange(id, delivered.unixts)

post.quit.urge <- post.quit.urge %>%
  rename(id = Part_ID, 
         record.status = Record_Status, 
         assessment.type = Asse_Name, 
         delivered.hrts = Initiated,
         assessment.hrts = AssessmentBegin) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  mutate(delivered.unixts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(delivered.unixts = as.numeric(delivered.unixts)) %>%
  mutate(assessment.unixts = as.POSIXct(strptime(assessment.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz="EST5EDT"))) %>% 
  mutate(assessment.unixts = as.numeric(assessment.unixts)) %>%
  mutate(delay = assessment.unixts - delivered.unixts) %>%
  left_join(x = ., y = pns.quit.dates, by = "id") %>%
  filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock) %>%
  arrange(id, delivered.unixts)

#------------------------------------------------------------------------------
# Decision rules on timestamps
#------------------------------------------------------------------------------

# Decision rules for:
# (1) Timestamp when engaged.yes=1 and missing assessment.unixts
# (2) Timestamp when engaged.yes=1 and engaged.yes=0

post.quit.about.to.slip <- post.quit.about.to.slip %>%
  mutate(assessment.unixts = if_else(is.na(assessment.unixts), delivered.unixts, assessment.unixts)) %>% 
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

post.quit.about.to.slip.part2 <- post.quit.about.to.slip.part2 %>%
  mutate(assessment.unixts = if_else(is.na(assessment.unixts), delivered.unixts, assessment.unixts)) %>% 
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

post.quit.already.slipped <- post.quit.already.slipped %>%
  mutate(assessment.unixts = if_else(is.na(assessment.unixts), delivered.unixts, assessment.unixts)) %>% 
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

post.quit.random <- post.quit.random %>%
  mutate(assessment.unixts = if_else(is.na(assessment.unixts), delivered.unixts, assessment.unixts)) %>% 
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

post.quit.urge <- post.quit.urge %>%
  mutate(assessment.unixts = if_else(is.na(assessment.unixts), delivered.unixts, assessment.unixts)) %>% 
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

# Decision rules for:
# (3) When delay is negative or when delay > 20 minutes
post.quit.about.to.slip <- post.quit.about.to.slip %>% filter(is.na(delay)|(delay >= 0 & delay <= 20*60))
post.quit.about.to.slip.part2 <- post.quit.about.to.slip.part2 %>% filter(is.na(delay)|(delay >= 0 & delay <= 20*60))
post.quit.already.slipped <- post.quit.already.slipped %>% filter(is.na(delay)|(delay >= 0 & delay <= 20*60))
post.quit.random <- post.quit.random %>% filter(is.na(delay)|(delay >= 0 & delay <= 20*60))
post.quit.urge <- post.quit.urge %>% filter(is.na(delay)|(delay >= 0 & delay <= 20*60))

