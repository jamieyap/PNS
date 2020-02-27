###############################################################################
# ABOUT:
# * Creates a dataset with EMA completion as outcome
# * Only Random EMAs are used; other types of EMAs are not used
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.pns.code, "pns-data-manip-utils.R"))
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

#------------------------------------------------------------------------------
# Read in raw data
#------------------------------------------------------------------------------
pns.quit.dates <- read.csv(file.path(path.pns.input_data, "pns_quit_dates.csv"))
df.raw.post.quit.random <- read.csv(file.path(path.pns.input_data, "Post_Quit_Random.csv"))

#------------------------------------------------------------------------------
# Tasks common to all analyses using PNS study data
#------------------------------------------------------------------------------
# Implement decision rules on constructing time frame
df.time.frame <- SetTimeFrame(df.quit.dates = pns.quit.dates, 
                              study.duration = 21, 
                              addtime = 0)
# Implement decision rules to include only "valid EMAs"
df.post.quit.random <- SetUpPostQuit(df.raw = df.raw.post.quit.random, 
                                     df.time.frame = df.time.frame)

#------------------------------------------------------------------------------
# Tasks specific to this dataset for analysis
#------------------------------------------------------------------------------
# Create a new variable so that for each row in the row data is checked: 
# equals 1 if response to any question was recorded
# equals 0 if no response to any question was recorded
these.cols <- c("record.id",             
                "record.status",         
                "id",                    
                "Day",                   
                "delivered.hrts",        
                "InitiatedDate",         
                "InitiatedTime",         
                "SnzCount",              
                "SnzTime1",              
                "SnzTime2",              
                "SnzTime3",              
                "SnzTime4",              
                "assessment.hrts",       
                "AssessmentCancelled",   
                "ParticipantComplied",   
                "AssessmentID",          
                "assessment.type",       
                "Asse_Type",             
                "Asse_Mode",             
                "AssessmentEnd",         
                "responded",             
                "completed",             
                "AssessmentCompleted",   
                "AssessmentNOTCompleted",
                "AssessmentTimedOut",    
                "StartDate",             
                "Quit_Date1",            
                "Quit_Date2",            
                "Quit_Date3",            
                "Quit_Date4",            
                "Quit_Date5",   
                "X",                     
                "delivered.unixts",      
                "assessment.unixts",     
                "delay",                 
                "quit.date",             
                "start.clock",           
                "end.clock") 

df.post.quit.random <- CheckAnyResponse(df = df.post.quit.random, drop.cols = these.cols)

# Implement decision rules to create the variable engaged.yes
df.post.quit.random <- df.post.quit.random %>% 
  mutate(engaged.yes = with.any.response)

# Implement decision rules for:
# (1) Timestamp when engaged.yes=1 and missing assessment.unixts
# (2) Timestamp when engaged.yes=1 and engaged.yes=0
df.post.quit.random <- df.post.quit.random %>%
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

#------------------------------------------------------------------------------
# Obtain subset of columns from the original set of EMA items
#------------------------------------------------------------------------------
# Column names with reference information
cols.ref <- c("id","record.id","start.clock","time.unixts.scaled","engaged.yes")

# Column names of questions framed in terms of the present time
cols.items.now <- c("Urge1","Urge2","Urge3",
                    "Affect1","Affect2","Affect3","Affect4","Affect5",
                    "Affect6","Affect7","Affect8","Affect9","Affect10",
                    "Expect1","Expect2","AbsSelfEff",
                    "Motive1","Motive2")

df.post.quit.random <- df.post.quit.random %>% select(c(cols.ref, cols.items.now))

#------------------------------------------------------------------------------
# Create new variables
#------------------------------------------------------------------------------
df.post.quit.random <- df.post.quit.random %>% 
  mutate(ones=1) %>%
  group_by(id) %>% 
  # Total number of prompts from start.clock up to the present
  mutate(total.prompts.since.start=cumsum(ones)) %>%
  select(-ones)

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------
write.csv(df.post.quit.random, 
          file.path(path.pns.output_data, "pns.analysis.engagement.csv"), 
          row.names=FALSE)

