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
# Setup PNS dataset. These are common data manipulation tasks for all
# PNS datasets for analyses
#------------------------------------------------------------------------------
source(file.path(path.code,"pns-run-curation/pns-setup.R"))

#------------------------------------------------------------------------------
# Specify relevant questions for each analysis dataset below
#------------------------------------------------------------------------------

# Specify columns to select from different datasets
these.cols.post.quit.random <- c("id", 
                                 "with.any.response", 
                                 "record.status", 
                                 "assessment.type",  
                                 "delivered.hrts",
                                 "delivered.unixts",
                                 "assessment.hrts",
                                 "assessment.unixts",
                                 "start.clock", 
                                 "end.clock")

###############################################################################
# Construct Basic Dataset
###############################################################################

# Select subset of columns from raw data
df.post.quit.random <- post.quit.random %>% select(these.cols.post.quit.random) 
df.post.quit.random <- df.post.quit.random[, order(colnames(df.post.quit.random))]

df.all <- df.post.quit.random %>%
  mutate(engaged.yes = (with.any.response==1) | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  mutate(engaged.yes = as.numeric(engaged.yes)) %>%
  arrange(id, assessment.unixts) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  select(id = id,
         start.clock = start.clock,
         end.clock = end.clock,
         engaged.yes = engaged.yes,
         with.any.response = with.any.response,
         record.status = record.status,
         delivered.hrts = delivered.hrts,
         delivered.unixts = delivered.unixts,
         assessment.hrts = assessment.hrts,
         assessment.unixts = assessment.unixts,
         assessment.type = assessment.type) # Rearrange columns

# Create IDs for each person-EMA delivered
df.all <- df.all %>% arrange(id, delivered.unixts)
df.all[,"delivered.assessment.id"] <- CreateID(dat = df.all, sequence.along = "delivered.unixts", by.var = "id", id.name = "delivered.assessment.id")

# Decision rule for observations with engaged.yes=1 and missing assessment.unixts timestamps
df.all <- df.all %>%
  mutate(assessment.unixts = if_else(is.na(assessment.unixts), delivered.unixts, assessment.unixts))

# Create a dataset for analysis
df.all <- df.all %>% 
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.secs = time.unixts - start.clock)

df.engaged.01 <- df.all %>% select(id, start.clock, end.clock, 
                                   time.unixts, time.secs, 
                                   engaged.yes, with.any.response)
         
write.csv(df.engaged.01, file.path(path.output_data, "PNS/df.analysis.engagement.01.csv"), row.names = FALSE)

###############################################################################
# Construct Basic Dataset + More Variables
###############################################################################

#------------------------------------------------------------------------------
# Specify more columns to add covariates
#------------------------------------------------------------------------------

more.cols.post.quit.random <- c("Affect4")

#------------------------------------------------------------------------------
# Merge column names
#------------------------------------------------------------------------------

if(!is.null(more.cols.post.quit.random)){
  these.cols.post.quit.random <- c(these.cols.post.quit.random, more.cols.post.quit.random)
}

#------------------------------------------------------------------------------
# Merge observations from different types of EMAs into one dataset
#------------------------------------------------------------------------------

df.post.quit.random <- post.quit.random %>% select(these.cols.post.quit.random) %>% rename(bored = Affect4) 
df.post.quit.random <- df.post.quit.random[, order(colnames(df.post.quit.random))]

df.all <- df.post.quit.random %>%
  mutate(engaged.yes = (with.any.response==1) | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  mutate(engaged.yes = as.numeric(engaged.yes)) %>%
  arrange(id, assessment.unixts) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  select(id = id,
         start.clock = start.clock,
         end.clock = end.clock,
         engaged.yes = engaged.yes,
         with.any.response = with.any.response,
         record.status = record.status,
         delivered.hrts = delivered.hrts,
         delivered.unixts = delivered.unixts,
         assessment.hrts = assessment.hrts,
         assessment.unixts = assessment.unixts,
         assessment.type = assessment.type,
         bored = bored) # Rearrange columns

# Create IDs for each person-EMA delivered
df.all <- df.all %>% arrange(id, delivered.unixts)
df.all[,"delivered.assessment.id"] <- CreateID(dat = df.all, sequence.along = "delivered.unixts", by.var = "id", id.name = "delivered.assessment.id")

# Decision rule for observations with engaged.yes=1 and missing assessment.unixts timestamps
df.all <- df.all %>%
  mutate(assessment.unixts = if_else(is.na(assessment.unixts), delivered.unixts, assessment.unixts))

# Create a dataset for analysis
df.all <- df.all %>% 
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.secs = time.unixts - start.clock)

df.engaged.02 <- df.all %>% select(id, start.clock, end.clock,
                                   time.unixts, time.secs, 
                                   engaged.yes, with.any.response,
                                   bored)

write.csv(df.engaged.02, file.path(path.output_data, "PNS/df.analysis.engagement.02.csv"), row.names = FALSE)

df.imputed.engaged.02 <- df.engaged.02 %>% 
  mutate(bored = if_else(engaged.yes==1 & is.na(bored), which(rmultinom(n=1, size=1, prob=c(1/5,1/5,1/5,1/5,1/5)) > 0), bored))

write.csv(df.imputed.engaged.02, file.path(path.output_data, "PNS/df.analysis.imputed.engagement.02.csv"), row.names = FALSE)


