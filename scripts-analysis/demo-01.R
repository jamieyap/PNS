###############################################################################
# ABOUT:
# Sample code to
# - read curated data csv files
# - correctly identify subsets of the data to utilize in analysis
# - constuct smoking variable relative to each EMA
# - fit a random effects model
###############################################################################

# Set current working directory to location of curated data
# Change path.pns.output_data to relevant working directory
# For example,
# path.pns.output_data <- "C:/Users/user/Desktop"
# note that above, a backslash instead of a forward slash is used
path.pns.output_data <- Sys.getenv("path.pns.output_data")
setwd(path.pns.output_data)

# Load R packages we'll use
library(dplyr)

# -----------------------------------------------------------------------------
# Read in curated data
# -----------------------------------------------------------------------------
# In R version 3.6.3, by default, read.csv automatically converts strings to
# factors. Both data types are handled differently in R. 
# Hence, we set the argument stringsAsFactors=FALSE to prevent this behavior;
# strings will remain strings
merged <- read.csv("merged.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Identify rows to be utilized in Pre-Quit & Post-Quit manuscripts
# Identify rows which will be utilzied in sensitivity analysis
# -----------------------------------------------------------------------------
# merged contains participants who did and did NOT have ambiguity in Quit Date
# A manuscript looking at both Pre-Quit and Post-Quit time periods will utilize
# all rows in merged
data.main.analysis.all <- merged

# merged contains EMAs which will be utilized in Pre-Quit manuscripts and
# Post-Quit manuscripts. Let's identify which rows will be utilized in a
# Post-Quit manuscript
data.main.analysis.postquit <- data.main.analysis.all %>% filter(use_as_postquit==1)

# Now, let's identify those EMAs which will be utilized in a Pre-Quit manuscript
data.main.analysis.prequit <- data.main.analysis.all %>% filter(use_as_postquit==0)

# Obtain subset of rows to be utilised in sensitivity analysis
# Individuals with sensitivity=1 are those with ambiguity in Quit Dates
# In sensitivity analysis, we only wish to include those individuals
# who did NOT have ambiguity in Quit Dates
data.sensitivity.analysis.all <- merged %>% filter(sensitivity==0)

# Now, let's identify those EMAs which will be utilized in a Post-Quit manuscript
data.sensitivity.analysis.postquit <- data.sensitivity.analysis.all %>% filter(use_as_postquit==1)

# Now, let's identify those EMAs which will be utilized in a Pre-Quit manuscript
data.sensitivity.analysis.prequit <- data.sensitivity.analysis.all %>% filter(use_as_postquit==0)

###############################################################################
# Sanity check:
# - During the post-quit period, what EMA types are present in the data?
# - During the pre-quit period, what EMA types are present in the data?
###############################################################################

data.main.analysis.postquit %>% group_by(assessment_type) %>% summarise(count.ema.type = n())
data.main.analysis.prequit %>% group_by(assessment_type) %>% summarise(count.ema.type = n())

data.sensitivity.analysis.postquit %>% group_by(assessment_type) %>% summarise(count.ema.type = n())
data.sensitivity.analysis.prequit %>% group_by(assessment_type) %>% summarise(count.ema.type = n())

###############################################################################
# From here onward, let's focus on main analysis for a manuscript utilizing
# ONLY the Post-Quit period
###############################################################################

use.this.data <- data.main.analysis.postquit

# -----------------------------------------------------------------------------
# Identify rows and columns that will be associated with the smoking outcome
# in data analysis. Then, identify rows and columns that will NOT be
# associated with the smoking outcome in data analysis
# -----------------------------------------------------------------------------

# Now, let's identify columns associated with smoking outcome
# First, let's perform some preliminary steps
# common.column.names refers to column names common to both rows that will
# and will NOT be used as smoking outcome in data analysis
common.column.names <- c("id",
                         "callnumr",
                         "start_study_hrts",
                         "end_study_hrts",
                         "quit_hrts",
                         "start_study_unixts",
                         "end_study_unixts",
                         "quit_unixts",
                         "record_id",
                         "assessment_type",
                         "use_as_postquit",
                         "sensitivity",
                         "with_any_response",
                         "delivered_hrts",
                         "begin_hrts",
                         "end_hrts",
                         "time_hrts",
                         "delivered_unixts",
                         "begin_unixts",
                         "end_unixts",
                         "time_unixts",
                         "this_row_smoking_outcome")

# smoking.column.names refers to column names that will ONLY be used
# in relation to the smoking outcome in data analysis
smoking.column.names <- c("ema_order",
                          "time_between_hours",
                          "rawdata_indicator",
                          "rawdata_qty",
                          "rawdata_timing",
                          "smoking_indicator",
                          "smoking_qty",
                          "smoking_delta_minutes",
                          "assessment_type_past_1", 
                          "time_hrts_past_1", 
                          "time_unixts_past_1")

# Identify column names that are in use.this.data but NOT
# in common.column.names or smoking.column.names
all.column.names <- colnames(use.this.data)
remove.these.column.names <- c(common.column.names, smoking.column.names)
ema.column.names <- setdiff(all.column.names, remove.these.column.names)

# this_row_smoking_outcome is a binary variable
# this_row_smoking_outcome=1 if row will be used as smoking outcome in data analysis
# this_row_smoking_outcome=0 if row will NOT be used as smoking outcome in data analysis
data.outcome <- use.this.data %>% 
  filter(this_row_smoking_outcome == 1) %>%
  select(common.column.names, smoking.column.names)

# Identify rows associated with EMAs
data.ema <- use.this.data %>% 
  filter(this_row_smoking_outcome == 0) %>%
  select(common.column.names, ema.column.names)

table(data.ema$assessment_type)

# -----------------------------------------------------------------------------
# Let's calculate summary statistics using data.outcome
# -----------------------------------------------------------------------------

table(data.outcome$smoking_indicator)
summary(data.outcome$smoking_indicator)

data.outcome %>% 
  summarise(notmiss = sum(!is.na(smoking_indicator)),
  rate = mean(smoking_indicator, na.rm=TRUE), 
  stddev = sd(smoking_indicator, na.rm=TRUE),
  minimum = min(smoking_indicator, na.rm=TRUE),
  maximum = max(smoking_indicator, na.rm=TRUE))

data.outcome %>% 
  filter(smoking_qty>0) %>% 
  summarise(ave = mean(smoking_qty, na.rm=TRUE))

# -----------------------------------------------------------------------------
# Let's calculate summary statistics using data.ema
# -----------------------------------------------------------------------------

# Let's check what assessment types are in data.ema
# Recall that due to ambiguity in Quit Dates, some Pre-Quit EMAs may be
# utilized in Post-Quit manuscripts
table(data.ema$assessment_type)

# TASK: 
# 1. Identify columns corresponding to the question
# "Since the last computer recording, I experienced or thought about a 
# NEW OR ONGOING stressful issue or problem."
# for all EMA types in a Post-Quit manuscript, except Already Slipped EMAs
# 2. Calculate the following:
#    - total number of rows
#    - total missing
#    - among EMAs with a response to this item, calculate
#      the percentage of responses in each of the four possible
#      categories

# Read in correspondence between codebook variable names and column names 
# in the merged curated dataset
dictionary.colnames <- read.csv("ema_item_names_reformatted_column_names.csv", stringsAsFactors = FALSE)

# Print out column names corresponding to the question
# "Since the last computer recording, I experienced or thought about a 
# NEW OR ONGOING stressful issue or problem."
dictionary.colnames %>%
  filter(name_codebook=="Stressor1_PostQ_Random" | name_codebook=="Stressor1_PostQ_Urge"|
           name_codebook=="Stressor1_PostQ_Slip" | name_codebook=="Stressor1_PostQ_Slip2"|
           name_codebook=="Stressor1_PreQ_Random" | name_codebook=="Stressor1_PreqUrge"|
           name_codebook=="Stressor1_PreQ_Sm" | name_codebook=="Stressor1_PreQ_Sm2")

data.ema  <- data.ema %>%
  filter(assessment_type!="Post-Quit Already Slipped") %>%
  # name the new variable new_var and set initial value to missing
  mutate(new_var = NA_integer_) %>% 
  # Refer to dictionary.colnames and codebook
  # In Post-Quit Random EMAs, this variable is named Stressor1_PostQ_Random in the codebook
  # and its name in the merged curated dataset is postquit_random_item_35
  # Begin filling in new_var: if row correspnds to Post-Quit Random EMA, then set new_var to be
  # equal to postquit_random_item_35, else, do not change new_var
  mutate(new_var = if_else(assessment_type=="Post-Quit Random", postquit_random_item_35, new_var)) %>%
  mutate(new_var = if_else(assessment_type=="Post-Quit Urge", postquit_urge_item_35, new_var)) %>%
  mutate(new_var = if_else(assessment_type=="Post-Quit About to Slip Part One", postquit_partone_item_32, new_var)) %>%
  mutate(new_var = if_else(assessment_type=="Post-Quit About to Slip Part Two", postquit_parttwo_item_42, new_var)) %>%
  mutate(new_var = if_else(assessment_type=="Pre-Quit Random", prequit_random_item_35, new_var)) %>%
  mutate(new_var = if_else(assessment_type=="Pre-Quit Urge", prequit_urge_item_35, new_var)) %>%
  mutate(new_var = if_else(assessment_type=="Pre-Quit Smoking Part One", prequit_partone_item_32, new_var)) %>%
  mutate(new_var = if_else(assessment_type=="Pre-Quit Smoking Part Two", prequit_parttwo_item_37, new_var))

data.ema %>% 
  select(new_var) %>%
  summarise(total.rows = n(),
            total.missing = sum(is.na(new_var)))

# Number of EMAs which have 1, 2, 3, 4, 5
# Observe that there no ratings of 5 in any EMA
# although the codebook says that it should be possible for
# participants to give a rating of 5
data.ema %>% 
  filter(!is.na(new_var)) %>%
  select(new_var) %>% 
  table(.)

data.ema %>% filter(assessment_type=="Pre-Quit Random") %>% select(new_var) %>% table(.)
data.ema %>% filter(assessment_type=="Pre-Quit Urge") %>% select(new_var) %>% table(.)
data.ema %>% filter(assessment_type=="Pre-Quit Smoking Part One") %>% select(new_var) %>% table(.)
data.ema %>% filter(assessment_type=="Pre-Quit Smoking Part Two") %>% select(new_var) %>% table(.)

data.ema %>% filter(assessment_type=="Post-Quit Random") %>% select(new_var) %>% table(.)
data.ema %>% filter(assessment_type=="Post-Quit Urge") %>% select(new_var) %>% table(.)
data.ema %>% filter(assessment_type=="Post-Quit About to Slip Part One") %>% select(new_var) %>% table(.)
data.ema %>% filter(assessment_type=="Post-Quit About to Slip Part Two") %>% select(new_var) %>% table(.)
data.ema %>% filter(assessment_type=="Post-Quit Already Slipped") %>% select(new_var) %>% table(.)

# -----------------------------------------------------------------------------
# Now, using data.ema, let's examine the relationship between an individual's
# rating of new_var in the current EMA and whether the individual reported any
# smoking within the next H hours after the current EMA
# -----------------------------------------------------------------------------

# Now, let's just take Random EMAs
# Remember, we are limiting the scope of our analysis to the Post-Quit period
# but some Pre-Quit Random EMAs may be utilized in a Post-Quit analysis
# (see documentation ofr more details on why this is the case)
data.ema  <- data.ema %>% filter(assessment_type == "Post-Quit Random" | assessment_type == "Pre-Quit Random")

# Let's say we are interested to know how new_var may be related to smoking_indicator
data.ema <- data.ema %>% select(id, callnumr, assessment_type, quit_hrts, quit_unixts, time_hrts, time_unixts, new_var)

# Arrange rows of data.ema according to increased id and time
data.ema <- data.ema %>% arrange(id, time_unixts)

###############################################################################
# Now, for each row in data.ema let's construct a new variable:
# smoke_next_10=1 if individual smoked within next H hours of current EMA
###############################################################################

# Note: set H to be 10/60 if interested in the next 10 minutes
H <- 4
data.ema$smoke_next_H <- 0  # initialize smoke_next_H to zero
all.ids <- unique(data.ema$id)

list.all <- list()

for(i in 1:length(all.ids)){
  current.participant.data <- data.ema %>% filter(id == all.ids[i])
  current.participant.outcome <- data.outcome %>% filter(id == all.ids[i])
  
  # How many Random EMAs does this participant have?
  tot.random.ema <- nrow(current.participant.data)
  for(j in 1:tot.random.ema){
    # Current EMA is j
    # Pick out timestamp corresponding to current EMA
    timenow <- current.participant.data[["time_unixts"]][j]
    
    # Are there any rows within data.outcome that fall within H hours of timenow?
    subset.outcome <- current.participant.outcome %>% 
      # Note: since time_unixts is in seconds, need to multiply H by 3600
      filter(time_unixts >= timenow & time_unixts <= timenow + H*60*60)
    
    # How many rows are there?
    tot.outcome.rows <- nrow(subset.outcome)
    
    if(tot.outcome.rows > 0){
      responses_within_H <- subset.outcome[["smoking_indicator"]]
      # First, check if all are missing
      if(sum(is.na(responses_within_H))==tot.outcome.rows){
        # Don't do anything
        next
      }else{
        count_yes <- sum(subset.outcome[["smoking_indicator"]], na.rm=TRUE)
        any_yes <- if_else(count_yes>0, 1, 0)
        current.participant.data[j, "smoke_next_H"] <- any_yes
      }
    }else{
      # Don't do anything
      next
    }
  } # END of loop for one participant
  # Update data for this participant
  list.all <- append(list.all, list(current.participant.data))
}  # END of loop for all participants with any EMA in data.ema


# Transform list back into a data frame
data.ema <- bind_rows(list.all)

###############################################################################
# Now, let's calculate summary statistics on smoke_next_H
###############################################################################

summary(data.ema$smoke_next_H)
# Note: consider alternate models if this number is tiny

###############################################################################
# Now, let's fit a random effects model
###############################################################################

# Load R packages we'll use
library(lme4)

# Let's rescale new_var
grand.average <- mean(data.ema$new_var, na.rm=TRUE)
grand.variance <- var(data.ema$new_var, na.rm=TRUE)
grand.sd <- sqrt(grand.variance)

data.ema <- data.ema %>% mutate(new_var_scaled = (new_var-grand.average)/grand.sd)

# Plain vanilla models
model1 <- glmer(smoke_next_H ~ new_var_scaled + (1|id), data=data.ema, family=binomial)
summary(model1)

# Control for days since quit
data.ema <- data.ema %>%
  # time_unixts and quit_unixts is in seconds
  # convert to days by dividing by 60*60*24 
  mutate(days_since_quit = (time_unixts - quit_unixts)/(60*60*24))

# Let's add another covariate
model2 <- glmer(smoke_next_H ~ new_var_scaled + days_since_quit + new_var_scaled:days_since_quit + (1|id), data=data.ema, family=binomial)
summary(model2)




