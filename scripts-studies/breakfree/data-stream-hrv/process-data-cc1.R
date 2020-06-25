library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.breakfree.cc1.input_data <- Sys.getenv("path.breakfree.cc1.input_data")
path.breakfree.cc2.input_data <- Sys.getenv("path.breakfree.cc2.input_data")
path.breakfree.staged_data <- Sys.getenv("path.breakfree.staged_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))
load(file.path(path.breakfree.staged_data, "raw.hrv.cc1.RData"))

# Identify HRV-related features from datadescriptor
hrv.frequency.domain.features <- c("ECG_RR_Interval_Low_High_Frequency_Energy_Ratio",
                                   "ECG_RR_Interval_High_Frequency_Energy",
                                   "ECG_RR_Interval_Medium_Frequency_Energy",
                                   "ECG_RR_Interval_Low_Frequency_Energy")

hrv.time.domain.features <- c("variance","quartile deviation")

# Create new variable: hrv is an indicator variable equal to 1 
# if feature is HRV-related  and equal to 0 otherwise
datadescriptor.cc1 <- datadescriptor.cc1 %>% 
  mutate(is.hrv.feature = case_when(
    NAME %in% hrv.frequency.domain.features ~ 1,
    (NAME=="ECG_RR_Interval") & (STATISTIC %in% hrv.time.domain.features) ~ 1,
    TRUE ~ 0
  ))

# Create new column to index item number so that column descriptions can be 
# cross-referenced
datadescriptor.cc1 <- datadescriptor.cc1 %>% 
  mutate(variable.id = paste("variable",1:nrow(.), sep=".")) %>%
  select(variable.id, everything())

# Reformat column names
datadescriptor.cc1 <- datadescriptor.cc1 %>% 
  rename(name=NAME,
         data.type=DATA_TYPE,
         frequency=FREQUENCY,
         statistic=STATISTIC,
         unit=UNIT,
         description=DESCRIPTION)

# Create data frame from list
df.raw.hrv.cc1 <- bind_rows(list.df.raw.hrv.cc1)
colnames(df.raw.hrv.cc1)[colnames(df.raw.hrv.cc1) %in% paste("V",3:39,sep="")] <- paste("variable",1:37,sep=".")
colnames(df.raw.hrv.cc1)[colnames(df.raw.hrv.cc1)=="V1"] <- "time.unixts"
df.raw.hrv.cc1 <- df.raw.hrv.cc1 %>% select(-V2)

# Get variable names of HRV-related features
hrv.cols <- datadescriptor.cc1[which(datadescriptor.cc1[["is.hrv.feature"]]==1),"variable.id"]
df.raw.hrv.cc1 <- df.raw.hrv.cc1 %>% select(user.id, time.unixts, hrv.cols)

# Create human-readable timestamp from unix timestamp
df.raw.hrv.cc1 <- df.raw.hrv.cc1 %>%
  mutate(time.unixts = time.unixts/1000) %>%
  mutate(time.hrts = as.POSIXct(time.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(time.hrts = strftime(time.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"))  %>% 
  mutate(time.hrts = as.character(time.hrts)) %>%
  mutate(timezone.hrts = "CST6CDT") %>%
  select(user.id, time.hrts, timezone.hrts, time.unixts, everything())

# Clean up output so that only info for HRV-related features is displayed
codebook <- datadescriptor.cc1 %>% 
  filter(is.hrv.feature==1) %>%
  mutate(hrv.variable.id = paste("variable",1:nrow(.),sep=".")) %>%
  select(hrv.variable.id, variable.id, name, frequency, statistic, unit, description) %>%
  rename(data.source = name)

colnames(df.raw.hrv.cc1)[grep("variable.",colnames(df.raw.hrv.cc1))] <- codebook$hrv.variable.id
codebook <- codebook %>% select(-variable.id)

# Write data frames to csv file
write.csv(codebook, file.path(path.breakfree.output_data, "hrv_features_description_cc1.csv"), row.names=FALSE, na="")
write.csv(df.raw.hrv.cc1, file.path(path.breakfree.output_data, "hrv_features_cc1.csv"), row.names=FALSE, na="")

