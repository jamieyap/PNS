library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)
library(readxl)

path.breakfree.cc1.input_data <- Sys.getenv("path.breakfree.cc1.input_data")
path.breakfree.cc2.input_data <- Sys.getenv("path.breakfree.cc2.input_data")
path.breakfree.other.input_data <- Sys.getenv("path.breakfree.other.input_data")
path.breakfree.staged_data <- Sys.getenv("path.breakfree.staged_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))

###############################################################################
# Determine which participants are CC1 participants and which participants are
# CC2 participants
###############################################################################

# Participant IDs for data collected using CC1 platform
ids.cc1 <- list.files(path = path.breakfree.cc1.input_data)
ids.cc1 <- as.character(ids.cc1)

# Participant IDs for data collected using CC2 platform
ids.cc2 <- list.files(path = path.breakfree.cc2.input_data)
ids.cc2 <- ids.cc2[grepl("aa_", ids.cc2)]
ids.cc2 <- ids.cc2[!grepl("test", ids.cc2)]

###############################################################################
# Read in recorded visit dates for CC1 and CC2 participants
###############################################################################

df.recorded.visit.dates <- read_excel(file.path(path.breakfree.other.input_data, 
                                                "Rice V2 Quit Day Data and Visit Dates.xlsx"), 
                                      col_types = c("numeric", "date", "date", "date","date"),
                                      col_names = TRUE,
                                      sheet = "V1- V4 Dates")

colnames(df.recorded.visit.dates) <- c("user.id", "v1", "v2", "v3", "v4")

###############################################################################
# Read in recorded quit dates for CC1 and CC2 participants
###############################################################################

df.recorded.quit <- read_excel(file.path(path.breakfree.other.input_data, 
                                         "Rice V2 Quit Day Data and Visit Dates.xlsx"), 
                               col_types = c("numeric", "text", "date", "date","text"),
                               col_names = TRUE,
                               sheet = "V2 Quit Day")

###############################################################################
# Begin data preparation tasks
###############################################################################

# Create indicator variables for reasons why quit dates are not recorded
noshow.condition01 <- grepl(pattern = "V2 no show", df.recorded.quit$...5) 
noshow.condition02 <- grepl(pattern = "PT was hospitalized between V2 and V4", df.recorded.quit$...5)
noshow.condition03 <- grepl(pattern = "V2, V3 no show", df.recorded.quit$...5)

df.recorded.quit$noshow.visit02 <- ifelse(noshow.condition01 | noshow.condition02 | noshow.condition03, 1, 0)
df.recorded.quit$dropped <-  ifelse(df.recorded.quit$`# Smoked`=="dropped", 1, 0)
df.recorded.quit$withdrew <-  ifelse(df.recorded.quit$`# Smoked`=="withdrew", 1, 0)

# Prepare columns: human-readable quit dates and times
df.recorded.quit <- df.recorded.quit %>%
  mutate(time.hour =  as.numeric(format(`V2 Time`, "%H")),
         time.minute = as.numeric(format(`V2 Time`, "%M"))/60,
         time.second = as.numeric(format(`V2 Time`, "%S"))/3600) %>%
  mutate(user.id = `PT ID`,
         quit.date = Date,
         quit.hour = time.hour + time.minute + time.second) %>%
  mutate(quit.hour = round(quit.hour, digits=2)) %>%
  mutate(quit.unixts = as.numeric(quit.date)+quit.hour) %>%
  select(user.id,
         noshow.visit02, dropped, withdrew,
         quit.date, quit.hour, quit.unixts)

# Determine which participants did not atend visit 2
ids.noshow.visit02 <- df.recorded.quit %>% 
  filter(noshow.visit02==1) %>%
  select(user.id) %>% 
  use_series(user.id)

# Fill in Quit Dates for participants who did not attend visit 2
df.dates.noshow <- df.recorded.visit.dates %>% 
  filter(user.id %in% ids.noshow.visit02) %>%
  mutate(noshowQD = v1 + 4*24*60*60) %>%  # 4 days after date of first visit
  select(user.id, noshowQD)
  
# Merge info from participants who did not attend visit 2 with info for other participants
df.recorded.quit <- left_join(x = df.recorded.quit, y = df.dates.noshow, by = "user.id")

# Implement decision rules regarding quit dates of participants who did not attend visit 2
df.recorded.quit <- df.recorded.quit %>%
  mutate(quit.date = if_else(noshow.visit02==1, noshowQD, quit.date)) %>%
  mutate(quit.hour = if_else(noshow.visit02==1, 4, quit.hour)) %>%
  mutate(quit.unixts = as.numeric(quit.date)+quit.hour) %>%
  select(-noshowQD)

###############################################################################
# Add info on whether individual is a CC1 participant or a CC2 participant
###############################################################################

df.recorded.quit <- df.recorded.quit %>%
  mutate(tmp.id = paste("aa_", substring(user.id, first=2), sep="")) %>%
  mutate(cc.version = case_when(
    user.id %in% ids.cc1 ~ 1,
    tmp.id %in% ids.cc2 ~ 2,
    TRUE ~ NA_real_
  )) %>%
  mutate(user.id = as.character(user.id)) %>%
  mutate(user.id = case_when(
    cc.version==1 ~ user.id,
    cc.version==2 ~ tmp.id,
    TRUE ~ user.id
  )) %>%
  select(-tmp.id) %>%
  select(user.id, cc.version, noshow.visit02, dropped, withdrew, everything()) %>%
  arrange(desc(is.na(cc.version)), user.id)

###############################################################################
# Remove participants who dropped or withdrew
###############################################################################

df.out <- df.recorded.quit %>% 
  filter(dropped==0 & withdrew==0) %>%
  select(-dropped, -withdrew) %>%
  arrange(cc.version, desc(noshow.visit02), user.id)

###############################################################################
# Write info
###############################################################################

write.csv(df.out, file.path(path.breakfree.output_data, "quit.dates.csv"), row.names = FALSE)

# Remove (almost) everything in the working environment
remove(list = ls())

