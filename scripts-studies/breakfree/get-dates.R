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

df.recorded.quit$noshow.visit2 <- ifelse(noshow.condition01 | noshow.condition02 | noshow.condition03, 1, 0)
df.recorded.quit$dropped <-  ifelse(df.recorded.quit$`# Smoked`=="dropped", 1, 0)
df.recorded.quit$withdrew <-  ifelse(df.recorded.quit$`# Smoked`=="withdrew", 1, 0)

df.recorded.quit <- df.recorded.quit %>%
  mutate(user.id = `PT ID`) %>%
  select(user.id, noshow.visit2, dropped, withdrew)

df.dates <- left_join(x = df.recorded.quit, df.recorded.visit.dates, by = "user.id")

###############################################################################
# Add info on whether individual is a CC1 participant or a CC2 participant
###############################################################################

df.dates <- df.dates %>%
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
  select(user.id, cc.version, noshow.visit2, dropped, withdrew, everything()) %>%
  arrange(desc(is.na(cc.version)), user.id)

###############################################################################
# Calculate Quit Date
###############################################################################

df.dates <- df.dates %>% 
  arrange(cc.version, desc(noshow.visit2), user.id) %>%
  mutate(v1 = as.POSIXct(strptime(v1, format = "%Y-%m-%d", tz = "UTC")),
         v2 = as.POSIXct(strptime(v2, format = "%Y-%m-%d", tz = "UTC")),
         v3 = as.POSIXct(strptime(v3, format = "%Y-%m-%d", tz = "UTC")),
         v4 = as.POSIXct(strptime(v4, format = "%Y-%m-%d", tz = "UTC"))) %>%
  mutate(quit.hrts = v1 + 4*24*60*60) %>%
  mutate(start.study.hrts = v1,
         end.study.hrts = quit.hrts + 10*24*60*60) %>%
  mutate(quit.hrts = quit.hrts + 4*60*60) %>%
  mutate(quit.unixts = as.numeric(quit.hrts),
         start.study.unixts = as.numeric(start.study.hrts),
         end.study.unixts = as.numeric(end.study.hrts)) %>%
  select(user.id, cc.version, noshow.visit2, dropped, withdrew, 
         v1, v2, v3, v4,
         start.study.hrts, quit.hrts, end.study.hrts,
         start.study.unixts, quit.unixts, end.study.unixts) %>%
  mutate(start.study.hrts = strftime(start.study.hrts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE),
         quit.hrts = strftime(quit.hrts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE),
         end.study.hrts = strftime(end.study.hrts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE))

###############################################################################
# Write info
###############################################################################

write.csv(df.dates, file.path(path.breakfree.output_data, "dates.csv"), row.names = FALSE, na = "")

