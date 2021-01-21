library(dplyr)

path.pns.code <- Sys.getenv("path.pns.code")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

list.all <- readRDS(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))
df.prequit.smoking.part.two <- list.all[[4]] %>% select(id, record.id, CigJustNow) %>% rename(numcigs = CigJustNow)
df.postquit.smoking.part.two <- list.all[[8]] %>% select(id, record.id, CigJustNow_PostQ_Slip2) %>% rename(numcigs = CigJustNow_PostQ_Slip2)
df.part.two <- rbind(df.prequit.smoking.part.two, df.postquit.smoking.part.two)

list.selected.cols <- list()

for(i in 1:length(list.all)){
  curr.dat <- list.all[[i]] %>% select(id, record.id, assessment.type, with.any.response, record.status, time.unixts)
  list.selected.cols <- append(list.selected.cols, list(curr.dat))
}

df.selected.cols <- bind_rows(list.selected.cols)
df.selected.cols <- df.selected.cols %>% 
  group_by(id) %>% 
  arrange(time.unixts) %>%
  mutate(ones = 1) %>%
  mutate(ema.order = cumsum(ones))

df.selected.cols <- df.selected.cols %>%   
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., 
                    cols.today = c("time.unixts","assessment.type","record.id"), 
                    h = c(1,1,1), 
                    this.numeric = c(TRUE,FALSE,FALSE))) 

df.selected.cols <- left_join(x = df.selected.cols, y = df.part.two, by = c("id","record.id"))

# Now, take subset of columns to use to calculate summary statistics
df.subset <- df.selected.cols %>% 
  filter((assessment.type == "Pre-Quit Smoking Part Two") | (assessment.type == "Post-Quit About to Slip Part Two")) %>%
  filter((assessment.type_shift_minus_1 == "Pre-Quit Smoking Part One") | (assessment.type_shift_minus_1 == "Post-Quit About to Slip Part One"))

df.subset <- df.subset %>% 
  group_by(assessment.type) %>%
  summarise(missing = sum(is.na(numcigs)),
            zero = sum(numcigs == 0, na.rm=TRUE),
            positive = sum(numcigs > 0, na.rm=TRUE))

df.subset$row.total <- rowSums(df.subset[,2:4])
last.row <- c("Column Totals", colSums(df.subset[,2:5]))
df.subset <- rbind(df.subset, last.row)

# Write result to csv file
write.csv(df.subset, file.path(path.pns.code, "checks_button_press/count_any_smoking.csv"), row.names = FALSE, na = "")


