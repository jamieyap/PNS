library(dplyr)

path.pns.code <- Sys.getenv("path.pns.code")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")

list.all <- readRDS(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

for(i in 1:length(list.all)){
  curr.dat <- list.all[[i]] %>% select(id, record.id, assessment.type, with.any.response, record.status)
  list.all[[i]] <- curr.dat
}

df.all <- bind_rows(list.all)

# Tabulate number of records for each possible pair of value for with.any.response and record.status
df.summary.0 <- df.all %>% 
  group_by(assessment.type) %>%
  summarise(completed = sum(with.any.response==0 & record.status=="Completed"),
            incomplete = sum(with.any.response==0 & record.status=="Incomplete/Timed Out"),
            cancelled = sum(with.any.response==0 & record.status=="CANCELLED"),
            fragment = sum(with.any.response==0 & record.status=="FRAGMENT RECORD"))

df.summary.1 <- df.all %>% 
  group_by(assessment.type) %>%
  summarise(completed = sum(with.any.response==1 & record.status=="Completed"),
            incomplete = sum(with.any.response==1 & record.status=="Incomplete/Timed Out"),
            cancelled = sum(with.any.response==1 & record.status=="CANCELLED"),
            fragment = sum(with.any.response==1 & record.status=="FRAGMENT RECORD"))

# Merge into one table and format output
df.summary.0 <- df.summary.0 %>% 
  mutate(with.any.response=0) %>% 
  select(assessment.type, with.any.response, everything())

df.summary.1 <- df.summary.1 %>% 
  mutate(with.any.response=1) %>% 
  select(assessment.type, with.any.response, everything())

df.summary.all <- rbind(df.summary.0, df.summary.1)
df.summary.all$row.total <- rowSums(df.summary.all[,3:6])
last.row <- c("Column Totals", NA, colSums(df.summary.all[,3:7]))
df.summary.all <- rbind(df.summary.all, last.row)

# Write result to csv file
write.csv(df.summary.all, file.path(path.pns.code, "checks_button_press/tabulate_any_response.csv"), row.names = FALSE, na = "")


