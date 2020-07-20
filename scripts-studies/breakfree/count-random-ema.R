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

randomEMA.responses.cc1 <- read.csv(file.path(path.breakfree.output_data, "randomEMA_responses_cc1.csv"), header = TRUE, stringsAsFactors = FALSE)
randomEMA.responses.cc2 <- read.csv(file.path(path.breakfree.output_data, "randomEMA_responses_cc2.csv"), header = TRUE, stringsAsFactors = FALSE)

###############################################################################
# Select common subset of columns from CC1 & CC2 data
###############################################################################

randomEMA.responses.cc1 <- randomEMA.responses.cc1 %>% 
        select(user.id, cc.version, 
               start.study.hrts, quit.hrts, end.study.hrts,
               start.study.unixts, quit.unixts, end.study.unixts,
               random.ema.id, status, prompt.hrts, prompt.unixts, with.any.response)

randomEMA.responses.cc2 <- randomEMA.responses.cc2 %>% 
        select(user.id, cc.version, 
               start.study.hrts, quit.hrts, end.study.hrts,
               start.study.unixts, quit.unixts, end.study.unixts,
               random.ema.id, status, prompt.hrts, prompt.unixts, with.any.response)

df.random <- rbind(randomEMA.responses.cc1, randomEMA.responses.cc2)
df.random <- df.random %>% mutate(postquit = if_else(prompt.unixts >= quit.unixts, 1, 0))

###############################################################################
# Tabulate number of random EMAs delivered
###############################################################################

tab <- df.random %>% group_by(user.id, postquit) %>% summarise(count.random = n())
tab.post <- tab %>% filter(postquit==1) %>% rename(count.postquit = count.random) %>% select(-postquit)
tab.pre <- tab %>% filter(postquit==0) %>% rename(count.prequit = count.random) %>% select(-postquit)

###############################################################################
# Merge info with dates
###############################################################################

df.dates <- read.csv(file.path(path.breakfree.output_data, "dates.csv"), header = TRUE, stringsAsFactors = FALSE)
df.dates <- df.dates %>% filter(dropped==0 & withdrew==0)
tab.all <- left_join(x = df.dates, y = tab.post, by = "user.id")
tab.all <- left_join(x = tab.all, y = tab.pre, by = "user.id")

tab.all$count.postquit  <- as.numeric(tab.all$count.postquit)
tab.all$count.postquit <- if_else(is.na(tab.all$count.postquit), 0, tab.all$count.postquit)

tab.all$count.prequit <- as.numeric(tab.all$count.prequit)
tab.all$count.prequit <- if_else(is.na(tab.all$count.prequit), 0, tab.all$count.prequit)
tab.all$count.total <- tab.all$count.postquit + tab.all$count.prequit
tab.all <- tab.all %>% arrange(count.postquit)

###############################################################################
# Plot histogram to visualize distribution
###############################################################################

hist(tab.all$count.postquit, breaks = seq(-1,55,4), col="goldenrod3", 
     main = "N=273 (Post-Quit)",
     xlim = c(-5,55), xaxt="n", xlab="Counts of Post-Quit Random EMAs", ylab="Number of Participants")
axis(1, seq(0,50,4))
abline(v=4*10, lwd=5, lty=2)

hist(tab.all$count.prequit, breaks = seq(-1,20,4), col="cornflowerblue", 
     main = "N=273 (Pre-Quit)",
     xlim = c(-5,25), xaxt="n", xlab="Counts of Pre-Quit Random EMAs", ylab="Number of Participants")
axis(1, seq(0,20,4))
abline(v=4*4, lwd=5, lty=2)

hist(tab.all$count.total, breaks = seq(-1,70,4), col="salmon", 
     main = "N=273 (Pre- & Post- Quit)",
     xlim = c(-5,70), xaxt="n", xlab="Counts of Random EMAs (Pre- & Post- Quit)", ylab="Number of Participants")
axis(1, seq(0,70,4))
abline(v=4*14, lwd=5, lty=2)

###############################################################################
# Calculate summary stats
###############################################################################

sum(tab.all$count.postquit < 4)  # 34
sum(tab.all$count.postquit == 0)  # 18

sum(tab.all$count.prequit < 2)  # 7
sum(tab.all$count.prequit == 0)  # 3

###############################################################################
# Create binary variables on particopants to include/exclude based on
# counts of Random EMAs delivered
###############################################################################

tab.all$use.prequit.analysis <- if_else(tab.all$count.prequit < 2, 0, 1)
tab.all$use.postquit.analysis <- if_else(tab.all$count.postquit < 4, 0, 1)
tab.all$use.prepostquit.analysis <- if_else(tab.all$use.prequit.analysis==1 & tab.all$use.postquit.analysis==1, 1, 0)

###############################################################################
# Select columns and save to file
###############################################################################

tab.all <- tab.all %>% 
        select(user.id, cc.version, 
               use.prequit.analysis, use.postquit.analysis, use.prepostquit.analysis) %>%
        arrange(cc.version, user.id)

write.csv(tab.all, file.path(path.breakfree.output_data, "use_in_analysis.csv"), row.names=FALSE, na="")

