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

# Read EMA item names and wording of question asked to participant
randomEMA.items.cc1 <- read.csv(file.path(path.breakfree.output_data, "randomEMA_items_cc1.csv"), header = TRUE, stringsAsFactors = FALSE)
randomEMA.items.cc2 <- read.csv(file.path(path.breakfree.output_data, "randomEMA_items_cc2.csv"), header = TRUE, stringsAsFactors = FALSE)
smokingEMA.items.cc1 <- read.csv(file.path(path.breakfree.output_data, "smokingEMA_items_cc1.csv"), header = TRUE, stringsAsFactors = FALSE)
smokingEMA.items.cc2 <- read.csv(file.path(path.breakfree.output_data, "smokingEMA_items_cc2.csv"), header = TRUE, stringsAsFactors = FALSE)
stressEMA.items.cc1 <- read.csv(file.path(path.breakfree.output_data, "stressEMA_items_cc1.csv"), header = TRUE, stringsAsFactors = FALSE)
stressEMA.items.cc2 <- read.csv(file.path(path.breakfree.output_data, "stressEMA_items_cc2.csv"), header = TRUE, stringsAsFactors = FALSE)

# Read participant responses to EMA items
randomEMA.responses.cc1 <- read.csv(file.path(path.breakfree.output_data, "randomEMA_responses_cc1.csv"), header = TRUE, stringsAsFactors = FALSE)
randomEMA.responses.cc2 <- read.csv(file.path(path.breakfree.output_data, "randomEMA_responses_cc2.csv"), header = TRUE, stringsAsFactors = FALSE)
smokingEMA.responses.cc1 <- read.csv(file.path(path.breakfree.output_data, "smokingEMA_responses_cc1.csv"), header = TRUE, stringsAsFactors = FALSE)
smokingEMA.responses.cc2 <- read.csv(file.path(path.breakfree.output_data, "smokingEMA_responses_cc2.csv"), header = TRUE, stringsAsFactors = FALSE)
stressEMA.responses.cc1 <- read.csv(file.path(path.breakfree.output_data, "stressEMA_responses_cc1.csv"), header = TRUE, stringsAsFactors = FALSE)
stressEMA.responses.cc2 <- read.csv(file.path(path.breakfree.output_data, "stressEMA_responses_cc2.csv"), header = TRUE, stringsAsFactors = FALSE)

# Perform sanity checks: are questions the same across EMA types?
# If TRUE, then output is ZERO
sum(!(randomEMA.items.cc1$question.text==smokingEMA.items.cc1$question.text))
sum(!(randomEMA.items.cc1$question.text==stressEMA.items.cc1$question.text))
sum(!(randomEMA.items.cc2$question.text==smokingEMA.items.cc2$question.text))
sum(!(randomEMA.items.cc2$question.text==stressEMA.items.cc2$question.text))

# Perform sanity checks: are question types the same across EMA types?
# If TRUE, then output is ZERO
sum(!(randomEMA.items.cc1$question.type==smokingEMA.items.cc1$question.type))
sum(!(randomEMA.items.cc1$question.type==stressEMA.items.cc1$question.type))
sum(!(randomEMA.items.cc2$question.type==smokingEMA.items.cc2$question.type))
sum(!(randomEMA.items.cc2$question.type==stressEMA.items.cc2$question.type))

# Perform sanity checks: are response options the same across EMA types?
# If TRUE, then output is ZERO
sum(!(randomEMA.items.cc1$question.options==smokingEMA.items.cc1$question.options))
sum(!(randomEMA.items.cc1$question.options==stressEMA.items.cc1$question.options))
sum(!(randomEMA.items.cc2$question.options==smokingEMA.items.cc2$question.options))
sum(!(randomEMA.items.cc2$question.options==stressEMA.items.cc2$question.options))

# Grab smoking-related items from each type of EMA
colnames.timevars <- c("user.id","ema.type","status",
                       "prompt.hrts","begin.hrts","end.hrts","timezone.hrts",
                       "prompt.unixts","begin.unixts","end.unixts","with.any.response")

randomEMA.responses.cc1 <- randomEMA.responses.cc1 %>% 
  select(colnames.timevars, "item.39", "item.40", "item.41", "item.42", "item.43") %>% 
  filter(with.any.response==1) %>% 
  select(-with.any.response) %>%
  mutate(user.id = as.character(user.id))

randomEMA.responses.cc2 <- randomEMA.responses.cc2 %>% 
  select(colnames.timevars, "item.39", "item.40", "item.41", "item.42", "item.43") %>% 
  filter(with.any.response==1) %>% 
  select(-with.any.response)

smokingEMA.responses.cc1 <- smokingEMA.responses.cc1 %>% 
  select(colnames.timevars, "item.39", "item.40", "item.41", "item.42", "item.43") %>% 
  filter(with.any.response==1) %>% 
  select(-with.any.response) %>%
  mutate(user.id = as.character(user.id))

smokingEMA.responses.cc2 <- smokingEMA.responses.cc2 %>% 
  select(colnames.timevars, "item.39", "item.40", "item.41", "item.42", "item.43") %>% 
  filter(with.any.response==1) %>% 
  select(-with.any.response)

stressEMA.responses.cc1 <- stressEMA.responses.cc1 %>% 
  select(colnames.timevars, "item.39", "item.40", "item.41", "item.42", "item.43") %>% 
  filter(with.any.response==1) %>% 
  select(-with.any.response) %>%
  mutate(user.id = as.character(user.id))

stressEMA.responses.cc2 <- stressEMA.responses.cc2 %>% 
  select(colnames.timevars, "item.39", "item.40", "item.41", "item.42", "item.43") %>% 
  filter(with.any.response==1) %>% 
  select(-with.any.response)

# Merge smoking-related responses from all EMA types into one file
df <- list(randomEMA.responses.cc1, randomEMA.responses.cc2,
           smokingEMA.responses.cc1, smokingEMA.responses.cc2,
           stressEMA.responses.cc1, stressEMA.responses.cc2)
df <- bind_rows(df)

# Create index for each EMA in data frame
df <- df %>% 
  arrange(user.id, prompt.unixts) %>% 
  group_by(user.id) %>% 
  mutate(ones=1) %>% 
  mutate(order.in.sequence = cumsum(ones)) %>%
  select(-ones) %>%
  select(user.id, ema.type, order.in.sequence, everything())

