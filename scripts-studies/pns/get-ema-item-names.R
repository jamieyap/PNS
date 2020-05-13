###############################################################################
# ABOUT:
# * Creates a file with a list of item names in each EMA raw data file;
#   these item names correspond to that recorded in the codebooks
###############################################################################

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")

###############################################################################
# Post Quit EMAs
###############################################################################

# Post Quit Random
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Random.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("postquit.random.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_postquit_random.csv"), row.names = FALSE)

# Post Quit Urge
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Urge.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("postquit.urge.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_postquit_urge.csv"), row.names = FALSE)

# Post Quit About to Slip Part 1
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip.csv"))
ema.item.names <- colnames(df.raw[,32:95])
new.item.names <- paste("postquit.about2slip1.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_postquit_about2slip1.csv"), row.names = FALSE)

# Post Quit About to Slip Part 2
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip_Part2.csv"))
ema.item.names <- colnames(df.raw[,32:105])
new.item.names <- paste("postquit.about2slip2.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_postquit_about2slip2.csv"), row.names = FALSE)

# Post Quit Already Slipped
df.raw <- read.csv(file.path(path.pns.input_data, "Post_Quit_Already_Slipped.csv"))
ema.item.names <- colnames(df.raw[,32:101])
new.item.names <- paste("postquit.alreadyslipped.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_postquit_alreadyslipped.csv"), row.names = FALSE)

###############################################################################
# Pre Quit EMAs
###############################################################################

# Pre Quit Random
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Random.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("prequit.random.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_prequit_random.csv"), row.names = FALSE)

# Pre Quit Urge
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Urge.csv"))
ema.item.names <- colnames(df.raw[,32:98])
new.item.names <- paste("prequit.urge.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_prequit_urge.csv"), row.names = FALSE)

# Pre Quit About to Slip Part 1
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking.csv"))
ema.item.names <- colnames(df.raw[,32:95])
new.item.names <- paste("prequit.about2slip1.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_prequit_about2slip1.csv"), row.names = FALSE)

# Pre Quit About to Slip Part 2
df.raw <- read.csv(file.path(path.pns.input_data, "Pre_Quit_Smoking_Part2.csv"))
ema.item.names <- colnames(df.raw[,32:100])
new.item.names <- paste("prequit.about2slip2.item.", 1:length(ema.item.names), sep="")
df <- data.frame(name.codebook = ema.item.names, name.new = new.item.names)

write.csv(df, file.path(path.pns.output_data, "item_names_prequit_about2slip2.csv"), row.names = FALSE)

