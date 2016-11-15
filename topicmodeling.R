##This script runs mallet and saves the results for further analysis. 

##
#Load Libraries
##
library(reshape2)
library(dplyr)
library(tidyr)
library(tools)

##
## Prep and Variable Creation ##
##

# Set working directory
dir <- "~/Dropbox/DigitalComponent/MB_Mining/" # adjust to suit
setwd(dir)

# folder containing txt files for MALLET to work on
importdir <- "~/Dropbox/DigitalComponent/MB_Mining/txt/"

# name of file for MALLET to train model on
output <- "mb.mallet"

# set number of topics for MALLET to use
ntopics <- 60

# set optimisation interval for MALLET to use
optint <-  20

# set file names for output of model, extensions must be as shown
outputstate <-  "topic-state.gz"
outputtopickeys <- "~/Dropbox/DigitalComponent/MB_mining/mallet_output_files/mb_keys.txt"
outputdoctopics <- "~/Dropbox/DigitalComponent/MB_mining/mallet_output_files/mb_composition.txt"
topicwordsweight <- "~/Dropbox/DigitalComponent/MB_Mining/mallet_output_files/topic_words_weight.txt"
stoplistfile <- "~/Dropbox/DigitalComponent/MB_mining/mbstops.txt"

# combine variables into strings ready for command line
##note to self: count also use `--num-top-words 15` to change number of top words in topic. I think the default is 10 
cd <- "cd ~/mallet" # location of the bin directory
import <- paste("bin/mallet import-dir --input", importdir, "--output", output, "--keep-sequence --stoplist-file ", stoplistfile,  sep = " ")
train  <- paste("bin/mallet train-topics  --input", output, "--num-topics", ntopics, "--optimize-interval",  optint, "--random-seed 45 --output-state", outputstate,  "--output-topic-keys", outputtopickeys, "--output-doc-topics", outputdoctopics, "--topic-word-weights-file", topicwordsweight, sep = " ")


##
# setup system enviroment for R
##
MALLET_HOME <- "~/Mallet/" # location of the bin directory

##
#Run Mallet
##
system(paste(cd, import, train, sep = " ; "), invisible = FALSE)



##
# Inspect results and mainpulate to be useful for analysis.
##
setwd(MALLET_HOME)
outputtopickeysresult <- read.table(outputtopickeys, header=F, sep="\t")
outputdoctopicsresult <-read.table(outputdoctopics, header=F, sep="\t")
topicwordsweightresult <- read.table(topicwordsweight, header=F, sep="\t")
head(outputtopickeysresult, n=10)

# manipulate outputdoctopicsresult to be more useful 
dat <- outputdoctopicsresult
l_dat <- reshape(dat, idvar=1:2, varying=list(topics=colnames(dat[,seq(3, ncol(dat)-1, 2)]), 
                                              props=colnames(dat[,seq(4, ncol(dat), 2)])), 
                 direction="long")

w_dat <- dcast(l_dat, V2 ~ V3)
rm(l_dat) # because this is very big but not longer needed

#
## Format, Save, and Output Results
#

# write reshaped table to CSV file for closer inspection
write.csv(w_dat, "~/Dropbox/DigitalComponent/MB_Mining/mallet_output_files/topic_model_table.csv")

#this creates a long data frame of the mallet results with each row containing a document, a topic number, and a proportion
results <- w_dat %>% gather(V2)

#Format results filename
results[1,1] %>% file_path_sans_ext()
results[,"filename"] <- basename(file_path_sans_ext(results$V2)) 
results <- results %>% select(-V2)
results <- rename(results, topic=V2.1)

#read in metadata and join metadata with results
setwd(dir = "~/Dropbox/DigitalComponent/MB_Mining/")
meta <- read.csv(file = "Metadata.csv", header = TRUE, sep = ",") 
meta['filename'] <- file_path_sans_ext(meta$Filename)
results <- left_join(results, meta, by = "filename")
write.csv(results, "mallet_output_files/tmodeling_values.csv")
head(results, n=10)




