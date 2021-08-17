##################################################################################################
#File loading
#################################################################################################
#load packages -- these are ones used commonly for wx QC
lapply(c("tidyverse", "lubridate", "reshape2", "stringr", "plotly", "roll", "data.table"), library, character.only = TRUE)

#set working directory
setwd("/YOUR/FILE/LOCATIONS")

# check your working directory is in the right spot
getwd()

# Load data - read headers
fileheaders <- read.csv("filename.csv",
                        nrows = 1, as.is = TRUE,
                        header = FALSE)
# Read in data, drop redundant header rows
data <- read.csv("filename.csv",
                 header = FALSE,
                 skip = 4,
                 stringsAsFactors = FALSE)

# Add headers to dataframe
colnames(data) <- fileheaders
names(data)
glimpse(data)
