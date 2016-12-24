# Extract and store the clients that have train data in May and June 2015
# This can be valuable since clients that surfaced after May/June 2015 might
# show different behavior and require different modeling

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)

# Target date
targetDate <- "12-11-2016"


###################################################################

# Read in the raw cleaned data
train <- readRDS(paste0("../Data/train.rds"))

# Extract clients that had train data in May 2015
May15Clients <- sort(unique(train[fecha_dato == "2015-05-28", ncodpers]))
June15Clients <- sort(unique(train[fecha_dato == "2015-06-28", ncodpers]))

# Store the May and June clients
saveRDS(May15Clients, file.path(getwd(), targetDate, "May15 clients.rds"))
saveRDS(June15Clients, file.path(getwd(), targetDate, "June15 clients.rds"))