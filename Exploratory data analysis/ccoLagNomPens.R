# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)

# Read the raw data
train <- readRDS("Data/train.rds")

###############################################################
# Look for patterns in the lag 1 portfolio for cco pos flanks #
###############################################################

# # Look for NA's in May 15 in the target products - all good
# a <- as.matrix(train[fecha_dato=="2015-06-28", -(1:24), with=FALSE])
# apply(a, 2, function(x) sum(is.na(x)))

# Study the positive flanks in cco_fin
studiedNcodpers <- intersect(train[fecha_dato == "2015-05-28", ncodpers],
                             train[fecha_dato == "2015-06-28", ncodpers])
posFlankIdsCcoJun15 <- which(
  train[ncodpers %in% studiedNcodpers & fecha_dato == "2015-05-28",
          ind_cco_fin_ult1 == 0] &
    train[ncodpers %in% studiedNcodpers & fecha_dato == "2015-06-28",
          ind_cco_fin_ult1 == 1])