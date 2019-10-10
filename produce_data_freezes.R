## Produce Data Freezes (datasets that will be used in fitting SLVM for VA)


## load the libraries needed 
rm(list=ls())
library(openVA)
library(ggplot2)
library(data.table)
library(lattice)
library(gplots)

## current date: 
current_date <- "10062019"

## download the PHMRC data, using the openVA package:
PHMRC_full <- data.table(read.csv(getPHMRC_url("adult")))

## get the regression variables we want to use
demo_vars <- PHMRC_full[,c("newid","site",
                           "g4_08", # proxy for income - is there a separate room for cooking?
                           "g5_06a", "g5_06b", # education level + # of years of education 
                           "g1_06y", "g1_06m", "g1_06d", # day, month, year of death 
                           "gs_text34"), with=FALSE]



## convert the symptom questions (apply dichotomization, etc.) to be binary
#this is so that general VA algorithms can be applied to the data 
convert.default <- ConvertData.phmrc(PHMRC_full, phmrc.type = "adult",
                                     cutoff = "default", cause = "va34")

# turn into data table (for easier handling)
convertedData <- data.table(convert.default$output)

## 7/19 - join site and death date to the cleaned dataset 

### 9/19 - update: Richard says that "newid" and "ID" are not equivalent
# just cbind the two datasets 
fullData <- cbind(demo_vars, convertedData)


## convert the Yes values to 1 
fullData[fullData=="Y"] <- 1
fullData[fullData==""] <- 0
fullData[fullData=="."] <- NA

## save as CSV to the repo: 
write.csv(fullData, paste0("~/repos/slvm_va/data_files/data_freeze_", current_date, ".csv"), row.names = FALSE)

