
##----------------------------
### Simulations ### 
##----------------------------

##----------------------------
### set up libraries ### 
##----------------------------
rm(list=ls())
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
##----------------------------
### load the VA datasets ### 
##----------------------------
 
setwd("/Users/irena/repos/slvm_va/data_files/")
dataFreeze <-  data.table(read.csv("data_freeze_10062019.csv"))
dataDict <- data.table(read_excel("data_dictionary.xlsx"))

##setwd("~/Dropbox/Irena/verbal_autopsy_data/")
# va_adult_data <- data.table(read.csv("IHME_PHMRC_VA_DATA_ADULT_Y2013M09D11_0.csv"))
# va_child_data <- data.table(read.csv("IHME_PHMRC_VA_DATA_CHILD_Y2013M09D11_0.csv"))
# va_neonate_data <- data.table(read.csv("IHME_PHMRC_VA_DATA_NEONATE_Y2013M09D11_0.csv"))

##----------------------------
###  ADULT VA exploration ### 
##----------------------------


### First, we should check for missing values: 

## names of the columns w/ missing values
colnames(dataFreeze)[colSums(is.na(dataFreeze)) > 0]
# count how many: 
length(colnames(dataFreeze)[colSums(is.na(dataFreeze)) > 0])

## 135 columns have missing values (responses that were "Don't Know/Refused to Answer", or just NA originally)
na_cols <- colnames(dataFreeze)[colSums(is.na(dataFreeze)) > 0]
na_matrix <- dataFreeze[,na_cols, with=FALSE]

na_matrix_sum <- na_matrix[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = na_cols]

## this means nothing, just to melt the dataset easily
na_matrix_sum$q_type <- "symptom"

# easily see which variables contain the most missing values 
na_matrix_long <- melt(na_matrix_sum, id.vars ="q_type", variable.name = "id")
na_matrix_long$perc_missing <- na_matrix_long$value/7841

shorthandID <- dataDict[, c("IHME_code","short_label"), with=FALSE]
              
na_matrix_long <- merge(na_matrix_long, shorthandID, by.x="id", by.y = "IHME_code", all.x=TRUE)

# ---------------------------------
## Make some graphs of CoD Distributions 
# --------------------------------
dataFreeze$death <- 1 

dataFreeze$sex <- ifelse(dataFreeze$g1_05==1, "Female", "Male") #get gender for plotting purposes

## month of birth date and month of death date 

## we can get the approximate age of the respondents by using the birth date and the death date 
# birth_dates <- paste(va_adult_data$g1_01y, va_adult_data$g1_01m,va_adult_data$g1_01d,sep="-")
# va_adult_data$birth_date <- strptime(birth_dates,format="%Y-%b-%d")
# 
# ## do the same with CoD 
# cod_dates <-paste(va_adult_data$g1_06y, va_adult_data$g1_06m,va_adult_data$g1_06d,sep="-")
# va_adult_data$cod_date <- strptime(cod_dates,format="%Y-%b-%d")

### 
dataFreeze$death <- 1
dataFreeze$sex <- ifelse(dataFreeze$g1_05==1, "Female", "Male") 
graphData <- dataFreeze[,list(death_count=sum(death)), by=c("gs_text34",#main CoD 
                                                            "g4_08", #separate room for cooking
                                                            "g5_06a", "g5_06b", # education level + # of years of education 
                                                            "site", "sex")]


### Since there are a relatively high # of unique CoD, we will create a meta-category column to group similar CoD 
## generally following what the IHME website has: 

create_meta_adult_cod <- function(x){
  category = x
  if(grepl(paste(c("Bite of Venomous Animal", "Violent Death", "Road Traffic",
                   "Poisonings", "Drowning", "Homicide","Falls", "Fires", "Other Injuries"), collapse = "|"),x)){
    category = "External"
  } else if(grepl(paste(c("Diarrhea/Dysentery", "Encephalitis", "Meningitis", "Measles",
                          "Hemorrhagic fever", "Other Infectious Diseases"), collapse = "|"), x)){
    category = "Infectious"
  } else if(grepl(paste(c("Malaria","AIDS", "TB"), collapse = "|"), x)){
    category = "Global Epidemic"
  } else if(grepl(paste(c("Pneumonia", "Asthma","COPD"), collapse = "|"), x)){
    category = "Respiratory"
  } else if(grepl(paste(c("Renal Failure"), collapse = "|"),x)){
    category = "Renal Failure"
  } else if(grepl(paste(c("Acute Myocardial Infarction", "Stroke","Other Cardiovascular Diseases"), collapse = "|"),x)){
    category = "CVD"
  } else if (grepl(paste(c("Stomach Cancer","Breast Cancer", "Esophageal Cancer", "Prostate Cancer",
                           "Colorectal Cancer", "Cervical Cancer", "Lung Cancer", "Leukemia/Lymphomas"), collapse="|"),x)){
    category = "Cancer"
  } else if (grepl(c("Suicide"),x)){
    category = "Suicide"
  } else if (grepl(c("Maternal"),x)){
    category = "Maternal"
  } else if (grepl(paste(c("Diabetes", "Epilepsy","Cirrhosis","Other Non-communicable Diseases"), 
                         collapse="|"),x)){
    category = "Noncommunicable \n Diseases"
  }
  return(category)
}

graphData$meta_cod <- mapply(create_meta_adult_cod, graphData$gs_text34)

## first, plot how many deaths in each "Meta category" 

## raw counts of death 
ggplot(data=graphData)+ 
  geom_bar(mapping = aes(x = meta_cod, y = death_count,  fill=meta_cod), stat = "identity")  +
  labs(title="IHME VA Dataset: Adult Causes of Death", 
       fill = "Cause of Death", x="CoD Category", y="Number of Deaths")

## fit a multinomial regression using CoD as response variable 
##and the regression variables as the predictors 
# We want to do this to assess initially if there is a relationship between these variables
## Future: we want to fit semi supervised models 
## % of deaths 

ggplot(data=graphData)+ 
  geom_bar(mapping = aes(x = meta_cod, y = death_count/sum(death_count),  fill=meta_cod), stat = "identity")  +
  scale_y_continuous(labels = scales::percent) +
  labs(title="IHME VA Dataset: Adult Causes of Death", fill = "Cause of Death",
       x="CoD Category", y="% of Deaths")


## display the causes of death in each meta category 
cod_plots <- list()
for(k in unique(graphData$meta_cod)){
  subset <- graphData[meta_cod==k]
  cod_plots[[k]] <- ggplot(data=subset)+ 
    geom_bar(mapping = aes(x = gs_text34, y = death_count,  fill=gs_text34), stat = "identity")  +
    xlab("CoD Category")+
    ylab("Number of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death by Category in ", k), fill = "Cause of Death") +
    theme(axis.text.y = element_text(angle = 90, hjust = 1))
}


## display the causes of death in each meta category by site location

## since males don't die of maternal causes, just make the bar zero: 
maleMaternal <- data.frame(cbind(site=unique(levels(graphData$site)), 
                                 meta_cod =rep("Maternal", 6),death_count= rep(0,6),
                                 sex= rep("Male",6),
                                 "gs_text34" = rep("Maternal",6),#main CoD 
                                 "g4_08"=rep(NA,6), #separate room for cooking
                                 "g5_06a"=rep(NA,6), 
                                 "g5_06b"=rep(NA,6)
), stringsAsFactors = FALSE)
graphData <-  rbind(graphData,maleMaternal)
graphData$death_count <- as.numeric(graphData$death_count)


site_plots <- list()
for(k in unique(graphData$site)){
  subset <- graphData[site==k]
  site_plots[[k]] <- ggplot(data=subset)+ 
    geom_bar(mapping = aes(x = meta_cod, y = death_count/sum(death_count),  fill=meta_cod), stat = "identity")  +
    scale_y_continuous(labels = scales::percent) +
    xlab("Category")+
    ylab("% of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death by Category in ", k), fill = "Cause of Death") +
    theme(axis.text.x = element_text(angle =45, vjust = 1, hjust = 1))
}


pdf("site_plots.pdf")
invisible(lapply(site_plots, print))
dev.off()

## site and sex plots 
site_and_sex_plots <- list()
for(k in unique(graphData$site)){
  subset <- graphData[site==k]
  site_and_sex_plots[[k]] <- ggplot(data=subset, aes(meta_cod, death_count/sum(death_count)))+ 
    geom_bar(mapping = aes(group=sex, fill=sex), position="dodge", stat = "identity")  +
    scale_y_continuous(labels = scales::percent) +
    xlab("Meta CoD Category")+
    ylab("% of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death by Category in ", k), fill = "Cause of Death")
}


pdf("site_and_sex_plots.pdf")
invisible(lapply(site_and_sex_plots, print))
dev.off()

## display the causes of death in each meta category by sex
sex_plots <- list()
for(k in unique(graphData$sex)){
  subset <- graphData[sex==k]
  sex_plots[[k]] <- ggplot(data=subset)+ 
    geom_bar(mapping = aes(x = meta_cod, y = death_count/sum(death_count),  fill=meta_cod), stat = "identity")  +
    scale_y_continuous(labels = scales::percent) +
    xlab("Meta CoD Category")+
    ylab("% of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death by Category for Sex= ", k), fill = "Cause of Death")
}


maleDataset <- graphData[sex=="Male"]
femaleDataset <- graphData[sex=="Female"]


male_cod_plots <- list()
for(k in unique(maleDataset$meta_cod)){
  subset <- maleDataset[meta_cod==k]
  male_cod_plots[[k]] <- ggplot(data=subset)+ 
    geom_bar(mapping = aes(x = gs_text34, y = death_count/sum(death_count),  fill=gs_text34), 
             stat = "identity")  +
    scale_y_continuous(labels = scales::percent) +
    xlab("Meta CoD Category")+
    ylab("% of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death for Males by Category in ", k), fill = "Cause of Death")
}

female_cod_plots <- list()
for(k in unique(femaleDataset$meta_cod)){
  subset <- femaleDataset[meta_cod==k]
  female_cod_plots[[k]] <- ggplot(data=subset)+ 
    geom_bar(mapping = aes(x = gs_text34, y = death_count/sum(death_count),  fill=gs_text34), 
             stat = "identity")  +
    scale_y_continuous(labels = scales::percent) +
    xlab("Meta CoD Category")+
    ylab("% of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death for Females by Category in ", k), fill = "Cause of Death")
}



## gender/age


## seasonality



