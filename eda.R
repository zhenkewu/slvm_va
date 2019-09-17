
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
##----------------------------
### load the VA datasets ### 
##----------------------------
setwd("~/Dropbox/Irena/verbal_autopsy_data/")
va_adult_data <- data.table(read.csv("IHME_PHMRC_VA_DATA_ADULT_Y2013M09D11_0.csv"))
va_child_data <- data.table(read.csv("IHME_PHMRC_VA_DATA_CHILD_Y2013M09D11_0.csv"))
va_neonate_data <- data.table(read.csv("IHME_PHMRC_VA_DATA_NEONATE_Y2013M09D11_0.csv"))

##----------------------------
###  ADULT VA exploration ### 
##----------------------------


### First, we should check for missing values: 

colnames(va_adult_data)[colSums(is.na(va_adult_data)) > 0]

## 22 variables actually contain missing values: 

#g1_06y -> year of death
# g1_07b -> last known age of deceased (months)
# g1_07b -> last known age of deceased (days)
# g2_03ay	 -> Date of first interview attempt [year]
# g2_03bd	 -> Date and time arranged for second interview attempt [day]
# g2_03cd -> Date and time arranged for third interview attempt [day]
# g2_03cy	-> Date and time arranged for third interview attempt [year]




va_adult_data$death <- 1 

# va_adult_data$female <- ifelse(va_adult_data$g1_05=="Female",1,0) # convert the gender variable to 1-0 

## we can get the approximate age of the respondents by using the birth date and the death date 
# birth_dates <- paste(va_adult_data$g1_01y, va_adult_data$g1_01m,va_adult_data$g1_01d,sep="-")
# va_adult_data$birth_date <- strptime(birth_dates,format="%Y-%b-%d")
# 
# ## do the same with CoD 
# cod_dates <-paste(va_adult_data$g1_06y, va_adult_data$g1_06m,va_adult_data$g1_06d,sep="-")
# va_adult_data$cod_date <- strptime(cod_dates,format="%Y-%b-%d")

### 

graphData <- va_adult_data[,list(death_count=sum(death)), by=c("gs_level","gs_code34","gs_text34",  #main CoD 
                                                               "gs_code46" , "gs_text46","gs_code55","gs_text55", #more detailed CoD
                                                               "site")]


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
  labs(title="IHME VA Dataset: Adult Causes of Death", fill = "Cause of Death")


## % of deaths 

ggplot(data=graphData)+ 
  geom_bar(mapping = aes(x = meta_cod, y = death_count/sum(death_count),  fill=meta_cod), stat = "identity")  +
  scale_y_continuous(labels = scales::percent) +
  labs(title="IHME VA Dataset: Adult Causes of Death", fill = "Cause of Death")


## display the causes of death in each meta category 
cod_plots <- list()
for(k in unique(graphData$meta_cod)){
  subset <- graphData[meta_cod==k]
  cod_plots[[k]] <- ggplot(data=subset)+ 
    geom_bar(mapping = aes(x = gs_text34, y = death_count,  fill=gs_text34), stat = "identity")  +
    xlab("CoD Category")+
    ylab("Number of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death by Category in ", k), fill = "Cause of Death")
}


## display the causes of death in each meta category 
site_plots <- list()
for(k in unique(graphData$site)){
  subset <- graphData[site==k]
  site_plots[[k]] <- ggplot(data=subset)+ 
    geom_bar(mapping = aes(x = meta_cod, y = death_count/sum(death_count),  fill=meta_cod), stat = "identity")  +
    scale_y_continuous(labels = scales::percent) +
    xlab("Meta CoD Category")+
    ylab("% of Deaths due to Cause") + 
    labs(title=paste0("Cause of Death by Category in ", k), fill = "Cause of Death")
}


### education levels? 


## gender/age


## seasonality











##----------------------------
### # Conversion of polytomous symptoms into dichotomous symptoms  ### 
##----------------------------


## turn fever into dichotomos (was there moderate/severe fever?)
convert_fever <- function(x) {
  response = "Y"
  if(grepl(paste(c("Moderate", "Severe"), collapse = "|"), x)){
    response = response
  }else if(grepl("Mild", x)){
    response = "N"
  } else {
    response = "." ## covnert Don't know to Missing 
  }
  return(response)
}



##----------------------------
###  CHILD VA exploration ### 
##----------------------------
## Visualize the main CoD for children in this dataset 
va_child_data$death <- 1 
graphData <- va_child_data[,list(death_count=sum(death)), by=c("gs_text34", "site")]



### Since there are a relatively high # of unique CoD, we will create a meta-category column to group similar CoD 
## generally following what the IHME website has: 

create_meta_child_cod <- function(x){
  category = x
  if(grepl(x, c("Bite of Venomous Animal", "Violent Death", "Road Traffic", "Poisonings", "Drowning", "Falls", "Fires"))){
    category = "External"
  } else if(grepl(x, c("Encephalitis", "Meningitis", "Measles","Hemorrhagic fever", "Other Infectious Diseases"))){
    category = "Infectious"
  } else if(grepl(x,c("Malaria","AIDS"))){
    category = "Global Epidemic"
  }else if(grepl(x, c("Diarrhea/Dysentery", "Other Digestive Diseases"))){
    category = "Digestive"
  } else if (grepl(x, c("Other Cancers", "Other Defined Causes of Child Deaths"))){
    category = "Other"
  }
}

## DAR = 
## UP = 
## AP = 
## 

ggplot(graphData, aes()) + 
  facet_wrap(~site) 
  



