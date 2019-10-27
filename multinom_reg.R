
##----------------------------
### Initial multinomial regression to capture covariate effects on cause of death categories ### 
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
library(foreign)
library(nnet)
library(grDevices)
library(gplots)
library(plotly)
library(broom)
##----------------------------
### load the VA datasets ### 
##----------------------------

setwd("/Users/irena/repos/slvm_va/data_files/")
dataFreeze <-  data.table(read.csv("data_freeze_10062019.csv"))
dataDict <- data.table(read_excel("data_dictionary.xlsx"))


dataFreeze$sex <- ifelse(dataFreeze$g1_05==1, "Female", "Male") 
regData <- dataFreeze[,  c("gs_text34",#main CoD 
                              "g4_08", #separate room for cooking
                             "g5_06a", # education level 
                           #"g5_06b",  # of years of education 
                              "g1_06y", "g1_06m", "g1_06d", # day, month, year of death 
                              "site", "sex"), with=FALSE]

## check how many missing values: 
sum(is.na(regData))
## 66 missing values - for now, just drop: 
regData_noNA <- na.omit(regData)
# regData_noNA$cod_dates <-paste(regData_noNA$g1_06y, regData_noNA$g1_06m,
#                                regData_noNA$g1_06d,sep="-")

## 
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

regData_noNA$meta_cod <- mapply(create_meta_adult_cod, regData_noNA$gs_text34)
## remove maternal CoD - since there will be zero counts for males
regData_noNA <- regData_noNA[!meta_cod=="Maternal"]

## -------------------------
## Check that there are no empty cells for the multinom regression 
## ------------------------

## highest counts of month of death are November
regData_noNA$death <- 1
regData_noNA[,list(death_count=sum(death)), by=c("g1_06m")]

# relevel the month variable so November is the reference category: 
regData_noNA$g1_06m <- relevel(regData_noNA$g1_06m, "January")

# regData_noNA$month <- copy(regData_noNA$g1_06m)
# regData_noNA[month=="April", month:="May"]

## for level of education: make "Primary School" the reference category: 
regData_noNA[,list(death_count=sum(death)), by=c("g5_06a")]
regData_noNA$g5_06a <- relevel(regData_noNA$g5_06a,"No Schooling")

## for # of years in school, make 0 the reference category: 10/15 - removed for now because it has many categories
# regData_noNA[,list(death_count=sum(death)), by=c("g5_06b")]
# regData_noNA$g5_06b <- relevel(regData_noNA$g5_06b, "0")

### make "Yes" the reference for "do you have a separate room for cooking" 
regData_noNA[,list(death_count=sum(death)), by=c("g4_08")]
regData_noNA$g4_08 <- relevel(regData_noNA$g4_08, "Yes")


### make CVD the reference category for the broader/coarser causes of death 
regData_noNA[,list(death_count=sum(death)), by=c("meta_cod")]
regData_noNA$meta_cod <- as.factor(regData_noNA$meta_cod)
regData_noNA$meta_cod <- relevel(regData_noNA$meta_cod, "CVD")

### make Mexico the reference category for the broader/coarser causes of death 
regData_noNA[,list(death_count=sum(death)), by=c("site")]
regData_noNA$site <- relevel(regData_noNA$site,"Mexico")


### make male the reference for sex variable: 
regData_noNA[,list(death_count=sum(death)), by=c("sex")]
regData_noNA$sex <- as.factor(regData_noNA$sex)
regData_noNA$sex <- relevel(regData_noNA$sex, "Male")

##### -------------------------
## Multinomial regression 
##### -------------------------
multinom_explore <- multinom(formula =meta_cod~ site + sex +g4_08 + g5_06a + g1_06m, 
                data = regData_noNA,maxit=1000,
                family="multinomial",MaxNWts =1000, reltol=1.0e-12)


multinom_results <- coef(multinom_explore)
multinom_tidy <- tidy(multinom_explore)
multinom_std <- multinom_tidy$std.error

plot(density(abs(colSums(multinom_results))))

heatmap.2(multinom_results,col=blueyelred,margins = c(10, 8))


xform <- list(categoryorder = "array",
              categoryarray = c("(Intercept)",
                                "sexFemale",
                                "g4_08No",
                                "siteAP",
                                "siteUP",
                                "siteBohol",
                                "siteDar",
                                "sitePemba",
                                "g5_06aNo Schooling",
                                "g5_06aHigh School",
                                "g5_06aCollege or Higher",
                                "g5_06aUnknown",
                                "g1_06mFebruary",
                                "g1_06mMarch",
                                "g1_06mApril",
                                "g1_06mMay",
                                "g1_06mJune",
                                "g1_06mJuly",
                                "g1_06mAugust",
                                "g1_06mSeptember",
                                "g1_06mOctober",
                                "g1_06mNovember",
                                "g1_06mDecember",
                                "g1_06mDon't Know"))

plot_ly(z=round(log(multinom_tidy$estimate),3),
        type="heatmap",y=multinom_tidy$y.level,x=multinom_tidy$term,
        text=paste(
          "std. error:", round(multinom_tidy$std.error, 3)
        )) %>%
  layout(xaxis = xform, title="Regression Coefficients")

plot_ly(z=round((multinom_tidy$std.error),3),
        type="heatmap",y=multinom_tidy$y.level,x=multinom_tidy$term) %>%
  layout(xaxis = xform, title="Standard Errors")


###---------------------
### DROP PEMBA 
###---------------------

regData_noPemba <- regData_noNA[site!="Pemba"]
# regData_noPemba <- regData_noPemba[g1_06m!="April"]

regData_noPemba$site <- as.character(regData_noPemba$site)
regData_noPemba$site <- factor(regData_noPemba$site, levels=c("Mexico", "AP", "UP", "Bohol", "Dar"))

multinom_noPemba <- multinom(formula =meta_cod~ site + sex +g4_08 + g5_06a + g1_06m, 
                             data = regData_noPemba,maxit=1000,
                             family="multinomial",MaxNWts =1000, reltol=1.0e-12)



tidy_multinom_noPemba <- tidy(multinom_noPemba)
#multinom_tidy$log_estimate <- log(multinom_tidy$estimate)

xform_noPemba <- list(categoryorder = "array",
              categoryarray = c("(Intercept)",
                                "sexFemale",
                                "g4_08No",
                                "siteAP",
                                "siteUP",
                                "siteBohol",
                                "siteDar",
                                "g5_06aNo Schooling",
                                "g5_06aHigh School",
                                "g5_06aCollege or Higher",
                                "g5_06aUnknown",
                                "g1_06mJanuary",
                                "g1_06mFebruary",
                                "g1_06mMarch",
                                "g1_06mApril",
                                "g1_06mMay",
                                "g1_06mJune",
                                "g1_06mJuly",
                                "g1_06mAugust",
                                "g1_06mSeptember",
                                "g1_06mOctober",
                                "g1_06mDecember",
                                "g1_06mDon't Know"))


plot_ly(z=round(log(tidy_multinom_noPemba$estimate),3),
        type="heatmap",y=tidy_multinom_noPemba$y.level,x=tidy_multinom_noPemba$term,
        text=paste(
          "std. error:", round(tidy_multinom_noPemba$std.error, 3)
        )) %>%
          layout(xaxis = xform_noPemba, title="Regression Coefficients (after excluding Pemba)")

plot_ly(z=round((tidy_multinom_noPemba$std.error),3),
        type="heatmap",y=tidy_multinom_noPemba$y.level,x=tidy_multinom_noPemba$term) %>%
  layout(xaxis = xform_noPemba, title="Standard Errors (after excluding Pemba)")


###---------------------
### DROP "Don't Know" for month of death (there are only six observations)
###---------------------
regData_noPemba <- regData_noPemba[!g1_06m%in%c("Don't Know")]
# regData_noPemba <- regData_noPemba[g1_06m!="April"]

regData_noPemba$g1_06m <- as.character(regData_noPemba$g1_06m)
regData_noPemba$g1_06m <- factor(regData_noPemba$g1_06m)
regData_noPemba$g1_06m <- relevel(regData_noPemba$g1_06m," November")

multinom_noPemba2 <- multinom(formula =meta_cod~ site + sex +g4_08 + g5_06a + g1_06m, 
                             data = regData_noPemba,maxit=1000,
                             family="multinomial",MaxNWts =1000, reltol=1.0e-12)



tidy_multinom_noPemba2 <- tidy(multinom_noPemba2)
#multinom_tidy$log_estimate <- log(multinom_tidy$estimate)

xform_noPemba2 <- list(categoryorder = "array",
                      categoryarray = c("(Intercept)",
                                        "sexFemale",
                                        "g4_08No",
                                        "siteAP",
                                        "siteUP",
                                        "siteBohol",
                                        "siteDar",
                                        "g5_06aNo Schooling",
                                        "g5_06aHigh School",
                                        "g5_06aCollege or Higher",
                                        "g5_06aUnknown",
                                        "g1_06mJanuary",
                                        "g1_06mFebruary",
                                        "g1_06mMarch",
                                        "g1_06mApril",
                                        "g1_06mMay",
                                        "g1_06mJune",
                                        "g1_06mJuly",
                                        "g1_06mAugust",
                                        "g1_06mSeptember",
                                        "g1_06mOctober",
                                        "g1_06mDecember"))


plot_ly(z=round(log(tidy_multinom_noPemba2$estimate),3),
        type="heatmap",y=tidy_multinom_noPemba2$y.level,x=tidy_multinom_noPemba2$term,
        text=paste(
          "std. error:", round(tidy_multinom_noPemba2$std.error, 3)
        )) %>%
  layout(xaxis = xform_noPemba2, title="Regression after removing Pemba and Unknown Month of Death")


plot_ly(z=round(log(tidy_multinom_noPemba2$std.error),3),
        type="heatmap",y=tidy_multinom_noPemba2$y.level,x=tidy_multinom_noPemba2$term,
        text=paste(
          "std. error:", round(tidy_multinom_noPemba2$std.error, 3)
        )) %>%
  layout(xaxis = xform_noPemba2, title="Std. Errors after removing Pemba and Unknown Month of Death") 
