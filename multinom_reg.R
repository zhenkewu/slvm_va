
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
                              "g5_06a", "g5_06b", # education level + # of years of education 
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

## check that there are no empty cells for the multinom regression 
regData_noNA$death <- 1
graphData <- regData_noNA[,list(death_count=sum(death)), by=c("meta_cod","g4_08", #separate room for cooking
                                                          "g5_06a", "g5_06b", # education level + # of years of education 
                                                          "g1_06m", # day, month, year of death 
                                                          "site", "sex")]

## remove maternal CoD
regData_noNA <- regData_noNA[!meta_cod=="Maternal"]

## highest counts of month of death are November
regData_noNA$month <- copy(regData_noNA$g1_06m)
regData_noNA[month=="April", month:="May"]

regData_noNA$g1_06m <- relevel(regData_noNA$g1_06m, " November")


## reference category: "Primary School" 
regData_noNA$g5_06a <- relevel(regData_noNA$g5_06a,"Primary School")

## reference category: Don't Know
regData_noNA$g5_06b <- relevel(regData_noNA$g5_06b, "0")

regData_noNA$meta_cod <- as.factor(regData_noNA$meta_cod)
regData_noNA$meta_cod <- relevel(regData_noNA$meta_cod, "CVD")

regData_noNA$site <- relevel(regData_noNA$site,"Mexico")

multinom_explore <- multinom(formula =meta_cod~ site + as.factor(sex) +g4_08 + g5_06a + g1_06m, 
                data = regData_noNA,maxit=1000,
                family="multinomial",MaxNWts =1000, reltol=1.0e-12)


multinom_results <- coef(multinom_explore)
multinom_tidy <- tidy(multinom_explore)
multinom_std <- multinom_tidy$std.error

multinom_tidy$multinom_coef_se <- paste0(round(multinom_tidy$estimate, 4), " (", round(multinom_tidy$std.error,4), ")")

plot(density(abs(colSums(multinom_results))))

result.df <- data.frame(multinom_results, row.names = TRUE)

heatmap.2(multinom_results,col=blueyelred,margins = c(10, 8))


xform <- list(categoryorder = "array",
              categoryarray = c("(Intercept)",
                                "as.factor(sex)Male",
                                "g4_08Yes",
                                "siteAP",
                                "siteUP",
                                "siteBohol",
                                "siteDar",
                                "sitePemba",
                                "g5_06aNo Schooling",
                                "g5_06aHigh School",
                                "g5_06aCollege or Higher",
                                "g5_06Unknown",
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

plot_ly(z=round(log(multinom_tidy$estimate),3),
        type="heatmap",y=multinom_tidy$y.level,x=multinom_tidy$term,
        text=paste(
          "std. error:", round(multinom_tidy$std.error, 3)
        )) %>%
  layout(xaxis = xform)

plot_ly(z=round((multinom_tidy$std.error),3),
        type="heatmap",y=multinom_tidy$y.level,x=multinom_tidy$term) %>%
  layout(xaxis = xform)


###---------------------
### DROP PEMBA 
###---------------------

regData_noPemba <- regData_noNA[site!="Pemba"]
# regData_noPemba <- regData_noPemba[g1_06m!="April"]

multinom_noPemba <- multinom(formula =meta_cod~ site + as.factor(sex) +g4_08 + g5_06a + g1_06m, 
                             data = regData_noPemba,maxit=1000,
                             family="multinomial",MaxNWts =1000, reltol=1.0e-12)



tidy_multinom_noPemba <- tidy(multinom_noPemba)
#multinom_tidy$log_estimate <- log(multinom_tidy$estimate)


plot_ly(z=round(log(tidy_multinom_noPemba$estimate),3),
        type="heatmap",y=tidy_multinom_noPemba$y.level,x=tidy_multinom_noPemba$term,
        text=paste(
          "std. error:", round(tidy_multinom_noPemba$std.error, 3)
        )) %>%
          layout(xaxis = xform)

plot_ly(z=round((tidy_multinom_noPemba$std.error),3),
        type="heatmap",y=tidy_multinom_noPemba$y.level,x=tidy_multinom_noPemba$term) %>%
  layout(xaxis = xform)

