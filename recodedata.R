# This Script is to Run in order = 1
                # Author : audreytyeo@gmail.com 

######### loading GK's data #####
load("../Data/data.RDat")
# changes will be made to GK's file and saved as "df.Rda" at the end of this script.
# the following code will save it with changes for analysis. 
# loading packages
library(kml)
library(readxl)
library(tidyverse)
library(janitor)
library(viridis)
library(RColorBrewer)
library(knitr)
library(ggplot2)
library(ggmosaic)
######## var WAPP #####
df$WAPP_cont <- as.numeric(df$WAPP_cont)
######## var Area #####
df$Area <- NA
# df$Area <- ifelse(df$Barn == "1" & df$Farm == "13", "Farm13Barn1", ifelse(df$Barn == "2" & df$Farm == "13", "Farm13Barn2",ifelse(df$Barn == "1" & df$Farm == "17", "Farm17Barn1", ifelse(df$Barn == "1" & df$Farm == "11", "Farm11Barn1", NA)) ))
# df$Area = factor(df$Area)
df$Area <- ifelse(df$Barn == "1" & df$Farm == "11", "Barn1", 
                  ifelse(df$Barn == "1" & df$Farm == "13", "Barn2",
                         ifelse(df$Barn == "2" & df$Farm == "13", "Barn3", 
                                ifelse(df$Barn == "1" & df$Farm == "17", "Barn4", NA)) ))
df$Area = factor(df$Area)
######## var matching #####
df$EarTag <- as.numeric(df$EarTag)
which(df$EarTag[df$Area == "Barn1"] %in% df$EarTag[df$Area == "Barn2"])
#[1] 526 540 607 674 741 808 to rename as they are duplications
#none:
which(df$EarTag[df$Area == "Barn1"] %in% df$EarTag[df$Area == "Barn3"])
which(df$EarTag[df$Area == "Barn1"] %in% df$EarTag[df$Area == "Barn4"])
which(df$EarTag[df$Area == "Barn2"] %in% df$EarTag[df$Area == "Barn3"])
which(df$EarTag[df$Area == "Barn2"] %in% df$EarTag[df$Area == "Barn4"])
which(df$EarTag[df$Area == "Barn3"] %in% df$EarTag[df$Area == "Barn4"])

sort(unique(df$EarTag[df$Area == "Barn1"]))
sort(unique(df$EarTag[df$Area == "Barn2"]))
sort(unique(df$EarTag[df$Area == "Barn3"]))
sort(unique(df$EarTag[df$Area == "Barn4"]))

# sort
sort(unique(df$EarTag))
# EarTag 1 to 6 are available labels to reassign to

# Reassign duplicate to new labels
"1" -> df$EarTag[df$Area == "Barn1" & df$EarTag == "526"]
"2" -> df$EarTag[df$Area == "Barn1" & df$EarTag == "540"]
"3" -> df$EarTag[df$Area == "Barn1" & df$EarTag == "607"]
"4" -> df$EarTag[df$Area == "Barn1" & df$EarTag == "674"]
"5" -> df$EarTag[df$Area == "Barn1" & df$EarTag == "741"]
"6" -> df$EarTag[df$Area == "Barn1" & df$EarTag == "808"]

#check
which(df$EarTag[df$Area == "Barn1"] %in% df$EarTag[df$Area == "Barn3"]) # has no entries of the same

######## Reproductive State #####
# df$ReproductiveState = factor(df$ReproductiveState, 
                              # level = c("only-lactating", "late pregnancy" ,"not-pregnant",
                                        # "early pregnancy/lactating" , "middle pregnancy/lactating"), ordered = TRUE)
#view results
df[df$EarTag == "1",]
df[df$EarTag == "2",]

######## remove missing values #####
dim(df)
df <- df[!df$EarTag == "11353",]
######## var meanPDheel : Mean scores from L and R limbs of heel and mid food #####


meanPDheel0 <- rowMeans(df[c('PDHLH', 'PDHRH')], na.rm=TRUE)
meanPDmid0 <- rowMeans(df[c('PDHLM', 'PDHRM')], na.rm=TRUE)
df$Hybrid[df$Hybrid == "F1"] <- "Hylamax"

##scoring:

meanPDmid0<-cut(x = meanPDmid0,breaks = c(-0.1,0.1,1.7,3.3,5,6.7,8.4,10),labels = c(0,1,2,3,4,5,6), right = TRUE)

class(meanPDmid0)<-"numeric"

meanPDheel0<-cut(x = meanPDheel0,breaks = c(-0.1,0.1,1.7,3.3,5,6.7,8.4,10),labels = c(0,1,2,3,4,5,6), right = TRUE)

class(meanPDheel0)<-"numeric"

df$meanPDheel <- meanPDheel0
df$meanPDmid <- meanPDmid0

######## var Claw length #####
#recoding Claw length:
df$Claws[df$Claws=="1 torn out HR, rest normal"]<-"normal"
df$Claws[df$Claws=="1 torn out HR, rest too long"]<-"too long"
df$Claws[df$Claws=="1 torn out VL, rest normal"]<-"normal"
df$Claws[df$Claws=="mormal"]<-"normal"
df$Claws[df$Claws=="normal & torn out 1xvl"]<-"normal"
df$Claws[df$Claws=="too lang"]<-"too long"
df$Claws[df$Claws=="too long, one torn out HL"]<-"too long"
df$Claws[df$Claws=="too long, one torn out HR"]<-"too long"
df$Claws[df$Claws=="torn out (2xhl)/too long"]<-"too long"
df$ReproductiveState[df$ReproductiveState=="only-lacting"]<-"only-lactating"
######## var mmyy #####
df$mmyy[df$visit == "1"] <- "July2016"
df$mmyy[df$visit == "2"] <- "August2016"
df$mmyy[df$visit == "3"] <- "September2016"
df$mmyy[df$visit == "4"] <- "October2016"
df$mmyy[df$visit == "5"] <- "November2016"
df$mmyy[df$visit == "6"] <- "December2016"
df$mmyy[df$visit == "7"] <- "January2017"
df$mmyy[df$visit == "8"] <- "February2017"
df$mmyy[df$visit == "9"] <- "March2017"
df$mmyy[df$visit == "10"] <- "April2017"
df$mmyy[df$visit == "11"] <- "May2017"
df$mmyy[df$visit == "12"] <- "June2017"
df$mmyy[df$visit == "13"] <- "endJune2017"
df$mmyy <- factor(df$mmyy, 
                  levels = c("July2016", "August2016", "September2016", "October2016", 
                             "November2016", "December2016", "January2017", "February2017", "March2017", 
                             "April2017", "May2017", "June2017", "endJune2017"), ordered = TRUE)
df$visit <- factor(df$visit, levels = c("1", "2", "3", "4", "5", "6", "7", "8", 
                                        "9", "10", "11", "12", "13"), ordered = TRUE)
######## var cat #####
df$cat <- NA
df$cat <- ifelse(df$Area == "Barn1", "A", 
                 ifelse(df$Area == "Barn2", "B", 
                        ifelse(df$Area == "Barn3", "C", 
                               ifelse(df$Area == "Barn4", "D", NA)) ))
######## var MdAge, MdWeight, mdScore, (MEDIANS) #####
df %>% 
	group_by(visit, cat) %>% 
	mutate(mdAge = median(Age)) -> df
df %>% 
	group_by(visit, cat) %>% 
	mutate(mdWeight = median(Weight)) -> df
df %>% 
	group_by(visit, cat) %>% 
	mutate(mdScore = median(meanPDheel)) -> df
######## var Clean #####
df$CleanFR <- factor(df$CleanFR, levels = c("clean", "dirty", "very dirty"), ordered = TRUE )
df$CleanFL <- factor(df$CleanFR, levels = c("clean", "dirty", "very dirty"), ordered = TRUE )
df$CleanHL <- factor(df$CleanFR, levels = c("clean", "dirty", "very dirty"), ordered = TRUE )
df$CleanHR <- factor(df$CleanFR, levels = c("clean", "dirty", "very dirty"), ordered = TRUE )
# save
save(df, file = "../Data/df.Rda")

