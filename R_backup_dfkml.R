# This Script is to Run in order = 2
                # Author : audreytyeo@gmail.com 

######## Load data #####
load("../Data/df.Rda")
library(kml)
library(tidyverse)
library(ggplot2)
library(janitor)
dev.off()
######## Cld object needed for kml #####
shortdf <- df[, c(3, 40, 37, 38, 41)]
str(shortdf)
spread(shortdf, mmyy, meanPDheel) -> widedf0
#spread(df, mmyy, meanPDheel) -> testwide
head(widedf0)
sum(is.na(widedf0))
#class(widedf0)
#names(widedf0) #is data.frame
widedf1 <- as.matrix(widedf0[, 4:16]) 
#class(widedf1)
#head(widedf1)
sum(is.nan(widedf1))
widedf1[is.nan(widedf1)] <- NA
widedf1 <- imputation(widedf1, "trajMean") 
# I do not think its appropriate to impute actually
# head(widedf1)
# colnames(widedf1)
sum(is.na(widedf1)) # 13
class(widedf1)
# create cld for matrix object pour widedf1 which has been imputated
mycld0 <- clusterLongData(widedf1, timeInData = 1:13)
#this likes matrices, the vignette said both
save(mycld, file = "../Data/cldSDQ0.Rdata")

######### nbCluster = 2 #####
# slow kml not needed
# kml(mycld, toPlot = "both") # runs well, straight from the paper
#choice(mycld)
#plotAllCriterion(mycld) # works # show case
# from now on we use fast kml,
kml(mycld0, nbClusters = 2, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10))

choice(mycld)
plotAllCriterion(mycld) # works # show case

######## nbCluster = 4 #####
#kml(mycld, nbClusters = 4, toPlot = "both")  # slow kml
kml(mycld0, nbClusters = 4, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10))
choice(mycld)
plotAllCriterion(mycld)

######## df = df_longdf : creating df for nbClusters = 2 clusters #####
str(widedf1)
widedf1 <- as.data.frame(widedf1)
#widedf1$cluster <- widedf0$cluster
widedf1$EarTag <- widedf0$EarTag
widedf1$Area <- widedf0$Area
widedf1$kmlclusters2 <- getClusters(mycld0, 2, 
                                    asInteger = FALSE) 
# yup it works after we run at least "slow kml", e.g.  kml(mycld, nbRedraw = 2, toPlot = "both")

######## creating df for nbClusters = 4 clusters #####
widedf1$kmlclusters4 <- getClusters(mycld0, 4, 
                                    asInteger = FALSE) 
# yup it works after we run at least "slow kml", e.g.  kml(mycld, nbRedraw = 2, toPlot = "both")
save(widedf1, file = "../Data/widedf1.Rda")

######## Long form df from kml partitions: for glm and lmer models #####
#df_long <- gather(widedf0, "mmyy", 
# "meanPDheel", -EarTag, -Area, -cluster, -kmlclusters2, -kmlclusters4)
gather(widedf1, mmyy, meanPDheel, -EarTag, 
       -Area, -kmlclusters4, -kmlclusters2) -> testlong

testlong$mmyy <- factor(testlong$mmyy, 
                        c("July2016", "August2016", "September2016", "October2016", 
                                         "November2016", "December2016", "January2017", "February2017", 
                                         "March2017", "April2017", "May2017", "June2017", "endJune2017"), 
                        ordered = TRUE)
names(testlong)
#df_longdf0 <- full_join(testlong, df, by = "EarTag")
df_longdf <- full_join(df, testlong, by = 
                         c("EarTag" = "EarTag", "mmyy" = "mmyy", "Area" = "Area")) 

#df_longdf <- inner_join(df, testlong, 
# by = c("EarTag" = "EarTag", "mmyy" = "mmyy", "Area" = "Area")) 
#names(df_longdf)
#"mmyy" <- colnames(testlong$mmyy2)
#df_longdf <- inner_join(df, testlong, 
# by = c("EarTag" = "EarTag2", "mmyy" = "mmyy2"))

df_longdf$mmyy <- factor(df_longdf$mmyy, 
                         levels = c("July2016", "August2016", "September2016", "October2016", 
                                    "November2016", "December2016", "January2017", "February2017", 
                                    "March2017", "April2017", "May2017", "June2017", "endJune2017"), 
                         ordered = TRUE)
df_longdf$visit <- factor(df_longdf$visit, 
                          levels = 
                            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"), 
                          ordered = TRUE)
names(df_longdf)
# df_longdf <- na.omit(df_longdf)
# first save but see second save below
save(df_longdf, file = "../Data/df_longdf.Rda")
names(df_longdf)
# [1] "ID"                "Pen"              
# [3] "EarTag"            "Hybrid"           
# [5] "Age"               "NoKindlings"      
# [7] "Weight"            "TimeD"            
# [9] "CleanFR"           "CleanFL"          
# [11] "CleanHR"           "CleanHL"          
# [13] "MoistFR"           "MoistFL"          
# [15] "MoistHR"           "MoistHL"          
# [17] "PDFR"              "PDFL"             
# [19] "PDHRM"             "PDHRH"            
# [21] "PDHLM"             "PDHLH"            
# [23] "Claws"             "WoundBite"        
# [25] "WoundFeetHR"       "WoundFeetHL"      
# [27] "RemarksDoe"        "visit"            
# [29] "ID.Farm"           "Barn"             
# [31] "Farm"              "ReproductiveState"
# [33] "WAPP"              "WAPP_cont"        
# [35] "Temperature"       "RelativeHumidity" 
# [37] "Area"              "meanPDheel.x"     
# [39] "meanPDmid"         "mmyy"             
# [41] "cat"               "mdAge"            
# [43] "mdWeight"          "mdScore"          
# [45] "kmlclusters2"      "kmlclusters4"     
# [47] "meanPDheel.y" 
# remove unimputated data
df_longdf <- df_longdf[, -c(38)]
names(df_longdf)
"meanPDheel" -> colnames(df_longdf)[47]
# final save for df_longdf
save(df_longdf, file = "../Data/df_longdf.Rdat")


######## concordance with areas and with each other #####
# 2 kml 
df_longdf %>% 
  tabyl(Area, kmlclusters2) %>% 
  adorn_percentages() %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame() -> area2kml
save(area2kml, file = "../Data/area2kml.Rda")
# 4 kml
df_longdf %>% 
  tabyl(Area, kmlclusters4)  %>% 
  adorn_percentages() %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame()  -> area4kml
save(area4kml, file = "../Data/area4kml.Rda")

# concordance of 2kml vs 4kml

df_longdf %>%
  tabyl(kmlclusters2, kmlclusters4) %>%  
  adorn_percentages() %>% 
  adorn_rounding(2) %>% 
  as.data.frame()  -> conkml

colnames(conkml) <- c( " ", "A", "B", "C", "D")
save(conkml, file = "../Data/conkml.Rda")

######## sample size per cluster #####
# load("../Data/df_longdf.Rda")
df_longdf %>% 
  tabyl(kmlclusters2) %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame() -> twokmlcounts
colnames(twokmlcounts) <- c("kml clusters", "n", "(%)")
save(twokmlcounts, file = "../Data/twokmlcounts.Rda")
df_longdf %>% 
  tabyl(kmlclusters4) %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame() -> fourkmlcounts
colnames(fourkmlcounts) <- c("kml clusters", "n", "(%)")
save(fourkmlcounts, file = "../Data/fourkmlcounts.Rda")
###### score vs kml

a = paste( "A", " (", fourkmlcounts$n[1], ")", sep = "")
b = paste( "B", " (", fourkmlcounts$n[2], ")", sep = "")
c = paste( "C", " (", fourkmlcounts$n[3], ")", sep = "")
d = paste( "D", " (", fourkmlcounts$n[4], ")", sep = "")
ggplot(df_longdf, 
       aes(x = visit, y = meanPDheel.y, 
           colour = kmlclusters4, group = kmlclusters4)) +
  geom_smooth(method = "loess") +
  scale_colour_discrete(name = "Four kml\npartitians (n)", 
                        labels=c(a, b, c, d)) +
  labs(x = "Visit", y = "Score") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
  scale_x_discrete(name = "Visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) 

load("../Data/twokmlcounts.Rda")
a = paste( "A", " (", twokmlcounts$n[1], ")", sep = "")
b = paste( "B", " (", twokmlcounts$n[2], ")", sep = "")
ggplot(df_longdf, 
       aes(x = visit, y = meanPDheel.y, colour = kmlclusters2, group = kmlclusters2)) +
  geom_smooth(method = "loess") +
  labs(x = "Visit", y = "Score") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12)) + 
          expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
  scale_colour_discrete(name = "Two kml\npartitians (n)", 
                        labels=c(a, b)) +
  scale_x_discrete(name = "Visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))



####### Descriptive stats tables #####
# mean score
# kmlclusters2
load("../Data/df_longdf.Rda")
mean(df_longdf[df_longdf$kmlclusters2 == "B",]$meanPDheel.y, na.rm = TRUE)
mean(df_longdf[df_longdf$kmlclusters2 == "A",]$meanPDheel.y, na.rm = TRUE)

#kmlclusters4
mean(df_longdf[df_longdf$kmlclusters4 == "A",]$meanPDheel.y, na.rm = TRUE)
sd(df_longdf[df_longdf$kmlclusters4 == "A",]$meanPDheel.y, na.rm = TRUE)
mean(df_longdf[df_longdf$kmlclusters4 == "B",]$meanPDheel.y, na.rm = TRUE)
mean(df_longdf[df_longdf$kmlclusters4 == "C",]$meanPDheel.y, na.rm = TRUE)
mean(df_longdf[df_longdf$kmlclusters4 == "D",]$meanPDheel.y, na.rm = TRUE)

# table for score
l = sprintf("%.1f (%.1f)", 
            mean(df_longdf[df_longdf$kmlclusters2 == "A",]$meanPDheel.y, na.rm = TRUE), 
            sd(df_longdf[df_longdf$kmlclusters2 == "A",]$meanPDheel.y, na.rm = TRUE))
m = sprintf("%.1f (%.1f)", 
            mean(df_longdf[df_longdf$kmlclusters2 == "B",]$meanPDheel.y, na.rm = TRUE), 
            sd(df_longdf[df_longdf$kmlclusters2 == "B",]$meanPDheel.y, na.rm = TRUE))

n = sprintf("%.1f (%.1f)", 
            mean(df_longdf[df_longdf$kmlclusters4 == "A",]$meanPDheel.y, na.rm = TRUE), 
            sd(df_longdf[df_longdf$kmlclusters4 == "A",]$meanPDheel.y, na.rm = TRUE))
p = sprintf("%.1f (%.1f)", 
            mean(df_longdf[df_longdf$kmlclusters4 == "B",]$meanPDheel.y, na.rm = TRUE), 
            sd(df_longdf[df_longdf$kmlclusters4 == "B",]$meanPDheel.y, na.rm = TRUE))
q = sprintf("%.1f (%.1f)", 
            mean(df_longdf[df_longdf$kmlclusters4 == "C",]$meanPDheel.y, na.rm = TRUE), 
            sd(df_longdf[df_longdf$kmlclusters4 == "C",]$meanPDheel.y, na.rm = TRUE))
r = sprintf("%.1f (%.1f)", 
            mean(df_longdf[df_longdf$kmlclusters4 == "D",]$meanPDheel.y, na.rm = TRUE), 
            sd(df_longdf[df_longdf$kmlclusters4 == "D",]$meanPDheel.y, na.rm = TRUE))
s = c("A of 4 partitions", "B of 4 partitions", "C of 4 partitions", "D of 4 partitions", 
      "A of 2 partitions", "B of two partitions" )
s = factor(s, level = c("A of 4 partitions", "B of 4 partitions", "C of 4 partitions", 
                        "D of 4 partitions", "A of 2 partitions", "B of two partitions" ), 
           ordered = TRUE)
idx = c(1, 3, 2, 4) # sorting as in publication
tab_kmlarea = array(NA, dim=c(3,6))
#df$Area = factor(df$Area)
rownames(tab_kmlarea) = c("  Partitions", "  n", " Mean (SD)")
tab_kmlarea
tab_kmlarea[1,] = s
tab_kmlarea[2,] = c(dim(df_longdf[df_longdf$kmlclusters4 == "A",])[1], 
                    dim(df_longdf[df_longdf$kmlclusters4 == "B",])[1], 
                    dim(df_longdf[df_longdf$kmlclusters4 == "C",])[1], 
                    dim(df_longdf[df_longdf$kmlclusters4 == "D",])[1], 
                    dim(df_longdf[df_longdf$kmlclusters2 == "A",])[1], 
                    dim(df_longdf[df_longdf$kmlclusters2 == "B",])[1])
tab_kmlarea[1,] = c("A of 4 partitions", "B of 4 partitions", 
                    "C of 4 partitions", "D of 4 partitions", 
                    "A of 2 partitions", "B of 2 partitions" )
tab_kmlarea[3,] = c(l, m, n, p , q, r)
tab_kmlarea <- as.data.frame(tab_kmlarea)
t(tab_kmlarea) -> kml24
kml24
save(kml24, file = "../Data/kml24.Rda")

######## kmlcluster4 #####

##### Claws
ggplot(df_longdf) +
  geom_mosaic(aes(x = product(Claws, visit), 
                  fill = Claws , conds = product(kmlclusters2))) +
  theme_minimal() +
  labs(x = "Claw length:Partitian", y = "Visit") +
  scale_fill_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10),
    axis.text.x.bottom = element_text(size = 6, angle = 90))

ggplot(df_longdf) +
  geom_mosaic(aes(x = product(Claws, visit), 
                  fill = Claws , conds = product(kmlclusters4))) +
  theme_minimal() +
  labs(x = "Claw length:Partitian", y = "Visit") +
  scale_fill_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10),
    axis.text.x.bottom = element_text(size = 6, angle = 90))

##### weight

ggplot(df_longdf,
       aes(x = df_longdf$visit, y = df_longdf$Weight, 
           colour = kmlclusters2, group = kmlclusters2)) + theme_gray() +
  geom_smooth(method = "loess") +
  labs(x = "Visit", y = "Weight") +
  scale_x_discrete(name = "Visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  labs(colour = "Two Partitians") +
  expand_limits(x=c(1,13), y=c(4.5, 5.5)) +
  ylab("Weight (kg)") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))


ggplot(df_longdf,
       aes(x = df_longdf$visit, y = df_longdf$Weight, 
           colour = kmlclusters4, group = kmlclusters4)) + theme_gray() +
  geom_smooth(method = "loess") +
  labs(x = "Visit", y = "Weight (kg)") +
  scale_x_discrete(name = "Visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  labs(colour = "Four Partitians") +
  expand_limits(x=c(1,13), y=c(4.5, 6.5)) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

###### age
ggplot(df_longdf, aes(x = df_longdf$visit, 
                      y = df_longdf$Age, colour = kmlclusters2, group = kmlclusters2)) + 
  geom_smooth(method = "loess") +
  scale_x_discrete(name = "Visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  labs(colour = "Two partitians") +
  expand_limits(x=c(1,13), y=c(10, 22.5)) +
  ylab("Age (months)") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

ggplot(df_longdf, aes(x = df_longdf$visit, 
                      y = df_longdf$Age, colour = kmlclusters4, group = kmlclusters4)) + 
  geom_smooth(method = "loess") +
  scale_x_discrete(name = "Visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  labs(colour = "Two partitians") +
  expand_limits(x=c(1,13), y=c(10, 22.5)) +
  ylab("Age (months)") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

###### temp
ggplot(df_longdf, 
       aes(x = visit, y = Temperature, colour = kmlclusters2, group = kmlclusters2)) +
  geom_smooth(method = "loess") +
  labs(x = "visit", y = "Temperature") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  geom_text(aes(x = 7.5, y=18, label = "")) + 
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12)) + 
  expand_limits(x=c(1,13), y=c(10, 27)) +
  scale_x_discrete(name = "visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  scale_colour_discrete(name = "Two partitions")

ggplot(df_longdf, 
       aes(x = visit, y = Temperature, colour = kmlclusters4, group = kmlclusters4)) +
  geom_smooth(method = "loess") +
  labs(x = "visit", y = "Temperature") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  geom_text(aes(x = 7.5, y=18, label = "")) + 
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12)) + 
  expand_limits(x=c(1,13), y=c(10, 27)) +
  scale_x_discrete(name = "visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  scale_colour_discrete(name = "Four partitions")


### rh
ggplot(df_longdf, 
       aes(x = visit, y = RelativeHumidity, colour = kmlclusters2, group = kmlclusters2)) +
  geom_smooth(method = "loess") +
  theme_gray() +
  labs(x = "visit", y = "Relative humidity (%)") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12)) + 
  scale_x_discrete(name = "visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  expand_limits(x=c(1,13), y=c(55, 72)) +
  scale_color_discrete(name = "Two partitians")

ggplot(df_longdf, 
       aes(x = visit, y = RelativeHumidity, colour = kmlclusters4, group = kmlclusters4)) +
  geom_smooth(method = "loess") +
  labs(x = "visit", y = "relative humidity (%)") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  expand_limits(x=c(1,13), y=c(55, 70))  +
  labs(colour = "4 kml clusters") +
  scale_x_discrete(name = "visit", limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  scale_color_discrete(name = "Four partitians")
######## kmlclusters2 #####
df_longdf %>% 
  group_by(visit, kmlclusters2) %>% 
  filter(kmlclusters2 == "A") %>% 
  tabyl(visit, Claws) %>% 
  adorn_percentages() %>% 
  adorn_rounding(digits = 2) %>% 
  mutate(kmlclusters2 = rep("A", 13)) -> kml2clawsA
df_longdf %>% 
  group_by(visit, kmlclusters2) %>% 
  filter(kmlclusters2 == "B") %>% 
  tabyl(visit, Claws) %>% 
  adorn_percentages() %>% 
  adorn_rounding(digits = 2) %>% 
  mutate(kmlclusters2 = rep("B", 13)) -> kml2clawsB

bind_rows(kml2clawsA, kml2clawsB) -> df_kml2claws
save(df_kml2claws, file = "../Data/df_kml2claws.Rda")

#concordance matrix of Farm area and kml clusters
load("../Data/df_longdf.Rda")

table(df_longdf$Area, df_longdf$kmlclusters4) -> area4kml

conareakml4 <- round(prop.table(area4kml, margin = 1), digits = 2) # how much do they agree with uniqe Farms
save(con_areakml4, file = "../Data/con_areakml4.Rda")

areakml2 <- table(df_longdf$Area, df_longdf$kmlclusters2)
con_areakml2 <- round(prop.table(areakml2, margin = 1), digits = 2)
con_areakml2 <- con_areakml2[,-c(3)]
save(con_areakml2, file = "../Data/con_areakml2.Rda")


areakml4 <- table(df_longdf$Area, df_longdf$kmlclusters4)
con_areakml4 <- round(prop.table(areakml4, margin = 1), digits = 2)
save(con_areakml4, file = "../Data/con_areakml2.Rda")
#
###### Generalised linear models to compare kml partitions#####
kml2diff <- summary(glm(kmlclusters2 ~ Claws + Hybrid + 
                          CleanFL + MoistFL + meanPDheel.x, data = df_longdf, family = "binomial"))
varkml2 <- kml2diff$coefficients

kml2difftab = data.frame(matrix(vector(), 3, 5, 
                                dimnames=list(c(), 
                                              c("Variable", "Estimate", "Std. Error", "CI", "P value"))), 
                         stringsAsFactors=F)
kml2difftab[,1] = c("Claws too long", "Hybrid : Hyla", "Mean heel scores")
kml2difftab[1,2] <- varkml2[2,1]
kml2difftab[2,2] <- varkml2[3,1]
kml2difftab[3,2] <- varkml2[9,1]
#Std error
kml2difftab[1,3] <- varkml2[2,2]
kml2difftab[2,3] <- varkml2[3,2]
kml2difftab[3,3] <- varkml2[9,2]
#CI
kml2difftab[1,4] <-  paste(
  varkml2[1,2] + 1.96*varkml2[2,2], 
  varkml2[1,2] - 1.96*varkml2[2,2])
kml2difftab[2,4] <-  paste(
  varkml2[1,3] + 1.96*varkml2[3,2],
  varkml2[1,3] - 1.96*varkml2[3,2])
kml2difftab[3,4] <-  paste(
  varkml2[1,4] + 1.96*varkml2[9,2],
  varkml2[1,4] - 1.96*varkml2[9,2])
# P value
kml2difftab[1,5] <- paste("p", "0.0001", sep = "<")
kml2difftab[2,5] <- paste("p", "0.0001", sep = "<")
kml2difftab[3,5] <- paste("p", "0.0001", sep = "<")



save(kml2difftab, file = "../Data/kml2difftab.Rda")

####### simulation on loss of data #####

set.seed(1989)
obs <- dim(df)[1]
percentloss <- c(0.01, 0.05, 0.10, 0.20)
sizeofloss <- round(obs*percentloss, 0)
sizeofloss 

# remove "percentloss" % of random rows of data
onepercentloss <- sample(1:obs, sizeofloss[1], replace = FALSE)
fivepercentloss <- sample(1:obs, sizeofloss[2], replace = FALSE)
tenpercentloss <- sample(1:obs, sizeofloss[3], replace = FALSE)
twentypercentloss <- sample(1:obs, sizeofloss[4], replace = FALSE)
#class(onepercentloss)

# remove 1% of random rows
df_onepercentloss <- df[-onepercentloss,]
dim(df_onepercentloss) #2586 x 44, 1% of data loss

# remove 5% of random rows
df_fivepercentloss <- df[-fivepercentloss,]
#dim(df_fivepercentloss) #2481 x 44, 5% of data loss

# remove 10% of random rows
df_tenpercentloss <- df[-tenpercentloss,]
#dim(df_tenpercentloss) #2351 x 44, 10% of data loss

# remove 20% of random rows
df_twentypercentloss <- df[-twentypercentloss,]
#dim(df_twentypercentloss) #2091 x 44, 20% of data loss

####### Random losses of data (as stipulated above) #####

####### one percent loss kml and sample size #####
# first create wide form
df_onepercentloss0 <- df_onepercentloss[,c(3, 37,28, 41, 38)]
#sum(is.na(df_onepercentloss0)) # 12
#dim(df_onepercentloss0) #2586 x 5
df_onepercentloss0$visit <- factor(df_onepercentloss0$visit, levels = c("1", "2",
                                                                        "3", "4",
                                                                        "5", "6",
                                                                        "7", "8",
                                                                        "9", "10",
                                                                        "11", "12","13"),
                                   order = TRUE)

df_onepercentloss_wide1 <-spread(df_onepercentloss0, visit, meanPDheel) #2587 x 17
onepercentloss_wide2 <- as.matrix(df_onepercentloss_wide1[,4:16])
#sum(is.na(onepercentloss_wide2)) # is 1898
#sum(is.nan(onepercentloss_wide2)) # is 12
is.nan(onepercentloss_wide2) -> "NA"
opkml <- imputation(as.matrix(df_onepercentloss_wide1[,4:16]), "trajMean")
#sum(is.na(opkml)) #0
save(opkml, file = "../Data/opkml.Rda")
opcld <- cld(traj = opkml, timeInData = 1:13)
save(opcld, file = "../Data/opcld.Rda")

# implement kml on 1 % loss
load("../Data/opcld.Rda")
#kml(opcld, 4, toPlot = "both") # slow kml
kml(opcld, nbClusters = 4, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10)) # fast kml
opkml <- data.frame(opkml)
opkml$kmlclusters4op <- getClusters(opcld, 4, asInteger = FALSE) #likes matrix class
#View(opkml)

op <- gather(opkml, visit, score, -kmlclusters4op)
1 -> op$visit[op$visit == "X1"]
2 -> op$visit[op$visit == "X2"]
3 -> op$visit[op$visit == "X3"]
4 -> op$visit[op$visit == "X4"]
5 -> op$visit[op$visit == "X5"]
6 -> op$visit[op$visit == "X6"]
7 -> op$visit[op$visit == "X7"]
8 -> op$visit[op$visit == "X8"]
9 -> op$visit[op$visit == "X9"]
10 -> op$visit[op$visit == "X10"]
11 -> op$visit[op$visit == "X11"]
12 -> op$visit[op$visit == "X12"]
13 -> op$visit[op$visit == "X13"]
op$visit <- factor(op$visit, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                        "9", "10","11", "12","13"), order = TRUE)
# save op df and create plot (for full data go to kml4score in ch03)
save(op, file = "../Data/op.Rda")

# sample size
# for zero percent loss see "sample size per cluster" in this file  

op %>% 
  tabyl(kmlclusters4op) %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame() -> opkml4counts
colnames(opkml4counts) <- c("kml clusters", "n", "(%)")
save(opkml4counts, file = "../Data/opkml4counts.Rda")

a = paste( "A", " (", opkml4counts$n[1], ")", sep = "")
b = paste( "B", " (", opkml4counts$n[2], ")", sep = "")
c = paste( "C", " (", opkml4counts$n[3], ")", sep = "")
d = paste( "D", " (", opkml4counts$n[4], ")", sep = "")
ggplot(op, aes(x = visit, y = score, group = kmlclusters4op, colour = kmlclusters4op)) +
  geom_smooth(method = "loess") +
  scale_colour_discrete(name = "% loss (n)", breaks = c("A", "B", "C", "D"), 
                    labels = c(a, b, c, d)) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted")  +
  theme(legend.position = "bottom") + 
  expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

######### five percent loss kml #####
# first create wide form
df_fivepercentloss0 <- df_fivepercentloss[,c(3, 37,28, 41, 38)]
#df_fivepercentloss0 <- df_fivepercentloss[,c(3,40, 37,39, 41)]
#sum(is.na(df_fivepercentloss0)) #13
#dim(df_fivepercentloss0) #2481 x 5
df_fivepercentloss0$visit <- factor(df_fivepercentloss0$visit, levels = c("1", "2",
                                                                        "3", "4",
                                                                        "5", "6",
                                                                        "7", "8",
                                                                        "9", "10",
                                                                        "11", "12","13"),
                                   order = TRUE)

df_fivepercentloss_wide1 <-spread(df_fivepercentloss0, visit, meanPDheel)
fivepercentloss_wide2 <- as.matrix(df_fivepercentloss_wide1[,4:16])
#sum(is.na(fivepercentloss_wide2)) # is 2016
#sum(is.nan(fivepercentloss_wide2)) #12
fpkml <- imputation(as.matrix(df_fivepercentloss_wide1[,4:16]), "trajMean")
#fpkml <- fpkml[-61, ]
# fpkml <- data.frame(fpkml) # at fpcld stage, idALL duplicated apparently
sum(is.na(fpkml)) #0
fpcld <- clusterLongData(traj = fpkml, timeInData = 1:13)
save(fpcld, file = "../Data/fpcld.Rda")
#kml(fpcld, 4, toPlot = "both") # slow kml
kml(fpcld, nbClusters = 4, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10)) # fast kml
#onepercentloss_wide3 <- as.data.frame(onepercentloss_wide2[-61,])
dim(fpkml)
fpkml <- data.frame(fpkml)
fpkml$kmlclusters4fp <- getClusters(fpcld, nbCluster = 4, asInteger = FALSE)

fp <- gather(fpkml, visit, score, -kmlclusters4fp)
1 -> fp$visit[fp$visit == "X1"]
2 -> fp$visit[fp$visit == "X2"]
3 -> fp$visit[fp$visit == "X3"]
4 -> fp$visit[fp$visit == "X4"]
5 -> fp$visit[fp$visit == "X5"]
6 -> fp$visit[fp$visit == "X6"]
7 -> fp$visit[fp$visit == "X7"]
8 -> fp$visit[fp$visit == "X8"]
9 -> fp$visit[fp$visit == "X9"]
10 -> fp$visit[fp$visit == "X10"]
11 -> fp$visit[fp$visit == "X11"]
12 -> fp$visit[fp$visit == "X12"]
13 -> fp$visit[fp$visit == "X13"]
fp$visit <- factor(fp$visit, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                        "9", "10","11", "12","13"), order = TRUE)
# save df
save(fp, file = "../Data/fp.Rda")

# sample size 
fp %>% 
  tabyl(kmlclusters4fp) %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame() -> fpkml4counts
colnames(fpkml4counts) <- c("kml clusters", "n", "(%)")
save(fpkml4counts, file = "../Data/fpkml4counts.Rda")

# plot
a = paste( "A", " (", fpkml4counts$n[1], ")", sep = "")
b = paste( "B", " (", fpkml4counts$n[2], ")", sep = "")
c = paste( "C", " (", fpkml4counts$n[3], ")", sep = "")
d = paste( "D", " (", fpkml4counts$n[4], ")", sep = "")

ggplot(fp, aes(x = visit, y = score, group = kmlclusters4fp, colour = kmlclusters4fp)) +
  geom_smooth(method = "loess") +
  scale_colour_discrete(name = "% loss (n)", breaks = c("A", "B", "C", "D"), 
                        labels=c(a, b, c, d)) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom") + 
  expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

######## ten percent loss kml #####
# ten create wide form
#scene 1 use visit as factor for ten percent loss
df_tenpercentloss0 <- df_tenpercentloss[,c(3, 37,28, 41, 38)]
#sum(is.na(df_tenpercentloss0)) #9
#dim(df_tenpercentloss0) #2351 x 5
df_tenpercentloss0$visit <- factor(df_tenpercentloss0$visit, levels = c("1", "2",
                                                                          "3", "4",
                                                                          "5", "6",
                                                                          "7", "8",
                                                                          "9", "10",
                                                                          "11", "12","13"),
                                    order = TRUE)

df_tenpercentloss_wide1 <-spread(df_tenpercentloss0, visit, meanPDheel)
tenpercentloss_wide2 <- as.matrix(df_tenpercentloss_wide1[,4:16])
#sum(is.na(tenpercentloss_wide2)) # is 2104
#sum(is.nan(tenpercentloss_wide2)) # is 9
tpkml <- imputation(as.matrix(df_tenpercentloss_wide1[,4:16]), "trajMean")
#tpkml <- tpkml[-61, ]
#tpkml <- data.frame(tpkml) # at fpcld stage, idALL duplicated apparently
#sum(is.na(tpkml)) #0
class(tpkml)
tpcld <- clusterLongData(traj = tpkml, timeInData = 1:13) #likes matrix class
save(tpcld, file = "../Data/tpcld.Rda")
#kml(tpcld, 4, toPlot = "both") # slow kml
kml(tpcld, nbClusters = 4, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10)) # fast kml
#onepercentloss_wide3 <- as.data.frame(onepercentloss_wide2[-61,])
tpkml <- data.frame(tpkml)
tpkml$kmlclusters4tp <- getClusters(tpcld, nbCluster = 4, asInteger = FALSE) # likes data.frame class
#View(tpkml)

tp <- gather(tpkml, visit, score, -kmlclusters4tp)
1 -> tp$visit[tp$visit == "X1"]
2 -> tp$visit[tp$visit == "X2"]
3 -> tp$visit[tp$visit == "X3"]
4 -> tp$visit[tp$visit == "X4"]
5 -> tp$visit[tp$visit == "X5"]
6 -> tp$visit[tp$visit == "X6"]
7 -> tp$visit[tp$visit == "X7"]
8 -> tp$visit[tp$visit == "X8"]
9 -> tp$visit[tp$visit == "X9"]
10 -> tp$visit[tp$visit == "X10"]
11 -> tp$visit[tp$visit == "X11"]
12 -> tp$visit[tp$visit == "X12"]
13 -> tp$visit[tp$visit == "X13"]
tp$visit <- factor(tp$visit, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                        "9", "10","11", "12","13"), order = TRUE)
# save df
save(tp, file = "../Data/tp.Rda")

# sample size
tp %>% 
  tabyl(kmlclusters4tp) %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame() -> tpkml4counts
colnames(tpkml4counts) <- c("kml clusters", "n", "(%)")
save(tpkml4counts, file = "../Data/tpkml4counts.Rda")

# plot
a = paste( "A", " (", tpkml4counts$n[1], ")", sep = "")
b = paste( "B", " (", tpkml4counts$n[2], ")", sep = "")
c = paste( "C", " (", tpkml4counts$n[3], ")", sep = "")
d = paste( "D", " (", tpkml4counts$n[4], ")", sep = "")

ggplot(tp, aes(x = visit, y = score, group = kmlclusters4tp, colour = kmlclusters4tp)) +
  geom_smooth(method = "loess") +
  scale_colour_discrete(name = "% loss", breaks = c("A", "B", "C", "D"), 
                        labels=c(a, b, c, d)) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom") + 
  expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

######## twenty percent loss kml #####
# twenty create wide form
df_twentypercentloss0 <- df_twentypercentloss[,c(3, 37,28, 41, 38)]
#df_fivepercentloss0 <- df_fivepercentloss[,c(3,40, 37,39, 41)]
#sum(is.na(df_twentypercentloss0)) #8
#dim(df_twentypercentloss0) #2091 x 5
df_twentypercentloss0$visit <- factor(df_twentypercentloss0$visit, levels = c("1", "2",
                                                                        "3", "4",
                                                                        "5", "6",
                                                                        "7", "8",
                                                                        "9", "10",
                                                                        "11", "12","13"),
                                   order = TRUE)

df_twentypercentloss_wide1 <-spread(df_twentypercentloss0, visit, meanPDheel)
twentypercentloss_wide2 <- as.matrix(df_twentypercentloss_wide1[,4:16])
#sum(is.na(twentypercentloss_wide2)) # is 2311
#sum(is.nan(twentypercentloss_wide2)) # is 8
twentypkml <- imputation(as.matrix(df_twentypercentloss_wide1[,4:16]), "trajMean")
#View(twentypkml)
#sum(is.na(twentypkml)) #0
twentypcld <- cld(traj = twentypkml, timeInData = 1:13)
save(twentypcld, file = "../Data/tpcld.Rda")
#kml(twentypcld, 4, toPlot = "both") # slow kml
kml(twentypcld, 4, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10)) # fast kml
twentypkml <- data.frame(twentypkml)
twentypkml$kmlclusters4twentyp <- getClusters(twentypcld, nbCluster = 4, asInteger = FALSE)
#View(twentypkml)
#twentypkml <- data.frame(twentypkml)
twentyp <- gather(twentypkml, visit, score, -kmlclusters4twentyp)
1 -> twentyp$visit[twentyp$visit == "X1"]
2 -> twentyp$visit[twentyp$visit == "X2"]
3 -> twentyp$visit[twentyp$visit == "X3"]
4 -> twentyp$visit[twentyp$visit == "X4"]
5 -> twentyp$visit[twentyp$visit == "X5"]
6 -> twentyp$visit[twentyp$visit == "X6"]
7 -> twentyp$visit[twentyp$visit == "X7"]
8 -> twentyp$visit[twentyp$visit == "X8"]
9 -> twentyp$visit[twentyp$visit == "X9"]
10 -> twentyp$visit[twentyp$visit == "X10"]
11 -> twentyp$visit[twentyp$visit == "X11"]
12 -> twentyp$visit[twentyp$visit == "X12"]
13 -> twentyp$visit[twentyp$visit == "X13"]
twentyp$visit <- factor(twentyp$visit, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                        "9", "10","11", "12","13"), order = TRUE)
twentyp$score <- as.numeric(twentyp$score)
# save df
save(twentyp, file = "../Data/twentyp.Rda")

# sample size
twentyp %>% 
  tabyl(kmlclusters4twentyp) %>% 
  adorn_rounding(digits = 2) %>% 
  as.data.frame() -> twentypkml4counts
colnames(twentypkml4counts) <- c("kml clusters", "n", "(%)")
save(twentypkml4counts, file = "../Data/twentypkml4counts.Rda")

# plot
a = paste( "A", " (", twentypkml4counts$n[1], ")", sep = "")
b = paste( "B", " (", twentypkml4counts$n[2], ")", sep = "")
c = paste( "C", " (", twentypkml4counts$n[3], ")", sep = "")
d = paste( "D", " (", twentypkml4counts$n[4], ")", sep = "")
ggplot(twentyp, aes(x = visit, y = score, 
                    group = kmlclusters4twentyp, colour = kmlclusters4twentyp)) +
  geom_smooth(method = "loess") +
  scale_colour_discrete(name = "% loss", breaks = c("A", "B", "C", "D"), 
                        labels=c(a, b, c, d)) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom") + 
  expand_limits(x=c(1,13), y=c(3.5, 5)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))
######## Means of scores per percentloss data set to generate mse

# table of means for lm model
df_longdf %>%
  select(kmlclusters4, visit, meanPDheel.y) %>% 
  group_by_at(vars(kmlclusters4, visit)) %>% 
  summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
  mutate_if(is.numeric, round, digits=1) %>% 
  as.data.frame() -> zerop_means
colnames(zerop_means)[3] <-  "zp_m"
colnames(zerop_means)[4] <-  "zp_sd"

op %>%
  group_by_at(vars(kmlclusters4op, visit)) %>% 
  summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
  mutate_if(is.numeric, round, digits=1) %>% 
  as.data.frame() -> op_means
colnames(op_means)[3] <-  "op_m"
colnames(op_means)[4] <-  "op_sd"

fp %>%
  group_by_at(vars(kmlclusters4fp, visit)) %>% 
  summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
  mutate_if(is.numeric, round, digits=1) %>% 
  as.data.frame() -> fp_means
colnames(fp_means)[3] <-  "fp_m"
colnames(fp_means)[4] <-  "fp_sd"

tp %>%
  group_by_at(vars(kmlclusters4tp, visit)) %>% 
  summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
  mutate_if(is.numeric, round, digits=1) %>% 
  as.data.frame() -> tp_means
colnames(tp_means)[3] <-  "tp_m"
colnames(tp_means)[4] <-  "tp_sd"

twentyp %>%
  group_by_at(vars(kmlclusters4twentyp, visit)) %>% 
  summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
  mutate_if(is.numeric, round, digits=1) %>% 
  as.data.frame() -> twentyp_means
colnames(twentyp_means)[3] <-  "twentyp_m"
colnames(twentyp_means)[4] <-  "twentyp_sd"

######## lm models and their residuals for loss of data #####
zeropmod = lm(zp_m ~ visit + kmlclusters4, data = zerop_means)
plot(zeropmod)
mean(zeropmod$residuals^2) # [1]  0.02424556

fpmod = lm(fp_m ~ visit + kmlclusters4fp, data = fp_means)
plot(fpmod)
mean(fpmod$residuals^2) # [1] 0.005465976

tpmod = lm(tp_m ~ visit + kmlclusters4tp, data = tp_means)
plot(tpmod)
mean(tpmod$residuals^2) # [1] 0.004252959

twentypmod = lm(twentyp_m ~ visit + kmlclusters4twentyp, data = twentyp_means)
plot(tpmod)
mean(tpmod$residuals^2) # [1] 0.004252959

######## MSE for percent loss data set #####

# Data frame needed long form
# create Data frame for MSE
full_join(df_longdf, zerop_means, 
           by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> hoy
#View(hoy)
full_join(hoy, op_means, 
          by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4op"), copy = FALSE) -> hoit
full_join(hoit, fp_means, 
           by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4fp"), copy = FALSE) -> hoi
full_join(hoi, tp_means, 
           by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4tp"), copy = FALSE) -> salut
full_join(salut, twentyp_means, 
           by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4twentyp"), copy = FALSE) -> hola
#View(hola)
df_longdfpre <- hola
save(df_longdfpre, file = "../Data/df_longdfpre.Rda") # "pre" for preimputation


# mse
#df_fivepercentloss$meanPDheel-df_longdfpre$fp_m
#df_longdfpre$mseA <- df_longdfpre$meanPDheel.y[df_longdfpre$kmlclusters4 == "A"] - df_longdfpre$fp_m[df_longdfpre$kmlclusters4 == "A"]

######## Long form percentloss data frames #####


#df_one$mse_zp <- ((df_longdfpre$meanPDheel.y - df_longdfpre$zp_m)^2) / dim(df_longdfpre)[1]

#df_longdfpre$mseo <- ((df_onepercentloss$meanPDheel - df_longdfpre$op_m)^2) / dim(loner)[1]
# gather 

####### zeroer 0% loss #####
#inner_join(df_longdf, zerop_means, 
#           by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> zeroer
#zeroer$mse_zp <- ((zeroer$meanPDheel.y - zeroer$zp_m)^2) / dim(zeroer)[1]
#names(zeroer)

full_join(df_longdf, zerop_means, 
          by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> zeroer
zeroer$mse_zp <- ((zeroer$meanPDheel.y - zeroer$zp_m)^2) / dim(zeroer)[1]
zeroer$variable <- rep("0 %", dim(zeroer)[1])
# gather zeroer
zero <- zeroer[c(46, 28, 47, 48, 49, 50, 51)]
names(zero)
#zero_long <- gather(zero, "variable", "value", -visit, -meanPDheel.y, -zp_m, -zp_sd, -mse_zp, -kmlclusters4)
# colnames(zero_long)[1] <- "kml cluster"
# colnames(zero_long)[3] <- "score"
# colnames(zero_long)[4] <- "mean"
# colnames(zero_long)[5] <- "sd"
# colnames(zero_long)[6] <- "mse"
# names(zero_long)

######## loner 1 % loss #####
full_join(op, op_means, 
           by = c("visit" = "visit", "kmlclusters4op" = "kmlclusters4op"), copy = FALSE) -> loner
dim(loner)
loner$mse_op <- ((loner$score - loner$op_m)^2) / dim(loner)[1]
loner$variable <- rep("1 %", dim(loner)[1])
#View(loner)
names(loner)

# gather 
#loner_long <- gather(loner, "variable", "value",-visit, -meanPDheel.y, -score, -op_m, -op_sd, -mse_op, -kmlclusters4op)
# colnames(loner_long)[1] <- "kml cluster"
# colnames(loner_long)[3] <- "score"
# colnames(loner_long)[4] <- "mean"
# colnames(loner_long)[5] <- "sd"
# colnames(loner_long)[6] <- "mse"
# names(loner_long)

######### fiver 5 % loss #####
full_join(fp, fp_means, 
           by = c("visit" = "visit", "kmlclusters4fp" = "kmlclusters4fp"), copy = FALSE) -> fiver
dim(fiver)
fiver$mse_fp <- ((fiver$score - fiver$fp_m)^2) / dim(fiver)[1]
#View(fiver)
fiver$variable <- rep("5 %", dim(fiver)[1])
names(fiver)
# gather 
#fiver_long <- gather(fiver, "variable", "value",-visit, -score, -fp_m, -fp_sd, -mse_fp, -kmlclusters4fp)
# colnames(fiver_long)[1] <- "kml cluster"
# colnames(fiver_long)[3] <- "score"
# colnames(fiver_long)[4] <- "mean"
# colnames(fiver_long)[5] <- "sd"
# colnames(fiver_long)[6] <- "mse"
# names(fiver_long)

######### tenner 10 % loss#####
full_join(tp, tp_means, 
           by = c("visit" = "visit", "kmlclusters4tp" = "kmlclusters4tp"), copy = FALSE) -> tenner
dim(tenner)
tenner$mse_tp <- ((tenner$score - tenner$tp_m)^2) / dim(tenner)[1]
tenner$variable <- rep("10 %", dim(tenner)[1])
#View(tenner)
names(tenner)
# gather 
#tenner_long <- gather(tenner, "variable", "value", -visit, -score, -fp_m, -tp_sd, -mse_tp, -kmlclusters4tp)
# colnames(tenner_long)[1] <- "kml cluster"
# colnames(tenner_long)[3] <- "score"
# colnames(tenner_long)[4] <- "mean"
# colnames(tenner_long)[5] <- "sd"
# colnames(tenner_long)[6] <- "mse"
# names(tenner_long)

######### twentier 20 % loss #####

full_join(twentyp, twentyp_means, 
           by = c("visit" = "visit", "kmlclusters4twentyp" = "kmlclusters4twentyp"), copy = FALSE) -> twentier
dim(twentier)
twentier$mse_twenty <- ((twentier$score - twentier$twentyp_m)^2) / dim(twentier)[1]
twentier$variable <- rep("20 %", dim(twentier)[1])
#View(twentier)
names(twentier)
# gather 
# twentier_long <- gather(twentier, "variable", "value", -visit, -score, -twentyp_sd, -mse_twenty, -kmlclusters4twentyp)
# colnames(twentier_long)[1] <- "kml cluster"
# colnames(twentier_long)[4] <- "sd"
# colnames(twentier_long)[5] <- "mse"
# names(twentier_long)

######## rbind


# check

zeroer <- zeroer[c(46, 28, 47, 48, 49, 50, 51)]


colnames(zeroer) <- c("kml clusters", "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(loner) <- c("kml clusters", "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(fiver) <- c("kml clusters", "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(tenner) <- c("kml clusters", "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(twentier) <- c("kml clusters", "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

names(zeroer)
names(loner)
names(fiver)
names(tenner)
names(twentier)

er_long <- data.frame(rbind(loner, zeroer, fiver, tenner, twentier))

names(er_long)

er_long$variable <- factor(er_long$variable, level = c("0 %", "1 %", "5 %", "10 %", "20 %"), 
                           ordered = TRUE)

# change names for er_long
# er_long$variable[er_long$variable == "zp_m"] <- "0 %"
# er_long$variable[er_long$variable == "op_m"] <- "1 %"
# er_long$variable[er_long$variable == "fp_m"] <- "5 %"
# er_long$variable[er_long$variable == "tp_m"] <- "10 %"
# er_long$variable[er_long$variable == "twentyp_m"] <- "20 %"
# er_long$variable <- factor(er_long$variable, levels = c("0 %", "5 %", "10 %", "20 %", ordered = TRUE))
unique(er_long$variable)
save(er_long, file = "../Data/er_long.Rda")
# sample size
er_long %>% 
  tabyl(variable, kml.clusters) %>% 
  adorn_totals(where = "col") -> tab_loss # this one deleted the data first, then performed imputation
tab_loss
save(tab_loss, file = "../Figures/tab_loss.Rda")
######## MSE plots for percent loss and no loss #####
# sample size
load("../Data/opkml4counts.Rda")
load("../Data/fpkml4counts.Rda")
load("../Data/tpkml4counts.Rda")
load("../Data/twentypkml4counts.Rda")

# full data, no loss here
fourkmlcounts$`Percentage loss` <- rep("0 %", dim(fourkmlcounts)[1])
opkml4counts$`Percentage loss` <- rep("1 %", dim(opkml4counts)[1])
fpkml4counts$`Percentage loss` <- rep("5 %", dim(fpkml4counts)[1])
tpkml4counts$`Percentage loss` <- rep("10 %", dim(tpkml4counts)[1])
twentypkml4counts$`Percentage loss` <- rep("20 %", dim(twentypkml4counts)[1])

ssize_kml <- rbind(fourkmlcounts, opkml4counts, fpkml4counts, tpkml4counts, twentypkml4counts)
ssize_kml$`kml clusters` <- factor(ssize_kml$`kml clusters`, levels = c("A", "B", "C", "D"), ordered = TRUE)
#ssize_kml$`kml clusters` <- sort(ssize_kml$`kml clusters`)
#ssize_kml$`Percentage loss` <- sort(ssize_kml$`Percentage loss`)

a = paste( "0 %", " (", ssize_kml$n[ssize_kml$`kml clusters` == "A" & ssize_kml$`Percentage loss` == "0 %"], ")", sep = "")
b = paste( "1 %", " (", ssize_kml$n[ssize_kml$`kml clusters` == "A" & ssize_kml$`Percentage loss` == "1 %"], ")", sep = "")
c = paste( "5 %", " (", ssize_kml$n[ssize_kml$`kml clusters` == "A" & ssize_kml$`Percentage loss` == "5 %"], ")", sep = "")
d = paste( "10 %", " (", ssize_kml$n[ssize_kml$`kml clusters` == "A" & ssize_kml$`Percentage loss` == "10 %"], ")", sep = "")
e = paste( "20 %", " (", ssize_kml$n[ssize_kml$`kml clusters` == "A" & ssize_kml$`Percentage loss` == "20 %"], ")", sep = "")

# Cluster A of 4kml with all losses
ggplot(er_long[er_long$kml.clusters == "A",]) +
  geom_smooth(aes(x = visit, y = mse.per.loss, 
            group = variable, colour = variable), method = "loess", se = FALSE) +
  scale_colour_discrete(breaks = c("0 %", "1 %", "5 %", "10 %", "20 %"), 
                        labels = c(a, b, c, d, e)) +
  labs(colour = "% loss (n)", y = "mse") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom") +
  expand_limits(x=c(1,13)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) 
#-> mseA

# Cluster B of 4kml with all losses
a = paste( "0 %", " (",
           ssize_kml$n[ssize_kml$`kml clusters` == "B" & ssize_kml$`Percentage loss` == "0 %"], ")", sep = "")
b = paste( "1 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "B" & ssize_kml$`Percentage loss` == "1 %"], ")", sep = "")
c = paste( "5 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "B" & ssize_kml$`Percentage loss` == "5 %"], ")", sep = "")
d = paste( "10 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "B" & ssize_kml$`Percentage loss` == "10 %"], ")", sep = "")
e = paste( "20 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "B" & ssize_kml$`Percentage loss` == "20 %"], ")", sep = "")
#c(a, b, c, d, e)
ggplot(er_long[er_long$kml.clusters == "B",]) +
  geom_smooth(aes(x = visit, y = mse.per.loss, 
                  group = variable, colour = variable), method = "loess", se = FALSE) +
  scale_colour_discrete(breaks = c("0 %", "1 %", "5 %", "10 %", "20 %"), 
                        labels = c(a, b, c, d, e)) +
  labs(colour = "% loss (n)", y = "mse") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  expand_limits(x=c(1,13)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) +
  theme(legend.position = "bottom",
        axis.title.x  = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7),
        axis.title.y = element_text(size = 12))

#-> mseB

# Cluster C of 4kml with all losses
a = paste( "0 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "C" & ssize_kml$`Percentage loss` == "0 %"], ")", sep = "")
b = paste( "1 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "C" & ssize_kml$`Percentage loss` == "1 %"], ")", sep = "")
c = paste( "5 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "C" & ssize_kml$`Percentage loss` == "5 %"], ")", sep = "")
d = paste( "10 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "C" & ssize_kml$`Percentage loss` == "10 %"], ")", sep = "")
e = paste( "20 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "C" & ssize_kml$`Percentage loss` == "20 %"], ")", sep = "")
#c(a, b, c, d, e)
ggplot(er_long[er_long$kml.clusters == "C",]) +
  geom_smooth(aes(x = visit, y = mse.per.loss, 
                  group = variable, colour = variable), method = "loess", se = FALSE) +
  scale_colour_discrete(breaks = c("0 %", "1 %", "5 %", "10 %", "20 %"), 
                        labels = c(a, b, c, d, e)) +
  labs(colour = "% loss (n)", y = "mse") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  expand_limits(x=c(1,13)) + theme(legend.position = "bottom") +
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) 
#-> mseC

# Cluster D of 4kml with all losses
a = paste( "0 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "D" & ssize_kml$`Percentage loss` == "0 %"], ")", sep = "")
b = paste( "1 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "D" & ssize_kml$`Percentage loss` == "1 %"], ")", sep = "")
c = paste( "5 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "D" & ssize_kml$`Percentage loss` == "5 %"], ")", sep = "")
d = paste( "10 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "D" & ssize_kml$`Percentage loss` == "10 %"], ")", sep = "")
e = paste( "20 %", " (", 
           ssize_kml$n[ssize_kml$`kml clusters` == "D" & ssize_kml$`Percentage loss` == "20 %"], ")", sep = "")
#c(a, b, c, d, e)
ggplot(er_long[er_long$kml.clusters == "D",]) +
  geom_smooth(aes(x = visit, y = mse.per.loss, 
                  group = variable, colour = variable), method = "loess", se = FALSE) +
  scale_colour_discrete(breaks = c("0 %", "1 %", "5 %", "10 %", "20 %"), 
                        labels = c(a, b, c, d, e)) +
  labs(colour = "% loss (n)", y = "mse") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  expand_limits(x=c(1,13)) + theme(legend.position = "bottom") +
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) 
#-> mseD

######### Score trajectory for percent loss and no loss #####

load("../Data/op.Rda")
load("../Data/fp.Rda")
load("../Data/tp.Rda")
load("../Data/twentyp.Rda")

op_long <- gather(op, kmlclusters4op, value, -score, -visit)
fp_long <- gather(fp, kmlclusters4fp, value, -score, -visit)
tp_long <- gather(tp, kmlclusters4tp, value, -score, -visit)
twentyp_long <- gather(twentyp, kmlclusters4twentyp, value, -score, -visit)

# reshape data
# for data frame op

op_long$loss <- rep("one percent loss", dim(op_long)[1])
op_long <- op_long[, -3]
head(op_long)

# for data frame fp
fp_long$loss <- rep("five percent loss", dim(fp_long)[1])
fp_long <- fp_long[, -3]
head(fp_long)

# for data frame tp
tp_long$loss <- rep("ten percent loss", dim(tp_long)[1])
tp_long <- tp_long[, -3]
head(tp_long)

# for data frame twentyp
twentyp_long$loss <- rep("twenty percent loss", dim(twentyp_long)[1])
twentyp_long <- twentyp_long[, -3]
head(twentyp_long)

#for data frame no loss

no_loss <- df_longdf[,c(28, 47, 46)]
no_loss$loss <- rep("zero percent loss", dim(df_longdf)[1])
colnames(no_loss) <- c("visit", "score", "value", "loss")

loss_long <- rbind(no_loss, op_long, fp_long, tp_long, twentyp_long)
loss_long$loss <- factor(loss_long$loss, c("zero percent loss", "one percent loss", 
                                           "five percent loss", "ten percent loss",
                                           "twenty percent loss"), order = TRUE)
save(loss_long, file = "../Data/loss_long.Rda")

######## Score in Loss plots #####
a = paste( "0 %", " (", tab_loss$A[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss$A[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss$A[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss$A[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss$A[5], ")", sep = "")
# loss plot for kml cluster A
ggplot(loss_long[loss_long$value == "A",], aes(x = visit, y = as.numeric(score), group = loss, 
                      colour = loss)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  scale_colour_discrete(name = "% loss (n)", 
                        labels = c(a,b,c,d, e)) +
  labs(y = "score") + 
  theme(legend.position = "bottom") + 
  expand_limits(x=c(1,13), y=c(3.7, 4)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

# loss plot for kml cluster B
a = paste( "0 %", " (", tab_loss$B[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss$B[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss$B[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss$B[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss$B[5], ")", sep = "")
ggplot(loss_long[loss_long$value == "B",], aes(x = visit, y = as.numeric(score), group = loss, 
                                               colour = loss)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_colour_discrete(name = "% loss (n)",  
                        labels = c(a,b,c,d,e)) +
  labs(y = "score") + 
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  expand_limits(x=c(1,13), y=c(3.7, 4)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

# loss plot for kml cluster C
a = paste( "0 %", " (", tab_loss$C[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss$C[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss$C[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss$C[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss$C[5], ")", sep = "")
ggplot(loss_long[loss_long$value == "C",], aes(x = visit, y = as.numeric(score), group = loss, 
                                               colour = loss)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_colour_discrete(name = "% loss (n)", 
                        labels = c(a,b,c,d,e)) +
  labs(y = "score") + 
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom") + 
  expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

# loss plot for kml cluster D
a = paste( "0 %", " (", tab_loss$D[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss$D[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss$D[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss$D[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss$D[5], ")", sep = "")
ggplot(loss_long[loss_long$value == "D",], aes(x = visit, y = as.numeric(score), group = loss, 
                                               colour = loss)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_colour_discrete(name = "% loss (n)", 
                        labels = c(a,b,c,d,e)) +
  labs(y = "score") + 
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom") + 
  expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

### end of loss plots and loss ###



