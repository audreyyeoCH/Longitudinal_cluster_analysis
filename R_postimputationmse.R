####### simulation on loss of data #####

set.seed(1964) # Michelle Obama's birth year
obs <- dim(df_longdf)[1]
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
df_onepercentloss0 <- df_longdf[-onepercentloss,]
#dim(df_onepercentloss0) #4446 x 47, 1% of data loss

# remove 5% of random rows
df_fivepercentloss0 <- df_longdf[-fivepercentloss,]
#dim(df_fivepercentloss0) #4341 x 47, 5% of data loss

# remove 10% of random rows
df_tenpercentloss0 <- df_longdf[-tenpercentloss,]
#dim(df_tenpercentloss0) #4211 x 47, 10% of data loss

# remove 20% of random rows
df_twentypercentloss0 <- df_longdf[-twentypercentloss,]
#dim(df_twentypercentloss0) #3950 x 47, 20% of data loss

save(df_onepercentloss0, file = "../Data/df_onepercentloss0.Rda")
save(df_fivepercentloss0, file = "../Data/df_fivepercentloss0.Rda")
save(df_tenpercentloss0, file = "../Data/df_tenpercentloss0.Rda")
save(df_twentypercentloss0, file = "../Data/df_twentypercentloss0.Rda")


####### Random losses of data (as stipulated above) #####

df_onepercentloss0 %>% 
	tabyl(kmlclusters4) %>% 
	adorn_rounding(digits = 2) %>% 
	as.data.frame() %>% 
	adorn_totals()-> opkml4counts0
colnames(opkml4counts0) <- c("kml clusters", "n", "(%)")
save(opkml4counts0, file = "../Data/opkml4counts0.Rda")

a = paste( "A", " (", opkml4counts0$n[1], ")", sep = "")
b = paste( "B", " (", opkml4counts0$n[2], ")", sep = "")
c = paste( "C", " (", opkml4counts0$n[3], ")", sep = "")
d = paste( "D", " (", opkml4counts0$n[4], ")", sep = "")
ggplot(df_onepercentloss0, aes(x = visit, y = meanPDheel.y, 
																group = kmlclusters4, colour = kmlclusters4)) +
	geom_smooth(method = "loess") +
	scale_colour_discrete(name = "Four partitions (n)", breaks = c("A", "B", "C", "D"), 
												labels=c(a, b, c, d)) +
	labs( y = "Score") +
	geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
	theme(legend.position = "bottom",
				axis.title = element_text(size = 12),
				legend.title = element_text(size = 8),
				legend.text = element_text(size = 8)) + 
	expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
	scale_x_discrete(name = "Visit", 
									 limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

######### five percent loss kml #####

# sample size 
df_fivepercentloss0 %>% 
	tabyl(kmlclusters4) %>% 
	adorn_rounding(digits = 2) %>% 
	as.data.frame() %>% 
	adorn_totals() -> fpkml4counts0
colnames(fpkml4counts0) <- c("kml clusters", "n", "(%)")
save(fpkml4counts0, file = "../Data/fpkml4counts0.Rda")

# plot
a = paste( "A", " (", fpkml4counts0$n[1], ")", sep = "")
b = paste( "B", " (", fpkml4counts0$n[2], ")", sep = "")
c = paste( "C", " (", fpkml4counts0$n[3], ")", sep = "")
d = paste( "D", " (", fpkml4counts0$n[4], ")", sep = "")

ggplot(df_fivepercentloss0, aes(x = visit, y = meanPDheel.y, 
                                group = kmlclusters4, colour = kmlclusters4)) +
	geom_smooth(method = "loess") +
	scale_colour_discrete(name = "Four partitions (n)", breaks = c("A", "B", "C", "D"), 
												labels=c(a, b, c, d)) +
  labs( y = "Score") +
	geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
	theme(legend.position = "bottom",
	      axis.title = element_text(size = 12),
	      legend.title = element_text(size = 8),
	      legend.text = element_text(size = 8)) + 
	expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
	scale_x_discrete(name = "Visit", 
									 limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

######## ten percent loss kml #####

# sample size
df_tenpercentloss0 %>% 
	tabyl(kmlclusters4) %>% 
	adorn_rounding(digits = 2) %>% 
	as.data.frame() %>% 
	adorn_totals()-> tpkml4counts0
colnames(tpkml4counts0) <- c("kml clusters", "n", "(%)")
save(tpkml4counts0, file = "../Data/tpkml4counts0.Rda")

# plot
a = paste( "A", " (", tpkml4counts0$n[1], ")", sep = "")
b = paste( "B", " (", tpkml4counts0$n[2], ")", sep = "")
c = paste( "C", " (", tpkml4counts0$n[3], ")", sep = "")
d = paste( "D", " (", tpkml4counts0$n[4], ")", sep = "")
ggplot(df_tenpercentloss0, aes(x = visit, y = meanPDheel.y, 
                               
                               group = kmlclusters4, colour = kmlclusters4)) +
	geom_smooth(method = "loess") +
	scale_colour_discrete(name = "Four partitions (n)", breaks = c("A", "B", "C", "D"), 
												labels=c(a, b, c, d)) +
  labs(y = "Score") +
	geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
	theme(legend.position = "bottom",
	      axis.title = element_text(size = 12),
	      legend.title = element_text(size = 8),
	      legend.text = element_text(size = 8)) + 
	expand_limits(x=c(1,13), y=c(3.5, 5.5)) + 
	scale_x_discrete(name = "Visit", 
									 limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

######## twenty percent loss kml #####

# sample size
df_twentypercentloss0 %>% 
	tabyl(kmlclusters4) %>% 
	adorn_rounding(digits = 2) %>% 
	as.data.frame() -> twentypkml4counts0
colnames(twentypkml4counts0) <- c("kml clusters", "n", "(%)")
save(twentypkml4counts0, file = "../Data/twentypkml4counts0.Rda")

# plot
a = paste( "A", " (", twentypkml4counts0$n[1], ")", sep = "")
b = paste( "B", " (", twentypkml4counts0$n[2], ")", sep = "")
c = paste( "C", " (", twentypkml4counts0$n[3], ")", sep = "")
d = paste( "D", " (", twentypkml4counts0$n[4], ")", sep = "")
ggplot(df_twentypercentloss0, aes(x = visit, y = meanPDheel.y, 
                                  group = kmlclusters4, colour = kmlclusters4)) +
	geom_smooth(method = "loess") +
	scale_colour_discrete(name = "Four partitions (n)", breaks = c("A", "B", "C", "D"), 
												labels=c(a, b, c, d)) +
	geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
	theme(legend.position = "bottom",
	      axis.title = element_text(size = 12),
	      legend.title = element_text(size = 8),
	      legend.text = element_text(size = 8)) + 
  labs(y = "Score") +
	expand_limits(x=c(1,13), y=c(3.5, 5)) + 
	scale_x_discrete(name = "Visit", 
									 limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

######## Means of scores per percentloss data set

# table of means for lm model
df_longdf %>%
	select(kmlclusters4, visit, meanPDheel.y) %>% 
	group_by_at(vars(kmlclusters4, visit)) %>% 
	summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
	mutate_if(is.numeric, round, digits=1) %>% 
	as.data.frame() -> zerop_means0
colnames(zerop_means0)[3] <-  "zp_m"
colnames(zerop_means0)[4] <-  "zp_sd"
zerop_means0 <- na.omit(zerop_means0)

df_onepercentloss0 %>%
	group_by_at(vars(kmlclusters4, visit)) %>% 
	summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
	mutate_if(is.numeric, round, digits=1) %>% 
	as.data.frame() -> op_means0
colnames(op_means0)[17] <-  "op_m"
colnames(op_means0)[37] <-  "op_sd"
op_means0 <- na.omit(op_means0)

df_fivepercentloss0 %>%
	group_by_at(vars(kmlclusters4, visit)) %>% 
	summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
	mutate_if(is.numeric, round, digits=1) %>% 
	as.data.frame() -> fp_means0
colnames(fp_means0)[17] <-  "fp_m"
colnames(fp_means0)[37] <-  "fp_sd"
fp_means0 <- na.omit(fp_means0)

df_tenpercentloss0 %>%
	group_by_at(vars(kmlclusters4, visit)) %>% 
	summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
	mutate_if(is.numeric, round, digits=1) %>% 
	as.data.frame() -> tp_means0
colnames(tp_means0)[17] <-  "tp_m"
colnames(tp_means0)[37] <-  "tp_sd"
tp_means0 <- na.omit(tp_means0)

df_twentypercentloss0 %>%
	group_by_at(vars(kmlclusters4, visit)) %>% 
	summarise_if(is.numeric, c(mean, sd), na.rm=TRUE) %>% 
	mutate_if(is.numeric, round, digits=1) %>% 
	as.data.frame() -> twentyp_means0
colnames(twentyp_means0)[3] <-  "twentyp_m"
colnames(twentyp_means0)[4] <-  "twentyp_sd"
twentyp_means0 <- na.omit(twentyp_means0)

######## lm models and their residuals for loss of data #####
zeropmod0 = lm(zp_m ~ visit + kmlclusters4, data = zerop_means)
plot(zeropmod0)
mean(zeropmod0$residuals^2) # [1]  0.02424556

fpmod0 = lm(fp_m ~ visit + kmlclusters4, data = fp_means0)
plot(fpmod0)
mean(fpmod$residuals0^2) # [1] 0.005465976 (old) vs 0.005465976

tpmod0 = lm(tp_m ~ visit + kmlclusters4, data = tp_means0)
plot(tpmod0)
mean(tpmod$residuals^2) # [1] 0.004252959 vs 28.50422

twentypmod0 = lm(twentyp_m ~ visit + kmlclusters4, data = twentyp_means0)
plot(tpmod0)
mean(tpmod$residuals^2) # [1] 0.004252959 (old) vs 28.50422

######## MSE for percent loss data set #####

####### zeroer 0% loss #####
#inner_join(df_longdf, zerop_means, 
#           by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> zeroer
#zeroer$mse_zp <- ((zeroer$meanPDheel.y - zeroer$zp_m)^2) / dim(zeroer)[1]
#names(zeroer)

full_join(df_longdf, zerop_means0, 
          by = c("visit" = "visit", 
                 "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> zeroer0
zeroer0$mse_zp0 <- ((zeroer0$meanPDheel.x - zeroer0$zp_m)^2) / dim(zeroer)[1]
zeroer0$variable0 <- rep("0 %", dim(zeroer0)[1])
# gather zeroer
names(zeroer0)
zeroer0 <- zeroer0[c(46, 28, 47, 48, 49, 50, 51)]
names(zeroer0)
#zero0 <- zero0[,c( 46, 28, 38, 62, 82, 88, 89)]
# [1] "kmlclusters4" "visit"        "meanPDheel.y" "op_m"         "op_sd"       
# [6] "mse_op0"      "variable0"  
######## loner 1 % loss #####
full_join(df_onepercentloss0, op_means0, 
          by = c("visit" = "visit", "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> loner0
dim(loner0)
# str(loner0)
loner0$mse_op0 <- ((loner0$meanPDheel.y - loner0$op_m)^2) / dim(loner0)[1]
loner0$variable0 <- rep("1 %", dim(loner0)[1])
loner0 <- loner0[,c( 46, 28, 47, 67, 87, 88, 89)]
names(loner0)
######### fiver 5 % loss #####
full_join(df_fivepercentloss0, fp_means0, 
          by = c("visit" = "visit", 
                 "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> fiver0
dim(fiver0)
fiver0$mse_fp0 <- ((fiver0$meanPDheel.y - fiver0$fp_m)^2) / dim(fiver0)[1]
#View(fiver0)
fiver0$variable0 <- rep("5 %", dim(fiver0)[1])
names(fiver0)
fiver0 <- fiver0[,c( 46, 28, 47, 67, 87, 88, 89)]

######### tenner 10 % loss#####
full_join(df_tenpercentloss0, tp_means0, 
          by = c("visit" = "visit", 
                 "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> tenner0
dim(tenner0)
tenner0$mse_tp0 <- ((tenner0$meanPDheel.x - tenner0$tp_m)^2) / dim(tenner0)[1]
tenner0$variable0 <- rep("10 %", dim(tenner0)[1])
#View(tenner)
names(tenner0)
tenner0 <- tenner0[,c(46, 28, 47, 67, 87, 88, 89)]
names(tenner0)
#c("kml clusters", "visit", "score", 
# "mean per loss", "sd per loss", "mse per loss", "variable")

######### twentier 20 % loss #####

full_join(df_twentypercentloss0, twentyp_means0, 
          by = c("visit" = "visit", 
                 "kmlclusters4" = "kmlclusters4"), copy = FALSE) -> twentier0
dim(twentier0)
twentier0$mse_twenty0 <- 
  ((twentier0$meanPDheel.y - twentier0$twentyp_m)^2) / dim(twentier0)[1]
twentier0$variable0 <- rep("20 %", dim(twentier0)[1])
#View(twentier0)
names(twentier0)
twentier0 <- twentier0[,c( 46, 28, 47, 67, 87, 88, 89)]

######## rbind


# check
names(zeroer0)
names(loner0)
names(fiver0)
names(tenner0)
names(twentier0)


colnames(zeroer0) <- c("kml.clusters", 
                       "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(loner0) <- c("kml.clusters", 
                      "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(fiver0) <- c("kml.clusters", 
                      "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(tenner0) <- c("kml.clusters", 
                       "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

colnames(twentier0) <- c("kml.clusters", 
                         "visit", "score", "mean per loss", "sd per loss", "mse per loss", "variable")

names(zeroer0)
dim(zeroer0)
names(loner0)
dim(loner0)
names(fiver0)
names(tenner0)
names(twentier0)

er_long0 <- data.frame(rbind(zeroer0, loner0, fiver0, tenner0, twentier0))

names(er_long0)


er_long0$variable <- factor(er_long0$variable, 
                            levels = c("0 %", "1 %", "5 %", "10 %", "20 %", ordered = TRUE))
#er_long0 <- na.omit(er_long0)
save(er_long0, file = "../Data/er_long0.Rda")

er_long0 %>%      
  tabyl(variable, kml.clusters) %>% 
  as.data.frame() %>% 
  adorn_totals(where = "col")-> tab_loss0
tab_loss0
tab_loss0 <- tab_loss0[-6, ]
save(tab_loss0, file = "../Data/tab_loss0.Rda")

######## MSE plots for percent loss and no loss #####
# sample size
load("../Data/opkml4counts0.Rda")
load("../Data/fpkml4counts0.Rda")
load("../Data/tpkml4counts0.Rda")
load("../Data/twentypkml4counts0.Rda")

# full data, no loss here
fourkmlcounts$`Percentage loss` <- rep("0 %", dim(fourkmlcounts)[1])
opkml4counts0$`Percentage loss` <- rep("1 %", dim(opkml4counts0)[1])
fpkml4counts0$`Percentage loss` <- rep("5 %", dim(fpkml4counts0)[1])
tpkml4counts0$`Percentage loss` <- rep("10 %", dim(tpkml4counts0)[1])
twentypkml4counts0$`Percentage loss` <- rep("20 %", dim(twentypkml4counts0)[1])

ssize_kml0 <- rbind(fourkmlcounts, 
                    opkml4counts0, fpkml4counts0, tpkml4counts0, twentypkml4counts0)
ssize_kml0$`kml clusters` <- factor(ssize_kml0$`kml clusters`, levels = c("A", "B", "C", "D"), ordered = TRUE)
#ssize_kml$`kml clusters` <- sort(ssize_kml$`kml clusters`)
#ssize_kml$`Percentage loss` <- sort(ssize_kml$`Percentage loss`)
names(fourkmlcounts)
names(fpkml4counts0)
names(tpkml4counts0)
names(twentypkml4counts0)
save(ssize_kml0, file = "../Data/ssize_kml0.Rda")

a = paste( "0 %", " (", tab_loss0$A[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$A[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$A[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$A[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$A[5], ")", sep = "")
# Cluster A of 4kml with all losses
ggplot(er_long0[er_long0$kml.clusters == "A",]) +
  geom_smooth(aes(x = visit, y = mse.per.loss, 
                  group = variable, colour = variable), method = "loess", se = FALSE) +
  labs(colour = "% loss (n)", y = "mse") +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  expand_limits(x=c(1,13)) + 
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13))) 
#-> mseA

a = paste( "0 %", " (", tab_loss0$B[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$B[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$B[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$B[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$B[5], ")", sep = "")
ggplot(er_long0[er_long0$kml.clusters == "B",], aes(x = visit, 
                                                    y = mse.per.loss, 
                                                    group = variable, colour = variable)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = c(6, 9), colour = "red", 
             linetype = "dotted") + 
  scale_colour_discrete(name = "% loss (n)", labels = c(a,b,c,d, e)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  labs(y = "mse")
#-> mseB

# # Cluster C of 4kml with all losses
a = paste( "0 %", " (", tab_loss0$C[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$C[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$C[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$C[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$C[5], ")", sep = "")
ggplot(er_long0[er_long0$kml.clusters == "C",]) +
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
#-> mseC

# Cluster D of 4kml with all losses
a = paste( "0 %", " (", tab_loss0$D[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$D[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$D[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$D[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$D[5], ")", sep = "")
ggplot(er_long0[er_long0$kml.clusters == "D",]) +
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

df_onepercentloss0$loss <- rep("1 % loss", dim(df_onepercentloss0)[1])
#head(df_onepercentloss0)

# for data frame fp
df_fivepercentloss0$loss <- rep("5 % loss", dim(df_fivepercentloss0)[1])
#head(df_fivepercentloss0)

# for data frame tp
df_tenpercentloss0$loss <- rep("10 % loss", dim(df_tenpercentloss0)[1])
#head(df_tenpercentloss0)

# for data frame twentyp
df_twentypercentloss0$loss <- rep("20 % loss", dim(df_twentypercentloss0)[1])
#head(df_twentypercentloss0)


# long form 
loss_long0 <- data.frame(rbind(df_longdf, df_onepercentloss0, df_fivepercentloss0, 
                               df_tenpercentloss0, df_twentypercentloss0))


loss_long0$loss <- factor(loss_long0$loss, c("0 % loss", "1 % loss", 
																					 "5 % loss", "10 % loss",
																					 "20 % loss"), order = TRUE)

save(loss_long0, file = "../Data/loss_long0.Rda")

######## Score in Loss plots #####

# loss plot for kml cluster A
a = paste( "0 %", " (", tab_loss0$A[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$A[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$A[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$A[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$A[5], ")", sep = "")
ggplot(er_long0[er_long0$kml.clusters == "A",], aes(x = visit, y = score, 
                                                    group = variable, colour = variable)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") + 
  scale_colour_discrete(name = "% loss (n)", labels = c(a,b,c,d, e)) +
  theme(legend.position = "bottom") 

# loss plot for kml cluster B
a = paste( "0 %", " (", tab_loss0$B[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$B[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$B[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$B[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$B[5], ")", sep = "")
ggplot(er_long0[er_long0$kml.clusters == "B",], aes(x = visit, y = score, 
                                                    group = variable, colour = variable)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") + 
  scale_colour_discrete(name = "% loss (n)", labels = c(a,b,c,d, e)) +
  theme(legend.position = "bottom")


# loss plot for kml cluster C
a = paste( "0 %", " (", tab_loss0$C[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$C[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$C[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$C[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$C[5], ")", sep = "")
ggplot(er_long0[er_long0$kml.clusters == "C",], aes(x = visit, y = score, 
                                                    group = variable, colour = variable)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") + 
  scale_colour_discrete(name = "% loss (n)", labels = c(a,b,c,d, e)) +
  theme(legend.position = "bottom")

# loss plot for kml cluster D

a = paste( "0 %", " (", tab_loss0$D[1], ")", sep = "")
b = paste( "1 %", " (", tab_loss0$D[2], ")", sep = "")
c = paste( "5 %", " (", tab_loss0$D[3], ")", sep = "")
d = paste( "10 %", " (", tab_loss0$D[4], ")", sep = "")
e = paste( "20 %", " (", tab_loss0$D[5], ")", sep = "")
ggplot(er_long0[er_long0$kml.clusters == "D",], aes(x = visit, y = score, group = variable, 
                                                    colour = variable)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = c(6, 9), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom") +
  expand_limits(x=c(1,13), y=c(3.7, 4)) + 
  scale_colour_discrete(name = "% loss (n)", labels = c(a,b,c,d, e)) +
  scale_x_discrete(name = "visit", 
                   limits = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13)))

### end of loss plots and loss ###


