####  Plot results for Kolmogrov-Smirnov test that was done for 
####  low incidence infection data and zero infection data
####  D value compares the Nematode populations between low and control

library(ggplot2)
library(plyr)
library(reshape2)
library(zoo)
##  Set working directory to avoid confusion

##  Extract the data from .csv file
dura <- read.csv("HiLowDura_compare.csv",
                  header = TRUE, sep=",")
dura$year <- dura$Tick/365


######################
##  Create Subsets  ##
######################
#   Virulence:
Vir_sub = data.frame(cbind(dura$Virulence, dura$year, dura$D, dura$pvalue))
colnames(Vir_sub) <- c("Virulence", "year","D","pvalue")

##  With weighted average
VirulenceData = ddply(Vir_sub, "Virulence", transform,
                      smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

#   Infection Rate:
Inf_sub = data.frame(cbind(dura$Infection.Rate, dura$year, dura$D, dura$pvalue))
colnames(Inf_sub) <- c("Infection.Rate", "year","D","pvalue")

##  With weighted average
Inf_RateData = ddply(Inf_sub, "Infection.Rate", transform,
                      smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

#  Viral Loads:
Viral_sub = data.frame(cbind(dura$Viral.Load, dura$year, dura$D, dura$pvalue))
colnames(Viral_sub) <- c("Viral.Load", "year","D","pvalue")

##  With weighted average
Viral_LoadData = ddply(Viral_sub, "Viral.Load", transform,
                     smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

#  Durability:
Dura_sub = data.frame(cbind(dura$Durability, dura$year, dura$D, dura$pvalue))
colnames(Dura_sub) <- c("Durability", "year","D","pvalue")

##  With weighted average
DuraData = ddply(Dura_sub, "Durability", transform,
                       smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

#  Transmissibility:
Trans_sub = data.frame(cbind(dura$Transmissibility, dura$year, dura$D, dura$pvalue))
colnames(Trans_sub) <- c("Transmissibility", "year","D","pvalue")

##  With weighted average
TransData = ddply(Trans_sub, "Transmissibility", transform,
                       smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

###########################
## Plot them and Analyze ##
###########################
## Create ggplot objects
Virulence.plot <- qplot(year, smoothD, data = VirulenceData, 
                        fill = Virulence, 
                        geom = "boxplot", 
                        group = round(year, digits =1))

Inf_Rate.plot <- qplot(year, smoothD, data = Inf_RateData, 
                        fill = Infection.Rate, 
                        geom = "boxplot", 
                        group = round(year, digits =1))

Viral_Load.plot <- qplot(year, smoothD, data = Viral_LoadData, 
                       fill = Viral.Load, 
                       geom = "boxplot", 
                       group = round(year, digits =1))

Durability.plot <- ggplot( DuraData, aes(x = year, y = D, 
                                         fill = Durability, 
                                         group = round(year, digits = 1))) + 
                                        geom_boxplot()

Transmissibility.plot <- ggplot(TransData, aes(x = year, y = smoothD, 
                                              fill = Transmissibility, 
                                              group = round(year, digits = 1))) + 
                                              geom_boxplot()

##  Customize and Plot
Virulence.plot + facet_wrap(~Virulence) + ggtitle("Kolmogrov Smirnov over time by Virulences")

Inf_Rate.plot + facet_wrap(~Infection.Rate) + ggtitle("Kolmogrov Smirnov over time by Infection Rates")

Viral_Load.plot + facet_wrap(~Viral.Load) + ggtitle("Kolmogrov Smirnov over time by Viral Loads")

Durability.plot + facet_wrap(~Durability) + ggtitle("D stat over time across Durabilities")

Transmissibility.plot + facet_wrap(~Transmissibility) + ggtitle("D stat over time across Transmissibility")
#######################
## Hypthesis Testing ##
#######################

