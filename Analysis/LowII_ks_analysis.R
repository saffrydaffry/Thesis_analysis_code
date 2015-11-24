####  Plot results for Kolmogrov-Smirnov test that was done for 
####  low incidence infection data and zero infection data
####  D value compares the Nematode populations between low and control

library(ggplot2)
library(plyr)
library(reshape2)
library(zoo)
##  Set working directory to avoid confusion
setwd("/Users/Safyre/Dropbox/SCN-Thesis/Analysis")

##  Extract the data from .csv file
lowks <- read.csv("lowinf_zeroinf/Low_incidence_kstest.csv",
                  header = TRUE, sep=",")
lowks$year <- lowks$Days/365

######################
##  Create Subsets  ##
######################

##  Make a subset for the self evaluation
#  New dataframe with self-inflicted KS-test
db = "/Users/Safyre/Documents/NematodeABM.db"
con <- dbConnect(dbDriver("SQLite"), db)
lowii <- dbGetQuery(con, "select * from LowII_complete")

#  Subset data
low_vars <- c("Nematodes", "InfectionRate", "Virulence", "ViralLoad", "year")
low_new <-(lowii[low_vars])


Ddata = ddply(low_new, .(year, Virulence, InfectionRate, ViralLoad), summarize,
              selfD=ks.test(Nematodes[sample(1:NROW(Nematodes), NROW(Nematodes)/2, replace = FALSE)], 
                            Nematodes[sample(1:NROW(Nematodes), NROW(Nematodes)/2, replace = FALSE)])$statistic 
              )

##   Virulence:
Vir_sub = data.frame(cbind(lowks$Virulence, lowks$year, lowks$D, lowks$P.Value))
colnames(Vir_sub) <- c("Virulence", "year","D","P.Value")

# With weighted average
VirulenceData = ddply(Vir_sub, "Virulence", transform,
                      smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

#   Infection Rate:
Inf_sub = data.frame(cbind(lowks$Infection.Rate, lowks$year, lowks$D, lowks$P.Value))
colnames(Inf_sub) <- c("Infection.Rate", "year","D","P.Value")

##  With weighted average
Inf_RateData = ddply(Inf_sub, "Infection.Rate", transform,
                      smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

#  Viral Loads:
Viral_sub = data.frame(cbind(lowks$Viral.Load, lowks$year, lowks$D, lowks$P.Value))
colnames(Viral_sub) <- c("Viral.Load", "year","D","P.Value")

##  With weighted average
Viral_LoadData = ddply(Viral_sub, "Viral.Load", transform,
                     smoothD = rollmean(D, 5, align = "right",na.pad = TRUE))

  

###########################
## Plot them and Analyze ##
###########################
## Create ggplot objects

# works!! :D
Virulence.plot <- ggplot() + 
                  geom_boxplot(data = VirulenceData, aes(x = year, y = smoothD,
                               fill = Virulence, group = round(year, digits = 1))) + 
                  geom_smooth(data = Ddata, aes(x = year, y = selfD))

Inf_Rate.plot <- ggplot() + 
                 geom_boxplot(data = Inf_RateData, aes(x = year, y = smoothD,
                              fill = Infection.Rate, group = round(year, digits = 1))) + 
                  geom_smooth(data = Ddata, aes(x = year, y = selfD))

Viral_Load.plot <- ggplot() + 
                   geom_boxplot(data = Viral_LoadData, aes(x = year, y = smoothD,
                                fill = Viral.Loads, group = round(year, digits = 1))) + 
                   geom_smooth(data = Ddata, aes(x = year, y = selfD))
##  Customize and Plot
Virulence.plot  +
  facet_wrap(~Virulence) + 
  ggtitle("Kolmogrov Smirnov over time by Virulences") 

  

Inf_Rate.plot + facet_wrap(~Infection.Rate) + ggtitle("Kolmogrov Smirnov over time by Infection Rates")

Viral_Load.plot + facet_wrap(~Viral.Load) + ggtitle("Kolmogrov Smirnov over time by Viral Loads")


