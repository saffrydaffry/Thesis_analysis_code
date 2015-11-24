## Time Scale Plots

## Preamble
library("RSQLite")
library("DBI")
library("ggplot2")  # for fancy plots
library(ggsubplot)  # for big data
library("grid")     # for hexbin
library("hexbin")   # for hexagonnal binning
library("lattice")  # for fast plots
library("zoo")
library("plyr")
library("reshape2") # reorganize dataframes easily
## end Preamble

## Load Data
#db = "/Users/Safyre/Documents/NematodeABM.db" #from laptop
db = "/Users/Mac/Documents/NematodeABM.db" #pc
con <- dbConnect("SQLite", db)
lowii <- dbGetQuery(con, "select * from LowII_complete")
#  or
lowii <-read.csv("lowii_complete_cat.csv", header = TRUE)

## Split by Crop year
germinatedate = 112/ 365
harvestdate = 240/365

lowii_yr1 <- subset(lowii, year<= harvestdate & year >= germinatedate ,select = c("year", "Nematodes_u", "Virulence", "InfectionRate") )
lowii_yr1$CropYear <- rep(1, length(lowii_yr1$year))
lowii_yr2 <- subset(lowii, year<= (harvestdate+1) & year >=(germinatedate+1), select = c("year", "Nematodes_u", "Virulence", "InfectionRate") )
lowii_yr2$CropYear <- rep(2, length(lowii_yr2$year))
lowii_yr3 <- subset(lowii, year<= (harvestdate+2) & year >=(germinatedate+2), select = c("year", "Nematodes_u", "Virulence", "InfectionRate") )
lowii_yr3$CropYear <- rep(3, length(lowii_yr3$year))
lowii_yr4 <- subset(lowii, year<= (harvestdate+3) & year >=(germinatedate+3), select = c("year", "Nematodes_u", "Virulence", "InfectionRate") )
lowii_yr4$CropYear <- rep(4, length(lowii_yr4$year))
lowii_yr5 <- subset(lowii, year<= (harvestdate+4) & year >=(germinatedate+4), select = c("year", "Nematodes_u", "Virulence", "InfectionRate") )
lowii_yr5$CropYear <- rep(5, length(lowii_yr5$year))

##  Function to paste regression line in plot
lm_eqn = function(VirxInf_lm){
  m = lm(log_Vir ~ year, VirxInf_lm);
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*"," ~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*"," ~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

#  Generates regression equations for each subset of data
#  Also determines the coordinates for the placement of the equation on the plot
regs <- ddply(VirxInf_lm, .(InfectionRate), lm_eqn)
regs.xpos <- ddply(VirxInf_lm, .(InfectionRate),
                   function(VirxInf_lm) (min(VirxInf_lm$year)+max(VirxInf_lm$year))/2)
regs.ypos <- ddply(VirxInf_lm, .(InfectionRate), 
                   function(VirxInf_lm) min(VirxInf_lm$log_Vir) + 0.05*(max(VirxInf_lm$log_Vir)-min(VirxInf_lm$log_Vir)))

regs$y <- regs.ypos$V1  #  V1 is the default column name
regs$x <- regs.xpos$V1


## Apply to plot of Vir_time:
Vir_time <- ggplot( data = VirxInf_lm, aes( x = year, y = log_Vir, fill = InfectionRate)) +
  geom_boxplot(aes(group= round(year, digits = 1))) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y~x) +
  geom_text(data=regs, size=5, color="black", 
            aes(x=x, y=y, label=V1),  
            parse=TRUE)  +
  facet_wrap(~InfectionRate) +
  ggtitle(" Virulence over time") +
  ylab("log transform of Transient Virulence") +
  scale_fill_discrete(name = "Infection Rates") 
