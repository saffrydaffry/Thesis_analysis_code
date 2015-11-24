###  Use ggplot to display D statistic over time
###  Compare distributions

##  Connect to DB
require("RSQLite")
#db="Users/sandrsn3/Documents/NematodeABM.db"
db = "C:/Users/Safyre/Documents/NematodeABM.db"
con <- dbConnect("SQLite", db)

##  extract db table into data fram
zero = dbGetQuery(con, "select * from zeroinf_complete")
lowii = dbGetQuery(con, "select * from LowII_complete")

##  select only relavent columns
zero_sub = subset(zero,select=c(Tick,Nematodes))
lowii_sub = subset(lowii, select = c(Tick, Nematodes))

##  create year column
zeroinf_sub$year = zeroinf_sub$Tick/365;
lowii_sub$year = lowii_sub$Tick/365;

## Append datasets
compare <- cbind(lowii_sub, zeroinf_sub)

## add noise to create second distribution
# replace with actual comparision data
# runif ->random numbers between 1 and 0 for 50 obs
#zeroinf_sub$NematodesNoise = zeroinf_sub$Nematodes + 1000*(rnorm(nrow(zeroinf_sub))) 


##  calculate KS test over nematode populations vs 2nd noisy nematode population
# grouped by Tick number
# Plyr package good for breaking large dimensional matrices into small workable pieces; allows ddply
require('plyr')  
Ddata = ddply(zeroinf_sub, .(year), summarize,
             samples=length(Nematodes),  #this part not really needed, just to check subset size
             mean=mean(Nematodes),       #column for Nematodes mean
             D=ks.test(Nematodes,NematodesNoise)$statistic, 
             p_D = ks.test(Nematodes, NematodesNoise)$p.value)  # pulls out p value  from Nematodes set

Ddata$smoothpD = filter(Ddata$p_D, c(1,1,1,1,1)/5)
# plot p vs Tick
require('ggplot2')
# ggplot with geometric points, can also do boxplot with geom_boxplot()
# aes has to match actual column titles
ggplot(data = Ddata, aes(x=year,y=smoothpD)) + geom_line()
# Code will give 50 benign warnings--one for each trial (50 obs per Tick).  
# These come from ks.test which warns that it can't compute p-values where ties occur
   