# Use ggplot to display D statistic over time
# Compare distributions

# Connect to DB
require("RSQLite")
db="/Users/sandrsn3/Documents/NematodeABM.db"
con <- dbConnect("SQLite", db)

# extract db table into data frame
zeroinf = dbGetQuery(con, "select * from zeroinf_dat")

# select only relavent columns
zeroinf_sub = subset(zeroinf,select=c(Tick,Nematodes))

# create year column
zeroinf_sub$year = zeroinf_sub$Tick/365

# add noise to create second distribution
# replace with actual comparision data
# runif ->random numbers between 1 and 0 for 50 obs
zeroinf_sub$NematodesNoise = zeroinf_sub$Nematodes + 1000*(runif(length(zeroinf_sub))) 


# calculate KS test over nematode populations vs 2nd noisy nematode population
# grouped by Tick number
# Plyr package good for breaking large dimensional matrices into small workable pieces; allows ddply
library('plyr')  
Ddata = ddply(zeroinf_sub, .(year), summarize,
             samples=length(Nematodes),  #this part not really needed, just to check subset size
             mean=mean(Nematodes),       #column for Nematodes mean
             D=ks.test(Nematodes,NematodesNoise)$statistic)  # pulls out D statistic from Nematodes set

# plot D vs Tick
library('ggplot2')
# ggplot with geometric points, can also do boxplot with geom_boxplot()
# aes has to match actual column titles
ggplot(data = Ddata, aes(x=year,y=D)) + geom_point()

# Code will give 50 benign warnings--one for each trial (50 obs per Tick).  
# These come from ks.test which warns that it can't compute p-values where ties occur
   