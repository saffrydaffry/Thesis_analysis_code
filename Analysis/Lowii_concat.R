###   Shortening dataset Lowii
## Variables that are missing from the data set are the Filename, Cysts, 
## Transmissibility, and Durability
## They all have constant values, Cysts = 10, Transmissibility = 0.5, Durability = 0.2

#db = "/Users/Safyre/Documents/NematodeABM.db" #from laptop
db = "/Users/Mac/Documents/NematodeABM.db" #pc
con <- dbConnect("SQLite", db)

##  Merge Datasets! (DONE SAVED FILE IN SANDRSN3 AS LOW_COMPLETE.CSV)
##  First add .csv to filenames in metadata to match real data
#  low_meta$Filename <- paste(low_meta$Filename, "", sep = ".csv")
#  low_new <-merge(low_meta, low_dat, by.x = "Filename", by.y = "file")

lowii <- dbGetQuery(con, "select * from LowII_complete")

##  Reduce Data because it's too hot to handle!!

# conduct a rolling mean to get NAs
#NematodeData <- ddply(lowii, .(year, Virulence, InfectionRate, ViralLoad), summarize,
 #                     Nematodecat=rollmean(Nematodes, 5, na.pad = TRUE))
lowii$year <-lowii$Tick/365
lowii$Tick <-NULL

# order the dataframe to match NematodeData
# then replace the Nematdoes column with new data from Nematodecat
lowii.sort <- lowii[order(lowii$year, lowii$Virulence, lowii$ViralLoad, lowii$InfectionRate),]              
#lowii.sort$Nematodes <- NematodeData$Nematodecat

# clear your RAM before next step
#gc()  #"garbage collection"

# Remove rows with NA values in Nematodes
#lowii.sort <- lowii.sort[complete.cases(lowii.sort$Nematodes), ]
#gc()

# still too much data, maybe grab chunks, do moving averages and append back together?
# average by a factor of 10

# too much too handle, so divey up by Virulence
lowii_0 <- subset(lowii.sort, Virulence == 0)
lowii_1 <- subset(lowii.sort, Virulence == 0.2)
lowii_2 <- subset(lowii.sort, Virulence == 0.8)
lowii_3 <- subset(lowii.sort, Virulence == 1)
lowii_4 <- subset(lowii.sort, Virulence == 1.2)

##  Repeat this set of code for each piece of lowii.sort
low4.sum <- ddply(lowii_4, .(year, InfectionRate, ViralLoad, Transmissibility), summarize,
                    Nematodes_u = mean(Nematodes),
                     Nematodes_sd = sd(Nematodes),
                     Nematodes_se = sd(Nematodes)/sqrt(length(Nematodes)),
                     
                     Soybean_u = mean(Soybean),
                     Soybean_sd = sd(Soybean),
                     Soybean_se = sd(Soybean)/sqrt(length(Soybean)),
                     
                     Virulence_u = mean(Virulence_mean),
                     Virulence_sd = sd(Virulence_mean),
                     Virulence_se = sd(Virulence_mean)/sqrt(length(Virulence_mean)),
                     
                     Transmissibility_u = mean(Transmissibility_mean),
                     Transmissibility_sd = sd(Transmissibility_mean),
                     Transmissibility_se = sd(Transmissibility_mean)/sqrt(length(Transmissibility_mean)),
                     
                     Durability_u = mean(Durability_mean),
                     Durability_sd = sd(Durability_mean),
                     Durability_se = sd(Durability_mean)/sqrt(length(Durability_mean)),
                     
                     Health_u = mean(Health_mean),
                     Health_sd = sd(Health_mean),
                     Health_se = sd(Health_mean)/sqrt(length(Health_mean)),
                     
                     Eggs.per.container_u = mean(Eggs_per_container_mean),
                     Eggs.per.container_sd = sd(Eggs_per_container_mean),
                     Eggs.per.container_se = sd(Eggs_per_container_mean)/sqrt(length(Eggs_per_container_mean)),
                     
                     VirusLoad_u = mean(Virus_Load),
                     VirusLoad_sd = sd(Virus_Load),
                     VirusLoad_se = sd(Virus_Load)/sqrt(length(Virus_Load)),
                     
                     UnhatchedJ2_u = mean(UnhatchedJ2),
                     UnhatchedJ2_sd = sd(UnhatchedJ2),
                     UnhatchedJ2_se = sd(UnhatchedJ2)/sqrt(length(UnhatchedJ2)),
                     
                     Dead_u = mean(Dead),
                     Dead_sd = sd(Dead),
                     Dead_se = sd(Dead)/sqrt(length(Dead)),
                     
                     Death.by.virus_u = mean(Death_by_virus),
                     Death.by.virus_sd = sd(Death_by_virus),
                     Death.by.virus_se = sd(Death_by_virus)/sqrt(length(Death_by_virus)),
                     
                     Fraction.Infected_u = mean(Fraction_Infected),
                     Fraction.Infected_sd = sd(Fraction_Infected),
                     Fraction.Infected_se = sd(Fraction_Infected)/sqrt(length(Fraction_Infected)),
                    
                     Cyst_u = mean(Cyst),
                     Cyst_sd = sd(Cyst),
                     
                     EggSac_u = mean(EggSac),
                     EggSac_sd =sd(EggSac),
                     
                     J1_u = mean(J1),
                     J1_sd = sd(J1),
                     
                     J2_u = mean(J2),
                     J2_sd =sd(J2),
                     
                     J3_u = mean(J3),
                     J3_sd = sd(J3),
                     
                     J4M_u = mean(J4M),
                     J4M_sd = sd(J4M),
                     J4F_u = mean(J4F),
                     J4F_sd = sd(J4F),
                     
                     Male_u = mean(M),
                     Male_sd = sd(M),
                     
                     Female_u = mean(F),
                     Female_sd = sd(F),
                     
                     Embryo_u = mean(Embryo),
                     Embryo_sd = sd(Embryo),
                     
                     F_Prime_u =mean(F_Prime),
                     F_Prime_sd =sd(F_Prime),
                    
                    Mating = mean(Mating),
                    Temp = mean(Temperature) 
)

# Remove NA's from each subset
#lowii.ma_1 <- lowii.ma_1[complete.cases(lowii.ma_1), ]
#lowii.ma_2 <- lowii.ma_2[complete.cases(lowii.ma_2), ]
#lowii.ma_3 <- lowii.ma_3[complete.cases(lowii.ma_3), ]
#lowii.ma_4 <- lowii.ma_4[complete.cases(lowii.ma_4), ]

# extract randomized subsets from section
#lowii.ma_11 <- lowii.ma_1[sample(1:nrow(lowii.ma_1), 47000, replace = FALSE), ]
#lowii.ma_21 <- lowii.ma_2[sample(1:nrow(lowii.ma_2), 47000, replace = FALSE), ]
#lowii.ma_31 <- lowii.ma_3[sample(1:nrow(lowii.ma_3), 47000, replace = FALSE), ]
#lowii.ma_41 <- lowii.ma_4[sample(1:nrow(lowii.ma_4), 47000, replace = FALSE), ]

rm(lowii.ma_1, lowii.ma_2, lowii.ma_3, lowii.ma_4)

# put back Virulence values
low0.sum$Virulence <- rep(0, nrow(low0.sum))
low1.sum$Virulence <- rep(0.2, nrow(low1.sum))
low2.sum$Virulence <- rep(0.8, nrow(low2.sum))
low3.sum$Virulence <- rep(1, nrow(low3.sum))
low4.sum$Virulence <- rep(1.2, nrow(low4.sum))

# put it all together
low_01 <- rbind(low0.sum, low1.sum)
low_012 <- rbind(low_01, low2.sum)
low_34 <- rbind(low3.sum, low4.sum)

rm(lowii.sort, lowii_0, lowii_1, lowii_2, lowii_3, lowii_4, lowii)
gc()

# Fingers crossed
lowii_new <- rbind(low_012, low_34)
write.csv(lowii_new, "lowii_complete_cat.csv")

#  Written...
dbWriteTable(con, "LowII_complete", lowii_new, overwrite = TRUE)

