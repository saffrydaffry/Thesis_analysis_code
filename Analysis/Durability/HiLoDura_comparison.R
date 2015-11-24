### R Script to compare High Low Durability
## to zero infection rates
## at a probabilistic level 

# Connect to DB
require("RSQLite")
con <- dbConnect("SQLite", dbname="~/Documents/NematodeABM.db")


results = numeric()
#Empty data structure for storing stuff

## background nematode inhibition changes


vloads = c( 1, 2)
infrates = c( 0.2, 0.8)
virulences = c( 0.5, 1)
transmy = c(0.2, 0.8)
dura = c(0.2, 0.4, 0.6, 0.8)

maxticks = 1825
reportinterval = 4

######
## Random SQLite3 commands that can help
#####

ALTER TABLE HiLoDura_dat ADD COLUMN Filename TEXT;
INSERT INTO HiLoDura_dat (Filename) 
  SELECT rtrim(file, '.csv') FROM HiLoDura_dat;

CREATE INDEX parameters on HiLoDura_meta (Virulence, ViralLoad, InfectionRate, Transmissibility, Durability);
## Hard earned line of code:

template = "SELECT Nematodes FROM HiLoDura_dat 
            WHERE Tick = %d
            AND SUBSTR(file, 0, 37) 
            IN (
                SELECT Filename FROM HiLoDura_meta
                WHERE Virulence = %f
                AND ViralLoad = %f
                AND InfectionRate = %f
                AND Transmissibility = %f
                AND Durability = %f)"

for (vload in vloads) {
  for (infrate in infrates) {
    for (virulence in virulences) {
      for(transmy in transmy){
        for(dura in dura){
      
            tick = 0
      
            while (tick < maxticks) {
              ##
              print(tick)
              zeroinf = dbGetQuery(con, sprintf("SELECT Nematodes FROM zeroinf_dat WHERE Tick = %d", tick))
              treatment = dbGetQuery(con, sprintf(template, tick, virulence, vload, infrate, transmy, dura))
              
              kst <- ks.test(zeroinf$Nematodes, treatment$Nematodes)
              
              results <- rbind(results, c(vload, infrate, virulence, transmy, dura, tick, kst$statistic, kst$p.value))
              tick = tick + reportinterval
          }
        }
      }
    }
  }
}

colnames(results) <- c("ViralLoads", "InfectionRate", "Virulence", "Transmissibility", "Durability", "Tick", "D", "pvalue")
write.csv(results, "HiLowDura_compare.csv")