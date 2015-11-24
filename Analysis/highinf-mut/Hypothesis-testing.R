## Hypothesis testing by crop years
library("RSQLite")
library("DBI")

## Read in Data
#db="/Users/sandrsn3/Documents/NematodeABM.db" # from mac
db = "/Users/Mac/Documents/NematodeABM.db" #from pc
#db = "/Users/Safyre/Documents/NematodeABM.db" #from laptop
con <- dbConnect("SQLite", db)

dat <- dbGetQuery(con, "select * from High_complete_summary")

# OR 

dat <- read.table("", header = TRUE, sep = ",")

## Kruskal-Wallis and Mann-Whitney-Wilcox
## Do varying Infection Rates parameters affect Nematode populations significantly?

#  create a subset to append control data
Low.Nem.sample <-subset(lowii, select = c("Nematodes_u", "InfectionRate", "year"))
#Zero.Nem.sample <- subset(zeroii, year <= 5, select = c("Nematodes_u", "InfectionRate", "year"))

#  Combine (50614 rows)
#  for some reason can't start with the bigger dataset
Nem.Inf.sample <-  Low.Nem.sample

#  remove NAs (0 rows removed)
#  InfectionRate column is currently type character.  Change to Numeric.
Nem.Inf.sample$InfectionRate <- as.numeric(Nem.Inf.sample$InfectionRate)

#Nem.Inf.sample$year<- round(Nem.Inf.sample$year, digits = 1)

#  Kruskal-Wallis test against InfectionRate treatments
kruskal.test(Nematodes_u~InfectionRate, data = Nem.Inf.sample)
##
wilcox.test(Nem0$Nematodes_u, Nem4$Nematodes_u, alternative = "greater")
wilcox.test(Nem0$Nematodes_u, Nem4$Nematodes_u, alternative = "less")

## Wilcox Matrix
Vir_pstat_false <- matrix(rep(0), nrow = 5, ncol = 5)
IR <- c(0, 0.01,0.1,0.2,0.5)

for(i in 1:5){
  for(j in 1:5){
    if(i ==1  && j !=1){
      V2 <- subset(VirxInf_lm$Vir_u_clone, VirxInf_lm$InfectionRate == IR[j])
      Control <- rep(0, length(V2))
      Vir_pstat_false[i,j] = wilcox.test(V2,Control)$p.value #paired is false
      rm(V2, Control) 
    }else if(i==1 && j==1){
      Control1 <- rep(0, 12000)
      Control2 <- rep(0, 12000)
      Vir_pstat_false[i,j] = wilcox.test(Control1,Control2)$p.value #paired is false
      rm(Control1,Control2)   
      
    }else if(i !=1 && j ==1){
      V1 <- subset(VirxInf_lm$Vir_u_clone, VirxInf_lm$InfectionRate == IR[i])
      Control <- rep(0, length(V1))
      Vir_pstat_false[i,j] = wilcox.test(V1,Control)$p.value #paired is false
      rm(V1,Control)    
    } else {  
      V1 <- subset(VirxInf_lm$Vir_u_clone,  VirxInf_lm$InfectionRate == IR[i])
      V2 <- subset(VirxInf_lm$Vir_u_clone, VirxInf_lm$InfectionRate == IR[j])
      Vir_pstat_false[i,j] = wilcox.test(V1,V2)$p.value #paired is false
      rm(V1,V2)
    } 
  }
}

#  Label the rows and columns 
rownames(Vir_pstat_false) <- c("0","0.01", "0.1", "0.2", "0.5")
colnames(Vir_pstat_false) <- c( "0","0.01", "0.1", "0.2", "0.5")