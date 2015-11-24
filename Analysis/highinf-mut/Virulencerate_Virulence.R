### code for plotting rate of virulence over virulence

#db = "/Users/Safyre/Documents/NematodeABM.db" #from laptop
db = "/Users/Mac/Documents/NematodeABM.db" #pc
con <- dbConnect("SQLite", db)

##  Merge Datasets! (DONE SAVED FILE IN SANDRSN3 AS LOW_COMPLETE.CSV)
##  First add .csv to filenames in metadata to match real data
#  low_meta$Filename <- paste(low_meta$Filename, "", sep = ".csv")
#  low_new <-merge(low_meta, low_dat, by.x = "Filename", by.y = "file")

lowii <- dbGetQuery(con, "select * from LowII_complete")
#  or
lowii <-read.csv("lowii_complete_cat.csv", header = TRUE)


slope.calc <- function(x,y){
  n = length(x)
  diffy = rep(0,n-1)
  diffx = rep(0, n-1)
  for(i in 1:(n-1)){
    y1 = y[i]
    y2 = y[i+1]
    x1 = x[i]
    x2 = x[i+1]
    diffy[i] = y2-y1
    diffx[i] = x2- x1
    
  }
  result = diffy/diffx
  result[n] =NA
  return(result)
}

## Subset relevant data
Virrate_sub <-subset(lowii, select = c("year", "ViralLoad", "InfectionRate", "Virulence", "Virulence_u", "Virulence_sd"))
Vir_rate_over_time <- ddply(Virrate_sub, .(Virulence, InfectionRate, ViralLoad), transform,
                            Virulence_u = Virulence_u,
                            VirRate = slope.calc(year, Virulence_u))
##  Remove NAs                            
Vir_rate_over_time <- Vir_rate_over_time[complete.cases(Vir_rate_over_time), ]

## Plot Rate of Virulence 
###############################
#   over time 
ggplot(Vir_rate_over_time) +
  geom_boxplot(aes(x = year, y = VirRate, group = round(year, digits = 1)), outlier.shape = NA) +
  facet_wrap(~Virulence)

#   and over Virulence
ggplot(Vir_rate_over_time) +
  #geom_boxplot(aes(x = Virulence_u, y = VirRate, group = round(Virulence_u, digits = 1)), outlier.shape = NA) +
  geom_point(aes( x = Virulence_u, y = VirRate), alpha = 0.5) +
  geom_smooth(aes( x = Virulence_u, y = VirRate), method = "lm") +
  #scale_x_continuous( limits = c(0.01, 12.5)) +
  #scale_x_log10() +
  #scale_y_log10() +
  ylab("Rate of Virulence") +
  xlab("Mean Virulance") +
  theme(axis.title = element_text(family = "sans", face = "bold")) +
  facet_wrap(~InfectionRate, scales = "free_x")

# histogram
ggplot(Vir_rate_over_time) +
  geom_histogram(aes(x =VirRate, y = ..count..))
