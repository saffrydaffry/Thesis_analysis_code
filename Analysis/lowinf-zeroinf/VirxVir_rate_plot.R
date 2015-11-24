## Virulence Rate over Virulence
##################################
library("ggplot2")
library("plyr")
## function to find slope
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
Virrate_sub <-subset(lowii_new, select = c("year", "ViralLoad", "InfectionRate", "Virulence", "Virulence_u", "Virulence_sd", "VirusLoad_u"))
Virrate_sub <- ddply(Virrate_sub, .(Virulence, InfectionRate, ViralLoad), transform,
                            Virulence_u = Virulence_u/VirusLoad_u,   ## Normalize
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

#  Line Equation
regs <- ddply(Vir_rate_over_time, .(InfectionRate), lm_eqn)
regs.xpos <- ddply(VirxInf_lm, .(InfectionRate),
                   function(VirxInf_lm) (min(VirxInf_lm$year)+max(VirxInf_lm$year))/2)
regs.ypos <- ddply(VirxInf_lm, .(InfectionRate), 
                   function(VirxInf_lm) min(VirxInf_lm$log_Vir) + 0.05*(max(VirxInf_lm$log_Vir)-min(VirxInf_lm$log_Vir)))

regs$y <- regs.ypos$V1  #  V1 is the default column name
regs$x <- regs.xpos$V1

#  Over InfectionRate
ggplot(Vir_rate_over_time) +
  #geom_boxplot(aes(x = Virulence_u, y = VirRate, group = round(Virulence_u, digits = 1)), outlier.shape = NA) +
  geom_point(aes( x = Virulence_u, y = VirRate), alpha = 0.5) +
  geom_smooth(aes( x = Virulence_u, y = VirRate),method = "lm", se = FALSE, color = "black") +
  ylab("Rate of Virulence") +
  xlab("Mean Virulance") +
  theme(axis.title = element_text(family = "sans", face = "bold")) + 
  #scale_fill_discrete(name = "Infection Rate") 
  scale_x_log10() +
  scale_y_log10() +
  geom_text(data=regs, size=5, color="black", 
         aes(x=x, y=y, label=V1),  
        parse=TRUE)  +
 facet_wrap(~InfectionRate, scales = "free_x")