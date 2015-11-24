### PCA using small dat
## Helper functions at the bottom
## smalldat is simply the means in relation to mut_rate, Virulence, InfectionRate, and yr
## has 420 rows total
library(ggplot2)
library(FactoMineR)
library(ggbiplot)
library(reshape2)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(plyr)

smalldat<-na.omit(smalldat)
smalldat$Temperature <-NULL

## focus only on first and last year, otherwise too many factors to deal with!
smalldat2 <- subset(smalldat, yr ==0 | yr==4)
pcdat2 <-smalldat2[,5:21]
pr.cp2 <- prcomp(formula = ~., data = pcdat2, scale = TRUE,  na.action =na.exclude)

sub1 = subset(summary_bigdat2, InfectionRate==0.8, select = c("mut_rate", "Virulence", "yr", "mortality_rate", "Virulence_mean", "Transmissibility_mean", "Virus_Load", "years"))
subR = subset(Rep_ratios, InfectionRate==0.8, select = c("yr", "mut_rate", "Virulence", "R0_new"))
sub = cbind(sub1, R0=subR$R0_new)


# function #dividing by virus_load doesn't change anything
pca_normalize = function(df){
  #score
  tv_score <- prcomp(formula = ~ (Transmissibility_mean) + (Virulence_mean), center = FALSE, scale = TRUE, data = df)$x[,1]
  rm_score <- prcomp(formula = ~ (R0) + (mortality_rate), center=FALSE,scale = FALSE, data=df)$x[,1]
  ##loadings
    t_load <- prcomp(formula = ~ (Transmissibility_mean) + (Virulence_mean), scale = TRUE, data=df)$rotation[1,1]
    v_load <- prcomp(formula = ~ (Transmissibility_mean) + (Virulence_mean), scale = TRUE, data=df)$rotation[2,1]
   # #eigens
   tv_eigen <-prcomp(formula = ~ (Transmissibility_mean) + (Virulence_mean), scale = TRUE, data=df)$sdev[1]
   rm_eigen <- prcomp(formula = ~ (R0) + (mortality_rate),scale = FALSE, data=df)$sdev[1]
  #database
  data.frame(years = df$years, tv_score, rm_score, mortality_rate = df$mortality_rate, Virulence_mean=df$Virulence_mean, Transmissibility_mean = df$Transmissibility_mean, R0 = df$R0, Virus_Load = df$Virus_Load,t_load , v_load)#, #, tv_eigen=rep(tv_eigen), rm_eigen=rep(rm_eigen))
}

pca_tv_skew_scale =ddply(sub, .(mut_rate, Virulence, yr), pca_normalize)
ALSEpca_tv_skew = pca_tv #center = FALSE
pca_tv_skew_scale #center = FALSE, for tv, SCALE = true

ggplot(subset(sub_dev, yr!=0), aes(x = years, y = Virulence_mean))+
  geom_point(aes(color = years, size = factor(yr)))+
  geom_boxplot(aes(fill = factor(yr)))+
  geom_vline(x = 0, color = "grey")+
  scale_size_manual(values = c(3.5,2.9,2.4,1.9,1.5))+
  scale_color_gradient2(midpoint = 2.3, mid = "green", low = "blue",  high = "red")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

ggplot(pca_tv, aes(x =rm_score, y  = tv_score))+geom_point(aes(color = mortality_rate*100))+
  geom_smooth()+
  geom_hline(y = 0, color = "grey")+
  scale_color_gradient2(midpoint = 6, mid = "green", low = "blue",  high = "red")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)


#scores
ggplot(subset(pca_tv_skew, years >2),aes(x =rm_score, y  = tv_score))+
  geom_point(shape = 21, aes(fill = years, color = years, size = factor(yr))) +
  #geom_smooth()+
  geom_point(color = "black",shape = 2, aes(x = rm_score, y = t_load))+
  geom_point(color = "grey", shape = 2, aes(x = rm_score, y = v_load))+
  scale_size_manual(values = c(3.5,2.9,2.4,1.9,1.5))+
  scale_fill_gradient(low="navy",high = "goldenrod1")+
  scale_color_gradient(low="purple4", high = "gold2")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

ggplot(subset(pca_tv, yr >2),aes(x =R0, y  = tv_score))+
  geom_point( alpha = 0.5, aes(color = years, size = factor(yr))) +
  geom_point(data = subset(pca_tv_skew, years >2), color = "black",shape = 2, aes(x = R0, y = t_load))+
  geom_point(data=subset(pca_tv_skew, years >2), color = "grey", shape = 2, aes(x = R0, y = v_load))+
  scale_size_manual(values = c(2,3))+
  scale_color_gradient(high="navy",low = "goldenrod1")+
  #scale_color_gradient(low="purple4", high = "gold2")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

start = ddply(pca_tv, .(mut_rate, Virulence, yr), summarize,
              tv_0 = tv_score[which.min(years)],
              rm_0 = rm_score[which.min(years)])


tvrm.cp = ddply()



######################################
mut_rate2 = factor(smalldat2$mut_rate, levels=unique(smalldat2$mut_rate), ordered=TRUE)
Virulence2 = factor(smalldat2$Virulence, levels = unique(smalldat2$Virulence), ordered = TRUE)
InfectionRate2 = factor(smalldat2$InfectionRate, levels = unique(smalldat2$InfectionRate), ordered = TRUE)
treatment2 = factor(smalldat2$yr, levels=unique(smalldat2$yr), ordered=TRUE)

scores2 <- data.frame(mut_rate = mut_rate2, Virulence = Virulence2, InfectionRate = InfectionRate2, treatment = treatment2, pr.cp2$x[,1:7])
#smalldat2 <- na.omit(smalldat2)

###  Princ Comp on all variables except the four we subsetted earlier
## Also remove the last 3 variables, Sick, HiV, HiT because 
# those are definitely linearly dependent on other variables
pcdat <-smalldat[,5:21]
pr.cp <- prcomp(formula = ~., data = pcdat, scale = TRUE,  na.action =na.exclude)
screeplot(pr.cp, type ="lines") # 4-5 pcs

## Get a glimpse of PCs
summary(pr.cp)
n = nrow(pcdat)  # total number of treatments
treatment = rep(0, n)
p = ncol(pr.cp$x)    # total number of components
treatment = factor(smalldat$yr, levels=unique(smalldat$yr), ordered=TRUE)

# choose which factors to identify in components
#treatment[which(smalldat2$yr==0)] <- 0
#treatment[which(smalldat2$yr==4)] <- 4
mut_rate = factor(smalldat$mut_rate, levels=unique(smalldat$mut_rate), ordered=TRUE)
Virulence = factor(smalldat$Virulence, levels = unique(smalldat$Virulence), ordered = TRUE)
InfectionRate = factor(smalldat$InfectionRate, levels = unique(smalldat$InfectionRate), ordered = TRUE)
#treatment = data.frame(year = treatment, Mut_rate = mut_rate)

### Eigenvalues of Correlation matrix
sum_eigen <- sum(pr.cp2$sdev^2)
Percent.Var = (pr.cp2$sdev^2)/sum_eigen #leave percent out for excel
Cum.percent = cumsum(Percent.Var)
Correlation <- data.frame(seq(1:p), pr.cp2$sdev^2, Percent.Var,Cum.percent )
colnames(Correlation) <- c("Components", "Eigenvalues", "Percent of Variance", "Cumulative Percent")
write.csv(Correlation, "cor_table_pca.csv", row.names = FALSE)

### Data frames for plotting
scores <- data.frame(mut_rate, Virulence, InfectionRate, treatment, pr.cp$x[,1:5])
#scores$treatment <- factor(scores$treatment)
write.csv (scores, "pca_scores.csv", row.names = FALSE)
#loadings
write.csv (pr.cp$rotation[,1:5], "pca_loading.csv")

ggplot(data = scores) + geom_density(aes(x = PC2, fill = factor(Virulence)), alpha = 0.8) + facet_grid(.~mut_rate)
## index of loadings
IL = pr.cp$rotation[,1:8]
for(i in 2: ncol(IL)){
  for(j in nrow(IL)){
    IL[j,i]=pr.cp$rotation[j,i]^2*Correlation$Eigenvalues[i]^2
  }
}

## data reduction, significance of loadings
#try pc1: Dead, j1, j2, j4f, j4m, m
#map1 = c(0, 0 ,0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0) #results in r =0.55, not good!
# map1 = c(0, 0 ,0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0)  # add J3 and F, only 0.001 improvement
#map1 = c(0, 0 ,0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0) # add dead,  r = 0.837, not bad
#map1 = c(0, 0 ,0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0) # add nematodes, r = 0.9999!
map1 = c(0, 0 ,0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0) # remove J3 and F, still r = 0.9999
pc1 = pr.cp$rotation[,1]
pc1_sv = map1*pc1
V = matrix(c(pc1, pc1_sv), ncol=2)
S = cov(smalldat[,5:21])

PC_Cov = t(V)%*%S%*%V
r1 = PC_Cov[1,2]/sqrt(PC_Cov[1,1]*PC_Cov[2,2])

## pc2: trans, vir, vL, Fi
map2 = c(1, 1 ,1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)  # r2 = 0.135, yikes!
map2 = c(1, 1 ,1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)  # add DBV, r2 = -0.106....
map2 = c(1, 1 ,1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)   #add health mean, remove DBV, r2 = 0.6234
pc2 = pr.cp$rotation[,2]
pc2_sv = map2*pc2
V = matrix(c(pc2, pc2_sv), ncol=2)
S = cov(smalldat[,5:21])

PC_Cov = t(V)%*%S%*%V
r2 = PC_Cov[1,2]/sqrt(PC_Cov[1,1]*PC_Cov[2,2])

## Permutation test with PCs
s1 = scores[,5]
s2 = scores[,6]
s3 = scores[,7]
s4 = scores[,8]
s5 = scores[,9]
s6 = scores[,10]

mod2 <- aovp(s1~s2*s3*s4*s5, center = FALSE)
mod3 <- aovp(PC2~factor(mut_rate)*factor(Virulence) +factor(treatment) +factor(InfectionRate), data = scores, center = FALSE)
summary(mod3)
mod3 <- aovp(PC1~factor(mut_rate)*factor(Virulence) +factor(treatment) +factor(InfectionRate), data = scores, center = FALSE)
summary(mod3)
mod3 <- aovp(PC3~factor(mut_rate)*factor(Virulence) +factor(treatment) +factor(InfectionRate), data = scores, center = FALSE)
summary(mod3)
mod3 <- aovp(PC4~factor(mut_rate)*factor(Virulence) +factor(treatment) +factor(InfectionRate), data = scores, center = FALSE)
summary(mod3)
mod3 <- aovp(PC5~factor(mut_rate)*factor(Virulence) +factor(treatment) +factor(InfectionRate), data = scores, center = FALSE)
summary(mod3)
## Correlation Circle with vectors
corcir = circle(c(0, 0), npoints = 100)
cors = as.data.frame(cor(pcdat, pr.cp$x[,1:4]))
arrows = data.frame(x1 = rep(0, nrow(cors)), y1 = rep(0, nrow(cors)), 
                    PC1 = cors$PC1, PC2 = cors$PC2)
arrows <- melt(arrows, id = c( "x1", "y1", "PC1"))
#ellipses using FactoMineR
pca <- PCA(smalldat[,1:22], scale.unit=TRUE, ncp=5, quali.sup = 1:4, graph = F)
concat = cbind.data.frame(factor(treatments), pca$ind$coord)
ellipse.coord = as.data.frame(coord.ellipse(concat,level.conf = 0.9999, bary=TRUE))
colnames(ellipse.coord) <- c("treatments", "PC1","PC2","call")

##biplot
#mut_rate
  ggbiplot(pr.cp2, group = factor(mut_rate2),obs.scale=1, var.scale=1, ellipse= TRUE, var.axes = FALSE, circle = FALSE) +
  geom_point(data =scores2, shape = 21, aes(x=PC1, y=PC2, fill = factor(mut_rate)), alpha = 0.8) + 
  theme_tufte(base_family = "Gill Sans MT", base_size = 14)
#virulence
ggbiplot(pr.cp2, group = factor(Virulence2),obs.scale=1, var.scale=1, ellipse= TRUE, var.axes = FALSE, circle = FALSE) +
  geom_point(data =scores2, shape = 21, aes(x=PC1, y=PC2, fill = factor(Virulence)), alpha = 0.8) + 
  theme_tufte(base_family = "Gill Sans MT", base_size = 14)
#Infection rate
ggbiplot(pr.cp2, group = factor(InfectionRate2),obs.scale=1, var.scale=1, ellipse= TRUE, var.axes = FALSE, circle = FALSE) +
  geom_point(data =scores2, shape = 21, aes(x=PC1, y=PC2, fill = factor(InfectionRate)), alpha = 0.8) + 
  theme_tufte(base_family = "Gill Sans MT", base_size = 14)
#years
ggbiplot(pr.cp2, group = factor(treatment2),obs.scale=1, var.scale=1, ellipse= TRUE, var.axes = FALSE, circle = FALSE) +
  geom_point(data =scores2, shape = 21, aes(x=PC1, y=PC2, fill = factor(treatmen)), alpha = 0.8) + 
  theme_tufte(base_family = "Gill Sans MT", base_size = 14)


### Correlation Circles
#PC1, PCA2
ggplot()+
  geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = PC1, yend = value, colour = factor(variable))) + 
  geom_text(data = cors[,1:2], size = 4,colour = muted("lightblue3"), aes(x = PC1, y = PC2, label = rownames(cors), family = "Gill Sans MT"))+
  geom_hline(yintercept = 0, colour = "gray65") + 
  geom_vline(xintercept = 0, colour = "gray65") + 
  xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "PC 1, 43.03% of Variance", y ="PC 2, 16.80% of Variance") +
  scale_color_manual(breaks = c("PC2"), values=c("lightblue3")) +
  theme_tufte(base_family = "Gill Sans MT", base_size = 12) +
  theme(legend.position = "none") #axis.title.x = element_text(face = 'bold'),


## BiPlots

p1<-ggplot() +
  geom_point(data =scores, aes(x=PC1, y=PC2, color = factor(treatments)), alpha = 0.5) +
  geom_path(data = ellipse.coord, aes(x = PC1, y = PC2, group = factor(treatments))) +
  theme(legend.position="none") 
p2<-ggplot(scores, aes(color = treatment)) +
  geom_point(aes(x=PC1, y=PC3), alpha = 0.5) +
  theme(legend.position="none") 
p3<-ggplot(scores, aes(color = treatment)) +
  geom_point(aes(x=PC1, y=PC4), alpha = 0.5) +
  theme(legend.position="none")
p4<-ggplot(scores, aes(color = treatment)) +
  geom_point(aes(x=PC2, y=PC3), alpha = 0.5) +
  theme(legend.position="none")
p5<-ggplot(scores, aes(color = treatment)) +
  geom_point(aes(x=PC2, y=PC4), alpha = 0.5) +
  theme(legend.position="none")
p6<-ggplot(scores, aes(color = treatment)) +
  geom_point(aes(x=PC3, y=PC4), alpha = 0.5) +
  theme(legend.position="none")

#multiplot(p1,p2,p3,p4,p5,p6, cols = 3)

#helpers
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}



