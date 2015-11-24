##V x T
point2 = ddply(subset(summary_bigdat2,years>2), .(InfectionRate,mut_rate, Virulence, yr), summarize,
                      x1 = Virulence_mean[which.min(years)]/Virus_Load[which.min(years)],
                      xend = Virulence_mean[which.max(years)]/Virus_Load[which.max(years)],
                      y1 = Transmissibility_mean[which.min(years)]/Virus_Load[which.min(years)],
                      yend = Transmissibility_mean[which.max(years)]/Virus_Load[which.max(years)])

ggplot(subset(summary_bigdat2, InfectionRate==0.8 & yr >=2), aes(x = Virulence_mean/Virus_Load, y =Transmissibility_mean/Virus_Load))+
  geom_point(aes(color = factor(DBV_color), size = factor(yr)), alpha =0.5) +
  geom_vline(xintercept = 0, linetype ="dotted", color = "grey", size = 0.7)+
  geom_hline(yintercept = 0, linetype ="dotted", color = "grey", size = 0.7)+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  geom_segment(data = subset(point2, InfectionRate==0.8), size = 1,arrow = arrow(length = unit(0.2, "inches")), aes(x = x1, y = y1, xend = xend, yend = yend, alpha = factor(yr)))+
  xlab("Virulence")+ylab('Transmissibility') + 
  scale_color_manual(breaks = c(-1,0,1), values = c(muted("blue"),"orange","red"), labels = c("Q1", "Q2 & Q3","Q4"), name = "Range of Deaths") +
  scale_alpha_manual(values = c(0.4,0.7,1))+
  scale_size_manual(values = c(2,2.25,3), name = "Crop Year") +
  theme_tufte(base_family= 'Gill Sans MT', base_size = 14)


##PCA stuff

ggplot(subset(pca_tv, yr>2),aes(x =mortality_rate, y  = Transmissibility_mean/Virus_Load))+
  geom_point(shape = 21, size = 3, alpha = 0.7, aes(fill = factor(Virulence), size =factor(yr))) +
 geom_segment(data = subset(point.pca, yr>2) , size = 1, arrow = arrow(length = unit(0.2, "inches")), aes(x = xmid, y = ymid, xend = xend, yend = yend, alpha = factor(yr)))+
scale_alpha_manual(values = c(0.5,1))+
  scale_size_manual(values = c(3,4))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14) +
  coord_flip()

point.pca = ddply(pca_tv, .(mut_rate, Virulence, yr), summarize,
               n= length(years),
               x1 = mortality_rate[which.min(years)],
               xmid = mortality_rate[n/2],
               xend = mortality_rate[which.max(years)],
               y1 = Transmissibility_mean[which.min(years)]/Virus_Load[which.min(years)],
               ymid = Transmissibility_mean[n/2]/Virus_Load[n/2],
               yend = Transmissibility_mean[which.max(years)]/Virus_Load[which.max(years)])



ggplot(subset(pca_tv, Virulence>0.1 & Virulence <2.5),aes(x =mortality_rate, y  = (Transmissibility_mean/Virus_Load)))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(mut_rate), size = factor(yr))) +
  #geom_smooth()+
  scale_size_manual(values = c(5.2,4.3,3.6,2.9,2.3))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(Virulence~., as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

idx = seq(1, nrow(pca_tv),by=3)

## save
ggplot(subset(pca_tv),aes(x =R0+mut_rate*8, y  = (Virulence_mean)/Virus_Load+Virulence*3))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0,0.1,0.2,0.4,0.6,0.8)*8, color = "grey")+
  #geom_smooth()+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  #facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

ggplot(subset(pca_tv),aes(x =R0+mut_rate*8, y  = (Transmissibility_mean)/Virus_Load+Virulence*3))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0,0.1,0.2,0.4,0.6,0.8)*8, color = "grey")+
  #geom_smooth()+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  #facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

ggplot(subset(pca_tv),aes(x =mortality_rate+mut_rate, y  = (Virulence_mean)/Virus_Load+Virulence))+
  geom_point(shape = 21, alpha = 0.5, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0,0.1,0.2,0.4,0.6,0.8), color = "grey")+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  #facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)+coord_flip()

#last 2 years
ggplot(subset(pca_tv,yr>2),aes(x =mortality_rate+mut_rate, y  = (Transmissibility_mean)/Virus_Load+Virulence))+
  geom_point(shape = 21, alpha = 0.5, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0,0.1,0.2,0.4,0.6,0.8), color = "grey")+
  #geom_smooth(aes(group = interaction(Virulence,mut_rate)))+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  #facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)+coord_flip()
#later years
ggplot(subset(pca_tv, years >2.5),aes(x =asinh(mortality_rate), y  = asinh(Transmissibility_mean/Virus_Load)))+
  geom_point(shape = 21, alpha = 0.5, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0,0.1,0.2,0.4,0.6,0.8), color = "grey")+
  #geom_smooth(aes(group = interaction(Virulence,mut_rate)))+
  scale_size_manual(values = c(2,2.4,3.3,4.6))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)+coord_flip()+scale_x_sqrt()+scale_y_log10()

## with facetting
ggplot(subset(pca_tv,yr>2),aes(x =mortality_rate, y  = (Transmissibility_mean)/Virus_Load))+
  geom_point(shape = 21, alpha = 0.5, aes(fill = factor(Virulence), size = factor(yr))) +
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  ylim(0.5,1)+
  geom_segment(data = subset(point2.center, yr>=2 & InfectionRate==0.8), size = 1, arrow = arrow(length = unit(0.2, "inches")), aes(x = xmid, y = ymid, xend = xend, yend = yend, alpha = factor(yr)))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  scale_alpha_manual(values = c(0.4,0.7,1))+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)+coord_flip()


ggplot(subset(pca_tv),aes(x =Virulence_mean/Virus_Load+mut_rate*15-Virulence, y  = (Transmissibility_mean)/Virus_Load+Virulence))+
  geom_point(shape = 21, alpha = 0.5, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0,0.1,0.2,0.4,0.6,0.8)*15, color = "grey")+
  #geom_smooth()+
  scale_size_manual(values = c(2,2.4,3.3,3.8,4.5))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  #facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

ggplot(subset(summary_bigdat2, InfectionRate==0.8& yr>0),aes(x =Virulence_mean/Virus_Load+mut_rate*15-Virulence, y  = Fraction_Infected+Virulence))+
  geom_point(shape = 21, alpha = 0.5, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0,0.1,0.2,0.4,0.6,0.8)*15, color = "grey")+
  #geom_smooth()+
  scale_size_manual(values = c(2,2.4,3.3,3.8,4.5))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  #facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)
# original sizes 5.2,4.3,3.6,2.9,2.3)
############################
ggplot(subset(summary_bigdat2, yr>1 &InfectionRate==0.8& Virulence <4 &Virulence>0.1),aes(x =(Virulence_mean)/Virus_Load+Virulence*0.5, y  = mortality_rate+mut_rate))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_vline(x = c(0.1,0.5,1,1.5,2,2.5,4), color = "grey" )+
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,2.5,4))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

ggplot(subset(summary_bigdat2, InfectionRate==0.8),aes(x =(Virulence_mean-Virulence)/Virus_Load-(Transmissibility_mean/Virus_Load), y  = (Transmissibility_mean/(years*Virus_Load))^-0.5))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

ggplot(subset(summary_bigdat2, InfectionRate==0.8),aes(x =Virulence_mean/Virus_Load, y  = mortality_rate/years))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

###########################
#derivatives
###########################

sub_dev = sub
sub_dev$Fraction_Infected = subset(summary_bigdat2,InfectionRate==0.8)$Fraction_Infected
sub_dev$VLdot = c(0,diff(sub_dev$Virulence_mean/sub_dev$Virus_Load)*(1/diff(sub_dev$years)))
sub_dev$TLdot = c(0,diff(sub_dev$Transmissibility_mean/sub_dev$Virus_Load)*(1/diff(sub_dev$years)))
#sub_dev$Vdot = sub_dev$Virulence_mean/(sub_dev$years) ## with VL is cleaner
#sub_dev$Tdot = sub_dev$Transmissibility_mean/(sub_dev$years)
sub_dev$Ddot = c(0,diff(sub_dev$mortality_rate)/diff(sub_dev$years))
sub_dev$Rdot = c(0,diff(sub_dev$R0)/diff(sub_dev$years))

pairs(~VLdot+I(Virulence_mean/Virus_Load)+I(Transmissibility_mean/Virus_Load), data = subset(sub_dev, Virulence>1& mut_rate==0.4))

pairs(~Ddot+I(Virulence_mean/Virus_Load)+I(Transmissibility_mean/Virus_Load)+R0+mortality_rate, data = subset(sub_dev, mut_rate==0.4 & Virulence==1.5))

qplot(VLdot,Virulence_mean, data = subset(sub_dev, mut_rate==0.8), goem = "point", color = factor(Virulence))
r))) +
##critical points
Vdot_zeros = ddply(sub_dev, .(mut_rate, Virulence,years), summarize,
                   Trans0 = Transmissibility_mean[which(VLdot==0)]/Virus_Load,
                   Vir0 = Virulence_mean[which(VLdot==0)]/Virus_Load)

Ddot_zeros = ddply(sub_dev, .(mut_rate, Virulence,years), summarize,
                   Trans0 = Transmissibility_mean[which(Ddot==0)]/Virus_Load,
                   Vir0 = Virulence_mean[which(Ddot==0)]/Virus_Load,
                   R00=R0[which(Ddot==0)],
                   Mort0 = mortality_rate[which(Ddot==0)])

Rdot_zeros = ddply(sub_dev, .(mut_rate, Virulence,years), summarize,
                   Trans0 = Transmissibility_mean[which(Rdot==0)]/Virus_Load,
                   Vir0 = Virulence_mean[which(Rdot==0)]/Virus_Load,
                   R00=R0[which(Rdot==0)],
                   Mort0 = mortality_rate[which(Rdot==0)])

#mort x vir
ggplot(subset(summary_bigdat2, InfectionRate==0.8),aes(x =Virulence_mean/Virus_Load, y  = mortality_rate))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_point(data = Ddot_zeros, aes(x = Vir0, y = Mort0),color = "black", size = 3)+
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(Virulence~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

#mort x trans
ggplot(subset(summary_bigdat2, yr>2&InfectionRate==0.8),aes(x =Transmissibility_mean/Virus_Load+Virulence, y  = 4*mortality_rate+3*mut_rate))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_point(data = subset(Ddot_zeros,years>2), aes(x = Trans0+Virulence, y = 4*Mort0+3*mut_rate),fill = "black", size = 1.5,shape = 22)+
  #xlim(0.5,1.5)+
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,3.3,4))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)+scale_x_log10()

#R0 x vir
ggplot(subset(sub_dev, InfectionRate==0.8),aes(x =Virulence_mean/Virus_Load, y  = R0))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_line(data = Rdot_zeros, aes(x = Vir0, y = R00),color = "black")+
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)


#R0 x mort
ggplot(subset(sub_dev, InfectionRate==0.8),aes(x =mortality_rate, y  = R0))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_point(data = Rdot_zeros, aes(x = Mort0, y = R00),color = "black")+
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(mut_rate~Virulence, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)

#just zeros
ggplot(data = subset(Rdot_zeros, years>3& Virulence >0.5 &Virulence!=4), aes(x = Mort0, y = R00))+
  geom_point(aes(color = factor(Virulence),size = years))+
  geom_smooth(aes(group = interaction(factor(Virulence),factor(mut_rate))), se=FALSE)+
  facet_grid(mut_rate~.)+
  ylim(-5,5)+
    theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)


ggplot(subset(summary_bigdat2, InfectionRate==0.8),aes(x =(Virulence_mean)/Virus_Load, y  = (Transmissibility_mean/Virus_Load)))+
  geom_point(shape = 21, alpha = 0.6, aes(fill = factor(Virulence), size = factor(yr))) +
  geom_point(data = Vdot_zeros, aes(x = Vir0, y = Trans0),color = "black", size = 3)+
  #geom_smooth(aes(group = factor(Virulence)))+
  scale_size_manual(values = c(2,2.4,3.3,4,5.2))+
  #scale_fill_gradient(low="navy",high = "goldenrod1")+
  geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
  facet_grid(.~mut_rate, as.table = FALSE) +
  theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)


library(MASS)

#vir x trans
fit = lm(log(Vdot)~Virulence_mean/Virus_Load+Transmissibility_mean/Virus_Load+factor(Virulence)+factor(mut_rate)+0,data = sub_dev)
boxcox(fit)
summary(fit)

fit = lm(Tdot^-.5~Virulence_mean/Virus_Load+Transmissibility_mean/Virus_Load+factor(Virulence)+factor(mut_rate)+0,data = sub_dev)
boxcox(fit)
summary(fit)

#mort_rate x trans
fit = lm(Ddot ~ mortality_rate+Transmissibility_mean/Virus_Load +factor(Virulence)*factor(mut_rate), data = sub_dev)
boxcox(fit)
summary(fit)

fit = lm(Tdot ~ mortality_rate+Transmissibility_mean/Virus_Load +factor(Virulence)*factor(mut_rate)+0, data = sub_dev)
boxcox(fit)
summary(fit)
