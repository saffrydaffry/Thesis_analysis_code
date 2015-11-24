## Comparison between growth chamber and field

# Connect to DB
require("RSQLite")
con <- dbConnect("SQLite", dbname="NematodeABM.db")

get_dat_sub <- function(frame) {
	frame$NemDiff <- c(0, diff(frame$Nematodes))
	frame_sub <- subset(frame, NemDiff != 0)
	frame_sub <- subset(frame_sub, Tick != 0)
	frame_sub$yr <- frame_sub$Tick %/% 365
	frame_sub$InhibitionRate <- frame_sub$NemDiff / frame_sub$Nematodes
	return(frame_sub)
}


## background nematode inhibition changes (no infection)
zeroinf <- dbGetQuery(con, "select * from zeroinf_dat")
zinf_sub <- get_dat_sub(zeroinf)
zinf_sub$loc <- "C0"


zeroinf_gc <- dbGetQuery(con, "select * from growth_chamber_zinf")
zinfgc_sub <- get_dat_sub(zeroinf_gc)
zinfgc_sub$loc <- "G0"


cmi <- dbGetQuery(con, "select * from champaign_dat")
cmi_sub <- get_dat_sub(cmi)
cmi_sub$loc <- "C"

gwch <- dbGetQuery(con, "select * from growthchamber_dat")
gw_sub <- get_dat_sub(gwch)
gw_sub$loc <- "G"

big_dat <- rbind(zinf_sub, zinfgc_sub, cmi_sub, gw_sub)
big_dat <- subset(big_dat, yr <= 4)

require(ggplot2)

### Plot effect of environment
bp <- ggplot(subset(big_dat, yr==4), aes(x=as.factor(loc), y=Nematodes, 
			fill=loc)) + geom_boxplot()			
			
bp + scale_fill_discrete(name="Location", breaks=c("C0", "C", "G0", "G"), 
		labels=c(	"CMI0", 
					"CMI", 
					"GC",
					"GC0")) +
	theme_bw() + 
	xlab("Location") +
	ylab("Nematode Numbers") + 
	#ggtitle("Influence of Environment") +
	#facet_grid(. ~ yr) +
	theme(legend.position = 'right')
	
bp <- ggplot(subset(big_dat, yr==4), aes(x=as.factor(loc), y=Fraction_Infected, 
			fill=loc)) + geom_boxplot()			
			
bp + scale_fill_discrete(name="Location", breaks=c("C0", "C", "G0", "G"), 
		labels=c(	"CMI0", 
					"CMI", 
					"GC",
					"GC0")) +
	theme_bw() + 
	xlab("Location") +
	ylab("Nematode Numbers") + 
	#ggtitle("Influence of Environment") +
	#facet_grid(. ~ yr) +
	theme(legend.position = 'right')

require(scales)
ggplot(subset(big_dat, yr==4), aes(x=Fraction_Infected, fill=loc)) + 
	scale_x_continuous(trans=log10_trans()) +
	#scale_y_continuous(trans=log10_trans()) +
	geom_histogram(binwidth=0.05)	+
	scale_fill_discrete(name="Location", breaks=c("C0", "C", "G0", "G"), 
		labels=c(	"CMI0", 
					"CMI", 
					"GC",
					"GC0")) +
	theme_bw() + 
	xlab("Location") +
	ylab("Nematode Numbers") + 
	#ggtitle("Influence of Environment") +
	#facet_grid(. ~ yr) +
	theme(legend.position = 'right')
	
	
ggplot(subset(big_dat, Virus_Load != 0), aes(x=Transmissibility_mean/Virus_Load, y=Virulence_mean/Virus_Load)) + 
	geom_point(aes(alpha = 1/200)) +
	facet_grid(. ~ yr) + 
	theme_bw() +
	xlab("Transmissibility") +
	ylab("Virulence") 
	
tp <- ggplot(subset(big_dat, Virus_Load != 0 & Transmissibility_mean/Virus_Load > 0.55), aes(x=Transmissibility_mean/Virus_Load)) +
	geom_histogram(aes(y=..density..), colour=gray(0.5), fill="white", binwidth=0.1) + 
	geom_density() +
	theme_bw() +
	facet_grid(loc ~ yr) + 
	xlab("Transmissibility")
	
vp <- ggplot(subset(big_dat, Virus_Load != 0 & Virulence_mean/Virus_Load < 5), aes(x=Virulence_mean/Virus_Load)) +
	geom_histogram(aes(y=..density..), colour=gray(0.5), fill="white", binwidth=0.5) + 
	geom_density() +
	theme_bw() +
	facet_grid(loc ~ yr) + 
	xlab("Virulence") 
	
multiplot(tp, vp, cols=1)
			
bp + scale_fill_discrete(name="Location", breaks=c("C0", "C", "G"), 
		labels=c("C0 - Champaign, no viruses", "C - Champaign", "G - Growth Chamber")) +
	theme_bw() + 
	xlab("Location") +
	ylab("Transmissibility") + 
	#ggtitle("Influence of Environment") +
	facet_grid(. ~ yr) +
	theme(legend.position = 'top')
	
	
nemcmi <- subset(big_dat, loc=="C")$Nematodes
nemcmiz <- subset(big_dat, loc=="C0")$Nematodes
nemgc <- subset(big_dat, loc=="G")$Nematodes
nemgcz <- subset(big_dat, loc=="G0")$Nematodes

nemreg <- lm(Nematodes ~ Tick  + as.factor(loc), big_dat)

nemreg <- lm(Nematodes ~ Tick + Fraction_Infected + I(Virulence_mean * Virus_Load) + I(Transmissibility_mean * Virus_Load) + as.factor(loc), big_dat)
