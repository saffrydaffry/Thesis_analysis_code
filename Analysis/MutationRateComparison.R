### Plot inhibition rate according to mutation rate.

# Connect to DB
require("RSQLite")
require(ggplot2)
con <- dbConnect("SQLite", dbname="NematodeABM.db")


get_dat_sub <- function(frame) {
	frame$NemDiff <- c(0, diff(frame$Nematodes))
	frame_sub <- subset(frame, NemDiff != 0)
	frame_sub <- subset(frame_sub, Tick != 0)
	frame_sub$yr <- frame_sub$Tick %/% 365.25
	frame_sub$InhibitionRate <- frame_sub$NemDiff / frame_sub$Nematodes
	return(frame_sub)
}


## background nematode inhibition changes (no infection)
#zeroinf <- dbGetQuery(con, "select * from zeroinf_dat")
#zinf_sub <- get_dat_sub(zeroinf)
#zinf_sub$mut_rate <- NA

## nematode inhibition changes with zero mutation rate
#tvgrid_nomut <- dbGetQuery(con, "select * from tvgrid_nomut_dat")
#tvg_nomut_sub <- get_dat_sub(tvgrid_nomut)
#tvg_nomut_sub$mut_rate <- 0


#tvg_01 <- dbGetQuery(con, "select * from varying_mutation_rate_dat where mut_rate == 0.1")
#tvg_01_sub <- get_dat_sub(tvg_01)

#tvg_02 <- dbGetQuery(con, "select * from varying_mutation_rate_dat where mut_rate == 0.2")
#tvg_02_sub <- get_dat_sub(tvg_02)

#tvg_04 <- dbGetQuery(con, "select * from varying_mutation_rate_dat where mut_rate == 0.4")
#tvg_04_sub <- get_dat_sub(tvg_04)

#tvg_06 <- dbGetQuery(con, "select * from varying_mutation_rate_dat where mut_rate == 0.6")
#tvg_06_sub <- get_dat_sub(tvg_06)

#tvg_08 <- dbGetQuery(con, "select * from varying_mutation_rate_dat where mut_rate == 0.8")
#tvg_08_sub <- get_dat_sub(tvg_08)

#dat <- rbind( tvg_nomut_sub, tvg_01_sub, tvg_02_sub, tvg_04_sub, tvg_06_sub, tvg_08_sub)

#rm(tvg_nomut_sub, tvg_01_sub, tvg_02_sub, tvg_04_sub, tvg_06_sub, tvg_08_sub, 
   #tvgrid_nomut, tvg_01, tvg_02, tvg_04, tvg_06, tvg_08)


## combine with meta data
#tvg_meta <-dbGetQuery(con, "select * from varying_mutation_rate_meta")
#tvg_no_mut_meta <- dbGetQuery(con, "select * from tvgrid_nomut_meta")
#tvg_meta$Filename <- paste(tvg_meta$Filename, "", sep = ".csv")
#tvg_no_mut_meta$Filename <- paste(tvg_no_mut_meta$Filename, "", sep = ".csv")
#tvg_no_mut_meta$mut_rate <- rep(0, nrow(tvg_no_mut_meta))
#tvg_meta <- rbind(tvg_meta, tvg_no_mut_meta)


### upload from new mut rate data
muts_dat <- dbGetQuery(con,"select * from mutsNew_dat ")
muts_meta <-dbGetQuery(con,"select * from mutsNew_meta")
  muts_meta$Filename <- paste(muts_meta$Filename, "", sep = ".csv")
muts_complete <- merge(muts_dat, muts_meta, by.x = "file", by.y = "Filename")

#tvg_new <-merge(tvg_meta, dat, by.x = "Filename", by.y = "file")
#tvg_new$mut_rate<-tvg_new$mut_rate.y

#remove unnecessary columns
#tvg_new$mut_rate.y<-NULL; tvg_new$mut_rate.x<-NULL; tvg_new$Filename<-NULL; tvg_new$file<-NULL;tvg_new$row_names.x<-NULL; tvg_new$row_names.y<-NULL
# remove a bit of outside season data (season is 115-240)

big_dat<- subset(muts_complete, (Tick>100 & Tick <255)|
                   (Tick>465 & Tick <620)|
                   (Tick>830 & Tick <985)| 
                   (Tick>1195 & Tick <1350)|
                   (Tick>1560 & Tick <1715))
#non-overlapping data
#big_dat<-subset(big_dat, Transmissibility==0.5) #mut_rate = 0 has Transmissbility = 0.2, 0.8
big_dat$years = big_dat$Tick/365.25
big_dat$yr <- big_dat$Tick %/% 365.25
big_dat$Sick <- big_dat$Virus_Load > 0.4
big_dat$HiT <- big_dat$Transmissibility_mean/big_dat$Virus_Load > 0.5
big_dat$HiV <- big_dat$Virulence_mean/big_dat$Virus_Load > 2.8
big_dat <-subset(big_dat, Virulence != 8) #remove virulence =8
big_dat$X <- NULL
big_dat$file <- NULL
big_dat$row_names.x <- NULL
big_dat$row_names.y <- NULL
big_dat$MaxTicks <- NULL


write.csv(big_dat, data.path)

#control data
zinf_sub<-subset(zinf_sub,(Tick>100 & Tick <255)|
                  (Tick>465 & Tick <620)|
                  (Tick>830 & Tick <985)| 
                  (Tick>1195 & Tick <1350)|
                  (Tick>1560 & Tick <1715))

#remove unnecessary columns
zinf_sub$row_names <- NULL; zinf_sub$mut_rate<-NULL; zinf_sub$file<-NULL
write.csv(zinf_sub,"C:/Users/Mac/Dropbox/SCN-Thesis/Analysis/control_dat.csv")

require(ggplot2)

 	
ggplot(big_dat, aes(x=as.factor(mut_rate), y=Nematodes)) + geom_boxplot() + 
	theme_bw() + 
	xlab("Mutation Rate") +
	ylab("Nematode Numbers") + 
	ggtitle("Influence of Mutation Rate over Time") +
	facet_grid(. ~ yr)


## Here we divide the virus load into low (<=0.4) or high (>0.4)
big_dat$Sick <- big_dat$Virus_Load > 0.4

ggplot(big_dat, aes(x=as.factor(mut_rate), y=Nematodes, fill=Sick)) + geom_boxplot() + 
	theme_bw() + 
	xlab("Mutation Rate") +
	ylab("Nematode Numbers") + 
	ggtitle("Influence of Mutation Rate over Time") +
	facet_grid(. ~ yr)
	
big_dat$HiT <- big_dat$Transmissibility_mean/big_dat$Virus_Load > 0.5
big_dat$HiV <- big_dat$Virulence_mean/big_dat$Virus_Load > 2.8

ggplot(big_dat, aes(x=as.factor(mut_rate), y=Nematodes, fill=Sick)) + geom_boxplot() + 
	theme_bw() + 
	xlab("Mutation Rate") +
	ylab("Nematode Numbers") + 
	ggtitle("Influence of Mutation Rate over Time") +
	facet_grid(. ~ yr) + 
	scale_fill_discrete(name="Viral Loads", 
		breaks=c(T, F), 
		labels=c("High (>0.4)", "Low (<=0.4)")) +
	theme(legend.position="top")

ggplot(big_dat, aes(x=as.factor(mut_rate), y=Nematodes, fill=HiV)) + geom_boxplot() + 
	theme_bw() + 
	xlab("Mutation Rate") +
	ylab("Nematode Numbers") + 
	ggtitle("Influence of Mutation Rate over Time") +
	facet_grid(. ~ yr) + 
	scale_fill_discrete(name="Virulence", 
		breaks=c(T, F), 
		labels=c("High (>2.8)", "Low (<=2.8)")) + 
	theme(legend.position="top")
	
ggplot(big_dat, aes(x=as.factor(mut_rate), y=Nematodes, fill=HiT)) + geom_boxplot() + 
	theme_bw() + 
	xlab("Mutation Rate") +
	ylab("Nematode Numbers") + 
	ggtitle("Influence of Mutation Rate over Time") +
	facet_grid(. ~ yr) + 
	scale_fill_discrete(name="Transmissibility", 
		breaks=c(T, F), 
		labels=c("High (>0.5)", "Low (<=0.5)")) + 
	theme(legend.position="top")


## Let's see how virulence changes over time. 
big_dat$iF <- big_dat$Fraction_Infected > 0.5
big_dat$vL <- big_dat$Virus_Load > 0.4
ggplot(subset(big_dat, Virulence_mean*Virus_Load < 7), 
	aes(x=as.factor(mut_rate), y=log(Virulence_mean * Virus_Load), fill=iF)) + geom_boxplot() +
	theme_bw() + 
	xlab("Mutation Rate") +
	ylab("Log(Pathogen Virulence) AU") + 
	#ggtitle("Influence of Mutation Rate over Time") +
	facet_grid(. ~ yr) + 
	scale_fill_discrete(name="Infected Fraction", 
		breaks=c(T, F), 
		labels=c("High (>0.5)", "Low (<=0.5)")) + 
	theme(legend.position="right")

tf_labeller <- function(var, value) {
	value <- as.character(value)
	if (var == "iF") {
		value[value == T] <- "High"
		value[value == F] <- "Low"
	}
	return(value)
}


s
	
big_dat$iF <- big_dat$Fraction_Infected > 0.5
big_dat$vL <- big_dat$Virus_Load > 0.4

ggplot(big_dat, aes(x=as.factor(mut_rate), y=Nematodes, fill=vL)) + geom_boxplot() + 
	theme_bw() + 
	xlab("Mutation Rate") +
	ylab("Nematode Numbers") + 
	ggtitle("Influence of Mutation Rate over Time") +
	facet_grid(. ~ yr) + 
	scale_fill_discrete(name="Virus Load", 
		breaks=c(T, F), 
		labels=c("High (>0.4)", "Low (<=0.4)")) + 
	theme(legend.position="top")

dbDisconnect(con)

### Generate inhibition distribution 

ggplot(subset(zinf_sub, yr<5), aes(x=Nematodes, fill=as.factor(yr))) + 
	geom_histogram(aes(y=..density..), colour="black", fill="white") +
	geom_density(alpha=0.3) +
	theme_bw() + 
	ggtitle("Nematode counts over time") + 
	scale_fill_discrete(name="Year") + 
	theme(legend.position="top")
	
require(mixtools)


for(y in 0:4) {
	yrsub <- subset(zinf_sub, yr==y)
	print(shapiro.test(yrsub$Nematodes))
	mixmdl <- normalmixEM(yrsub$Nematodes, k=2)
	print(summary(mixmdl))
	plot(mixmdl, which=2)
	readline()
}


### Calculate a vector giving the average path followed
## by the zinf runs. 

zinf_files <- unique(zeroinf$file)

zinf_run_dat <- numeric(0)
for(f in zinf_files) {
	run <- subset(zeroinf, file==f)$Nematodes
	length(run) <- 913 ## force the length to be the same - pad zeros on right
	zinf_run_dat <- cbind(zinf_run_dat, run)
}

zinf_nem_mean <- apply(zinf_run_dat, 1, mean)
zinf_nem_mean <- zinf_nem_mean[1:450] ## get the first 450 observations (5 yr)

zinf_nem_sd <- apply(zinf_run_dat, 1, sd)[1:450]

## get metadata 

var_meta <- dbGetQuery(con, "select * from varying_mutation_rate_meta")

## compute distance from average nem curve

get_dist_from_mean <- Vectorize(function(file) {
	dat <- dbGetQuery(con, paste("select * from varying_mutation_rate_dat where file == '", file, ".csv'", sep=""))
	vec <- dat$Nematodes
	if(length(vec) >= 450) {
		vec <- vec[1:450]
		}
	else {
		length(vec) <- 450
		}
	
	mat <- as.matrix(rbind(zinf_nem_mean, vec))
	return(dist(mat))
})

get_dist_from_mean2 <- Vectorize(function(f) {
	dat <- subset(zeroinf, file==paste(f, ".csv", sep=""))
	vec <- dat$Nematodes
	if(length(vec) >= 450) {
		vec <- vec[1:450]
		}
	else {
		length(vec) <- 450
		}
	
	mat <- as.matrix(rbind(zinf_nem_mean, vec))
	return(dist(mat))
})

get_dist_from_mean3 <- Vectorize(function(f) {
	dat <- subset(tvgrid_nomut, file==paste(f, ".csv", sep=""))
	vec <- dat$Nematodes
	if(length(vec) >= 450) {
		vec <- vec[1:450]
		}
	else {
		length(vec) <- 450
		}
	
	mat <- as.matrix(rbind(zinf_nem_mean, vec))
	return(dist(mat))
})

var_meta$dist_from_zinf <- get_dist_from_mean(var_meta$Filename)

zeromut_meta <- dbGetQuery(con, "select * from tvgrid_nomut_meta")
zeromut_meta$dist_from_zinf <- get_dist_from_mean3(zeromut_meta$Filename)
zeromut_meta$mut_rate <- 0

## loading the zeroinf metadata

zeroinf_meta <- read.csv("ZeroInf/ZeroInf.txt")
zeroinf_meta$mut_rate <- NA
zeroinf_meta$row_names <- 0

zeroinf_meta$dist_from_zinf <- get_dist_from_mean2(zeroinf_meta$Filename)

big_meta <- rbind(zeroinf_meta, zeromut_meta, var_meta)

ggplot(big_meta, aes(x=dist_from_zinf, fill=as.factor(mut_rate))) + 
	geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=10000) +
	geom_density(alpha=0.3) +
	theme_bw() + 
	ggtitle("Run distance distribution") + 
	scale_fill_discrete(name="Mutation rate") + 
	theme(legend.position="top") +
	xlab("Distance from the average noninfected nematode population") +
	ylab("Density")
	
ggplot(subset(big_meta, dist_from_zinf > 1.8e5), aes(x=dist_from_zinf, 	fill=as.factor(mut_rate))) + 
	geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=10000) +
	geom_density(alpha=0.3) +
	theme_bw() + 
	ggtitle("Run distance distribution for distance > 1.8e05") + 
	scale_fill_discrete(name="Mutation rate") + 
	theme(legend.position="top") +
	xlab("Distance from the average noninfected nematode population") +
	ylab("Density")

## Plotting the outlier runs.
ggplot(subset(big_meta, dist_from_zinf > 2e5), aes(x=dist_from_zinf, fill=as.factor(mut_rate))) + 
	#geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=10000) +
	geom_density(alpha=0.3) +
	theme_bw() + 
	ggtitle("Run distance distribution") + 
	scale_fill_discrete(name="Mutation rate") + 
	theme(legend.position="top") +
	xlab("Distance from the average noninfected nematode population") +
	ylab("Density") +
	facet_grid(. ~ InfectionRate)
	
