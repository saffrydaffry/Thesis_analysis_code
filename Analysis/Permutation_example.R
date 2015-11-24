null.diff<-mean(null.y[carrier==1])-mean(null.y[carrier==0])
alt.diff<-mean(alt.y[carrier==1])-mean(alt.y[carrier==0])

one.test <- function(x,y) {
  xstar<-sample(x)
  mean(y[xstar==1])-mean(y[xstar==0])
}

many.truenull <- replicate(1000, one.test(carrier, null.y))
many.falsenull <- replicate(1000, one.test(carrier, alt.y))

hist(many.truenull)
abline(v=null.diff, lwd=2, col="purple")
mean(abs(many.truenull) > abs(null.diff))

hist(many.falsenull)
abline(v=alt.diff, lwd=2, col="purple")
mean(abs(many.falsenull) > abs(alt.diff))

##unknown dist ex
dat <- data.frame(y=rep(0:1,each=100), SNP1=rbinom(200,2,.1),
                  SNP2=rbinom(200,2,.2),SNP3=rbinom(200,2,.2),
                  SNP4=rbinom(200,2,.4),SNP5=rbinom(200,2,.1),
                  SNP6=rbinom(200,2,.2),SNP7=rbinom(200,2,.2),
                  SNP8=rbinom(200,2,.4))


oneZ<-function(outcome, snp){
  model <- glm(outcome~snp, family=binomial())
  coef(summary(model))["snp","z value"]
}
maxZ<-function(outcome, snps){
  allZs <- sapply(snps,
                  function(snp) oneZ(outcome, snp))
  max(abs(allZs))
}
true.maxZ<-maxZ(dat$y, dat[,-1])
manypermZ<-replicate(10000, maxZ(sample(dat$y), dat[,-1]))

true.maxZ<-maxZ(dat$y, dat[,-1])
manypermZ<-replicate(10000, maxZ(sample(dat$y), dat[,-1]))