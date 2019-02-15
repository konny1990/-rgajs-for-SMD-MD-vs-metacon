install.packages("meta")
library(meta)
#check with metacon
ncont1=c(12,13,14)
ncont2=c(14,15,16)
mean1=c(187.5,193.4,161.3)
mean2=c(165.6,171.9,180.5)
sd1=c(12.3, 8.5,6.9)
sd2=c(11.9, 10.8, 11.1)
pooledSMD1=metacont(ncont1,mean1,sd1,ncont2,mean2,sd2,sm="SMD")
summary(pooledSMD1)
#///////////////////////////

