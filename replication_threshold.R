library(haven)
library(dplyr)
library(segmented)


e <- read_dta(".../e.dta")

# cAST NET
at<-e%>%filter(arte_pesca=="ATARRAYA")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.at2<-lm(lcpue~salinidad+sal2,data=at)
fit.at<-lm(lcpue~salinidad,data=at)
fit.at.seg1<-segmented(fit.at,seg.Z=~salinidad,psi=c(25))
fit.at.seg1
fit.at.seg2<-segmented(fit.at,seg.Z=~salinidad,psi=c(5,25))
fit.at.seg2
AIC(fit.at.seg1)
AIC(fit.at.seg2)
AIC(fit.at)
AIC(fit.at2)
slope(fit.at.seg2)

#SEINE NET/BAG

cha<-e%>%filter(arte_pesca=="CHINCHORRA")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.cha2<-lm(lcpue~salinidad+sal2,data=cha)
fit.cha<-lm(lcpue~salinidad,data=cha)
fit.cha.seg1<-segmented(fit.cha,seg.Z=~salinidad,psi=c(25))
fit.cha.seg1
fit.cha.seg2<-segmented(fit.cha,seg.Z=~salinidad,psi=c(5,25))
fit.cha.seg2
AIC(fit.cha.seg1)
AIC(fit.cha.seg2)
AIC(fit.cha)
AIC(fit.cha2)
slope(fit.cha.seg1)

#SEINE NET

cho<-e%>%filter(arte_pesca=="CHINCHORRO")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.cho2<-lm(lcpue~salinidad+sal2,data=cho)
fit.cho<-lm(lcpue~salinidad,data=cho)
fit.cho.seg1<-segmented(fit.cho,seg.Z=~salinidad,psi=c(25))
fit.cho.seg1
fit.cho.seg2<-segmented(fit.cho,seg.Z=~salinidad,psi=c(5,25))
fit.cho.seg2
AIC(fit.cho.seg1)
AIC(fit.cho.seg2)
AIC(fit.cho)
AIC(fit.cho2)
slope(fit.cho.seg2)

#BASKET TRAP

na<-e%>%filter(arte_pesca=="NASAS")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.na2<-lm(lcpue~salinidad+sal2,data=na)
fit.na<-lm(lcpue~salinidad,data=na)
fit.na.seg1<-segmented(fit.na,seg.Z=~salinidad,psi=c(25))
fit.na.seg1
fit.na.seg2<-segmented(fit.na,seg.Z=~salinidad,psi=c(5,25))
fit.na.seg2
AIC(fit.na.seg1)
AIC(fit.na.seg2)
AIC(fit.na)
AIC(fit.na2)
fit.na$coefficients[2]

#HOOKS AND LINE

pa<-e%>%filter(arte_pesca=="PALANGRE")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.pa2<-lm(lcpue~salinidad+sal2,data=pa)
fit.pa<-lm(lcpue~salinidad,data=pa)
fit.pa.seg1<-segmented(fit.pa,seg.Z=~salinidad,psi=c(25))
fit.pa.seg1
fit.pa.seg2<-segmented(fit.pa,seg.Z=~salinidad,psi=c(5,25))
fit.pa.seg2
AIC(fit.pa.seg1)
AIC(fit.pa.seg2)
AIC(fit.pa)
AIC(fit.pa2)
slope(fit.pa.seg1)

#ENCIRCLING GILL NET

bo<-e%>%filter(arte_pesca=="RED DE ENMALLE (BOLICHEO)")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.bo2<-lm(lcpue~salinidad+sal2,data=bo)
fit.bo<-lm(lcpue~salinidad,data=bo)
fit.bo.seg1<-segmented(fit.bo,seg.Z=~salinidad,psi=c(25))
fit.bo.seg1
fit.bo.seg2<-segmented(fit.bo,seg.Z=~salinidad,psi=c(5,25))
fit.bo.seg2
AIC(fit.bo.seg1)
AIC(fit.bo.seg2)
AIC(fit.bo)
AIC(fit.bo2)
fit.bo$coefficients[2]

#FIXED GILL NET

fi<-e%>%filter(arte_pesca=="RED DE ENMALLE (FIJA)")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.fi2<-lm(lcpue~salinidad+sal2,data=fi)
fit.fi<-lm(lcpue~salinidad,data=fi)
fit.fi.seg1<-segmented(fit.fi,seg.Z=~salinidad,psi=c(25))
fit.fi.seg1
fit.fi.seg2<-segmented(fit.fi,seg.Z=~salinidad,psi=c(5,25))
fit.fi.seg2
AIC(fit.fi.seg1)
AIC(fit.fi.seg2)
AIC(fit.fi)
AIC(fit.fi2)
slope(fit.fi.seg1)

#FIXED GILL NET/Z

za<-e%>%filter(arte_pesca=="RED DE ENMALLE (ZANGARREO)")%>%subset(select=c(year,mes,lcpue,salinidad,sal2))
fit.za2<-lm(lcpue~salinidad+sal2,data=za)
fit.za<-lm(lcpue~salinidad,data=za)
fit.za.seg1<-segmented(fit.za,seg.Z=~salinidad,psi=c(25))
fit.za.seg1
fit.za.seg2<-segmented(fit.za,seg.Z=~salinidad,psi=c(5,25))
fit.za.seg2
AIC(fit.za.seg1)
AIC(fit.za.seg2)
AIC(fit.za)
AIC(fit.za2)
slope(fit.za.seg2)


#FIGURE 3

salinidad<-(seq(min(na$salinidad,na.rm=TRUE),max(na$salinidad,na.rm=TRUE),length.out=100))
ci_na<-predict(fit.na,newdata =data.frame(salinidad),interval="confidence")
ci_bo<-predict(fit.bo,newdata =data.frame(salinidad),interval="confidence")

png("segmented.png", width = 6000, height = 3000, res = 300)

par(mfrow=c(2,4))
plot(at$salinidad,at$lcpue,
     main="Cast net",xlab="",ylab="")
plot(fit.at.seg2,add=T,conf.level=0.95, shade=TRUE)
abline(v=c(7.8,31.4), lty=c(2,2))
plot(cha$salinidad,cha$lcpue,
     main="Seine net/bag",xlab="",ylab="")
plot(fit.cha.seg1,add=T,conf.level=0.95, shade=TRUE)
abline(v=23.3, lty=2)
plot(cho$salinidad,cho$lcpue,
     main="Seine net",xlab="",ylab="")
plot(fit.cho.seg2,add=T,conf.level=0.95, shade=TRUE)
abline(v=c(3.3,22.8), lty=c(2,2))
plot(na$salinidad,na$lcpue,
     main="Basket trap",xlab="",ylab="")
polygon(c(rev(salinidad),salinidad),
        c(rev(ci_na[,3]),ci_na[,2]),col="#CCCCCC",border=NA)
abline(fit.na,col="red")
plot(pa$salinidad,pa$lcpue,
     main="Hooks and line",xlab="",ylab="")
plot(fit.pa.seg1,add=T,conf.level=0.95, shade=TRUE)
abline(v=32.3, lty=2)
plot(bo$salinidad,bo$lcpue,
     main="Encircling gill net",xlab="",ylab="")
polygon(c(rev(salinidad),salinidad),
        c(rev(ci_bo[,3]),ci_bo[,2]),col="#CCCCCC",border=NA)
abline(fit.bo,col="red")
plot(fi$salinidad,fi$lcpue,
     main="Fixed gill net",xlab="",ylab="")
plot(fit.fi.seg1,add=T,conf.level=0.95, shade=TRUE)
abline(v=25.8, lty=2)
plot(za$salinidad,za$lcpue,
     main="Fixed gill net/z",xlab="",ylab="")
plot(fit.za.seg2,add=T,conf.level=0.95, shade=TRUE)
abline(v=c(3.3,24), lty=c(2,2))

dev.off()

