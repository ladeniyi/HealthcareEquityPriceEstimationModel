library("YRmisc")           # each session
library("readxl")           # each session

# 1. import spInfox excel file
spInfox <- read_excel("~/Desktop/BUA 633/Class 3 June 1/spInfox.xlsx")
spInfoSave<-spInfox
spInfox<-as.data.frame(spInfox)
dim(spInfox)
names(spInfox)


# 2. import spDatax  excel file
spDatax <- read_excel("~/Desktop/BUA 633/Class 3 June 1/spDatax.xlsx")
spDataSave<-spDatax
data.class(spDatax)
spDatax<-as.data.frame(spDatax)
dim(spDatax)
names(spDatax)

# Merge 2 dataframe by "tkr" overlap
spmdf<-merge(spInfox,spDatax,by="tkr")
names(spmdf)
dim(spmdf)

# create new numeric variable date extracted from date
spmdf$date
spmdf$year<-as.numeric(substring(spmdf$date,7,10))
names(spmdf)

# Cross Section Data Analysis
unique(spmdf$sector)
unique(spmdf$industry)
data.frame(table(spmdf$sector))

# extract cross section data for year 2022
csdf<-spmdf[spmdf$year==2022 & spmdf$industry=="Health Care Providers & Services",c("tkr","price","eps","bvps","year","name","industry")]
dim(csdf)
names(csdf)
csdf
csdf$price
                                        +   +
  # function specification   price = f(eps,bvps)
  
# Graphical
par(mfcol=c(3,3))  # par - partion
par(mfrow=c(3,3))  # par - partion

hist(csdf$price,xlab = "Price",ylab="Freq",main="Fig. 1 Hist of Price",col="black")
hist(csdf$eps,xlab = "EPS",ylab="Freq",main="Fig. 2 Hist of EPS", col="black")
hist(csdf$bvps,xlab = "BVPS",ylab="Freq",main="Fig. 3 Hist of BVPS", col="black")

# Scatterplots
par(mfrow=c(2,2))
scatter.smooth(csdf$bvps,csdf$price,xlab="BVPS",ylab="Price",main="Fig. 4 Price v BVPS")
scatter.smooth(csdf$eps,csdf$price,xlab="EPS",ylab="Price",main="Fig. 5 Price v EPS",
     pch="*",cex=2)   

Analytical
# des stats
ds.summ(csdf[,c("price","eps","bvps")],2)
ds.summ(csdf[,c("price","eps","bvps")],2)[,-c(7,8)]

# cor
round(cor(csdf[,c("price","eps","bvps")]),4)

#lm 
dim(csdf)
csdf<-na.omit(csdf)
dim(csdf)
fit<-lm(price~eps+bvps,data=csdf,na.action=na.omit)
summary(fit)
names(fit)
coefficients(fit)
names(summary(fit))
predValues<-predict(fit,csdf)
residValues<-residuals(fit)

# Model Validation
vdf<-data.frame(csdf,predValues,residValues)
par(mfrow=c(2,2))
hist(vdf$residValues)
#plot(vdf$predValues,vdf$price,type="n")
#   text(vdf$predValues,vdf$price,vdf$tkr)

scatter.smooth(vdf$predValues,vdf$price,type="n")
text(vdf$predValues,vdf$price,vdf$tkr)