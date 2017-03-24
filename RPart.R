#install.packages("foreign")
library(foreign)
data.set = read.dbf("sample_points_200m_all_data_10_16updated.dbf", as.is = FALSE)

code5 = which(data.set$code==5)
data5 = data.set[code5,]


data5 = data5[which(data5$depth<0),]
data5 = data5[which(data5$depth> -9999),]

data5$river = data5$RiverDistC/1000
data5$geor = data5$GeorgeDist/1000
data5$port = data5$PortDist/1000
data5$shore = data5$shoredista/1000
data5$hist = data5$HistAleDis/1000
data5$ale = data5$HistAleDis/1000


data5$riverSq = data5$river^2
data5$georSq = data5$geor^2
data5$portSq = data5$port^2
data5$shoreSq = data5$shore^2
data5$histSq = data5$hist^2
data5$aleSq = data5$ale^2
data5$slopSq = (data5$slope)^2
data5$depSq = (data5$depth)^2


# create train and test sets (~40% for training, ~60% for testing)
data5$randu <- runif(nrow(data5),0,1)
data5.train <- data5[data5$randu < .4,]
data5.test <- data5[data5$randu >= .6,]


library("rpart")
model.rpart <- rpart(as.factor(SummerCodP)~ depth + slope + river + geor + port + shore + hist + ale, data = data5.train)
plotcp(model.rpart,minline= TRUE)
model.prune <- prune(model.rpart,cp=.011)
 
# plot it
library(rpart.plot)
# Figure 1 in paper
rpart.plot(model.prune,type=0)
predTest = predict(model.prune, newdata = data5.test, type = "class")
conf = table(predTest, data5.test$FishGround)
error = (conf[2,1] + conf[1,2])/sum(conf)
# PDF plot
 pdf("titanic-age-class-plot.pdf")
 rpart.plot(model.rpart,type=0,extra=2,cex=2)
 graphics.off()

 

 ########
n = length(data5$geor)
SpringCodPDecision = rep(0,n)
for(i in 1:n)
{
  if(data5$geor >=98 & data5$geor <118 | data5$geor >=118 & data5$geor <121 & data5$depth >= -185)
  {
    SpringCodPDecision = 1
  }
}

write.csv(data.frame(data5$POINT_X, data5$POINT_Y,SpringCodPDecision), file = "SpringCodPDecision.csv")


n = length(data5$geor)
SummerCodPDecision = rep(0,n)
for(i in 1:n)
{
  if(data5$geor >=238 & data5$geor <269 & data5$depth < -78 & data5$depth >= -180 & data5$hist >=31 & data5$hist <35 & data5$shore>=12 & data5$shore <=21)
  {
    SummerCodPDecision = 1
  }
}

#Summer Spring
one = which(data5$geor >=98 & data5$geor <118)
two = which(data5$geor >=118 & data5$geor <121 & data5$depth >= -185)
three =   which(data5$geor >=260 & data5$geor <269 & data5$depth >= -180 & data5$hist >=18 & data5$hist <27)
four = which(data5$geor >=238 & data5$geor <269 & data5$depth >= -78 & data5$hist >=27 & data5$hist <35 & data5$shore>=12)
five = which(data5$geor >=238 & data5$geor <269 & data5$depth < -78 & data5$depth >= -180 & data5$hist >=31 & data5$hist <35 & data5$shore>=12 & data5$shore <=21)


#Fall
fOne = which(data5$geor <268 & data5$geor >=227 &data5$hist>=20 & data5$depth >=-138 & data5$port >=34 & data5$river <47)
fTwo = which(data5$geor <220 & data5$geor >=202 &data5$hist>=20 & data5$depth >=-138 & data5$port >=34 & data5$river <47)
fThree = which(data5$geor <240 & data5$geor >=202 &data5$hist>=20 & data5$hist <40 & data5$depth >=-138 & data5$shore <25)
fFour = which(data5$geor <236 & data5$geor >=231 &data5$hist>=27 & data5$depth >=-138 & data5$port <34 & data5$river <33)
fFive = which(data5$geor <268 & data5$geor >=250 &data5$hist>=30 & data5$depth >=-138 & data5$port <34 & data5$river <33)


#Winter
wOne = which(data5$depth >= -100 & data5$shore >=11 & data5$geor >= 267 & data5$hist >=40 & data5$depth >=-90)
WTwo = which(data5$shore >=11 & data5$geor >= 248 & data5$geor <267 & data5$hist >=26 & data5$hist <34 & data5$depth >=-91 & data5$river <20)
WThree = which(data5$depth >= -100 & data5$shore >=11 & data5$geor >= 248 & data5$geor <267 & data5$hist <26 & data5$river <20 & data5$port<17)
WFour = which(data5$depth >= -100 & data5$shore >=17 & data5$geor >= 248 & data5$geor <267 & data5$hist >=32 & data5$river <33 & data5$river >=20 & data5$port<28)
WFive = which(data5$depth <= -50 & data5$depth >= -100 & data5$shore <11 & data5$geor >= 267 & data5$geor <304 & data5$hist >=16  &data5$river <4)
WSix =  which( data5$depth >= -100 & data5$shore <11 & data5$geor >= 264 & data5$geor <269 & data5$hist <2616 & data5$depth< -50 &data5$river <8.6)

SpringCodDecision = rep(0,length(data5$SpringCodP))
SummerCodDecision = rep(0, length(data5$SummerCodP))
FallCodDecision = rep(0,length(data5$FallCodPre))
WinterCodDecision = rep(0,length(data5$WinterCodP))

SpringCodDecision = replace(SummerCodDecision,c(one,two),1)
SummerCodDecision = replace(SummerCodDecision,c(one,two,three, four, five),1)
FallCodDecision = replace(FallCodDecision, c(fOne,fTwo,fThree,fFour,fFive),1)
WinterCodDecision = replace(WinterCodDecision,c(wOne,WTwo,WThree,WFour,WFive,WSix),1)
write.csv(data.frame(data5$POINT_X, data5$POINT_Y,SummerCodDecision), file = "SummerCodDecision.csv")

write.csv(data.frame(data5$POINT_X, data5$POINT_Y,SpringCodPDecision), file = "SpringCodDecision.csv")
write.csv(data.frame(data5$POINT_X, data5$POINT_Y,FallCodDecision), file = "FallCodDecision.csv")
write.csv(data.frame(data5$POINT_X, data5$POINT_Y,WinterCodDecision), file = "WinterCodDecision.csv")

