###################
#FISHERIES        #
#Parikshit Sharma #
#January 2017     #
##################

install.packages("foreign")
library(foreign)
#library(stargazer)
#library(ggplot2)
#library(logistf)
#library(dummies)

#Reading the File
data.set = read.dbf("sample_points_200m_all_data_10_16updated.dbf", as.is = FALSE)
names(data.set)
#summary(data.set)



#Omitting Missing and negative from depth and slope
omitlistDepth1 = which(data.set$depth == -9999) #409 entries found
omitlistDepth2 = which(data.set$depth > 0) #-->  #1983 entries found

#omitlistslope1 = which(data.set$slope == -9999)--> same as the ones for deppth


#Isolating the Columns
ObjectID = data.set[,1][-c(omitlistDepth1,omitlistDepth2)]
depth = data.set[,2][-c(omitlistDepth1,omitlistDepth2)]
slope = data.set[,3][-c(omitlistDepth1,omitlistDepth2)]
shoreDist = data.set[,4][-c(omitlistDepth1,omitlistDepth2)]
portDist = data.set[,5][-c(omitlistDepth1,omitlistDepth2)]
georgeDist = data.set[,6][-c(omitlistDepth1,omitlistDepth2)]
xCor = data.set[,7][-c(omitlistDepth1,omitlistDepth2)]
yCor = data.set[,8][-c(omitlistDepth1,omitlistDepth2)]
pointID = data.set[,9][-c(omitlistDepth1,omitlistDepth2)]
histAleDist = data.set[,10][-c(omitlistDepth1,omitlistDepth2)]
winterCod = data.set[,11][-c(omitlistDepth1,omitlistDepth2)]
springCod = data.set[,12][-c(omitlistDepth1,omitlistDepth2)]
summerCod = data.set[,13][-c(omitlistDepth1,omitlistDepth2)]
fallCod = data.set[,14][-c(omitlistDepth1,omitlistDepth2)]
riverDist = data.set[,15][-c(omitlistDepth1,omitlistDepth2)]
fishGround = data.set[,16][-c(omitlistDepth1,omitlistDepth2)]
code = data.set[,17][-c(omitlistDepth1,omitlistDepth2)]
winterCodP = data.set[,18][-c(omitlistDepth1,omitlistDepth2)]
springCodP = data.set[,19][-c(omitlistDepth1,omitlistDepth2)]
summerCodP = data.set[,20][-c(omitlistDepth1,omitlistDepth2)]
fallCodP = data.set[,21][-c(omitlistDepth1,omitlistDepth2)]

#Converting to meters
shoreDist = shoreDist/1000
portDist = portDist/1000
georgeDist = georgeDist/1000
histAleDist = histAleDist/1000
riverDist = riverDist/1000

#Squaring the values

depthSq = depth^2
slopeSq = slope^2
shoreDistSq = shoreDist^2
portDistSq = portDist^2
georgeDistSq = georgeDist^2
histAleDistSq = histAleDist^2
riverDistSq = riverDist^2

depth = sqrt(depthSq)
slope = sqrt(slopeSq)
shoreDist = sqrt(shoreDistSq)
portDist = sqrt(portDistSq)
georgeDist = sqrt(georgeDistSq)
histAleDist = sqrt(histAleDistSq)
riverDist = sqrt(riverDistSq)

#Creating Dummies
num.iter = length(winterCod)

fishGround2 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(winterCod[i] > 0 & springCod[i] > 0 & summerCod[i] > 0 & fallCod[i] >0)
  {
    fishGround2[i] = 1
  }
}


num.iter = length(winterCod)

fishGround3 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(winterCod[i] > 1 & springCod[i] > 1 & summerCod[i] > 1 & fallCod[i] >1)
  {
    fishGround3[i] = 1
  }
}



num.iter = length(winterCod)

fishGround4 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(winterCod[i] >2 & springCod[i] > 2 & summerCod[i] > 2 & fallCod[i] >2)
  {
    fishGround4[i] = 1
  }
}


#WinterCod
num.iter = length(winterCod)

fishGroundWinter = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(winterCod[i] > 0 )
  {
    fishGroundWinter[i] = 1
  }
}

#SpringCod
num.iter = length(winterCod)

fishGroundSpring = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(springCod[i] > 0 )
  {
    fishGroundSpring[i] = 1
  }
}


##Summer Cod
num.iter = length(winterCod)

fishGroundSummer = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(summerCod[i] > 0 )
  {
    fishGroundSummer[i] = 1
  }
}

#FallCod
num.iter = length(winterCod)

fishGroundFall = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(fallCod[i] > 0 )
  {
    fishGroundFall[i] = 1
  }
}

#SeasonalSimilarities
winterSpring = which(fishGroundFall == fishGroundSpring) 
springSummer = which(fishGroundSpring == fishGroundSummer)
summerFall = which(fishGroundSummer == fishGroundFall)
fallWinter = which(fishGroundFall == fishGroundWinter)


#CodeDummies

#Code =1
num.iter = length(winterCod)
fishGroundCode1 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(code[i]==1 )
  {
    fishGroundCode1[i] = 1
  }
}

#Code = 2
num.iter = length(winterCod)
fishGroundCode2 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(code[i]==2 )
  {
    fishGroundCode2[i] = 1
  }
}


#Code =3
num.iter = length(winterCod)
fishGroundCode3 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(code[i]==3)
  {
    fishGroundCode3[i] = 1
  }
}


#Code=4
num.iter = length(winterCod)
fishGroundCode4 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(code[i]==4 )
  {
    fishGroundCode4[i] = 1
  }
}


#Code=5
num.iter = length(winterCod)
fishGroundCode5 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(code[i]==5 )
  {
    fishGroundCode5[i] = 1
  }
}



#Code=6
num.iter = length(winterCod)
fishGroundCode6 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(code[i]==6 )
  {
    fishGroundCode6[i] = 1
  }
}

####################################### REGRESSIONS ########################################

Regression = glm(fishGround ~ riverDist + sqrt(georgeDistSq) + sqrt(portDistSq) + histAleDist +  sqrt(slopeSq) + sqrt(depthSq), data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSq = glm(fishGround ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqWinter = glm(fishGroundWinter ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqSpring = glm(fishGroundSpring ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqSummer = glm(fishGroundSummer ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqFall = glm(fishGroundFall ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqCode1 = glm(fishGroundCode1 ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqCode2 = glm(fishGroundCode2 ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqCode3 = glm(fishGroundCode3 ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqCode4 = glm(fishGroundCode4 ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqCode5 = glm(fishGroundCode5 ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqCode6 = glm(fishGroundCode6 ~ sqrt(riverDistSq) + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

#Slope Estimates

ProbEstSq = round(unlist(predict(RegressionSq, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqWinter = round(unlist(predict(RegressionSqWinter, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqSpring = round(unlist(predict(RegressionSqSpring, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqSummer = round(unlist(predict(RegressionSqSummer, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqFall = round(unlist(predict(RegressionSqFall, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqCode1 = round(unlist(predict(RegressionSqCode1, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqCode2 = round(unlist(predict(RegressionSqCode2, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqCode3 = round(unlist(predict(RegressionSqCode3, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqCode4 = round(unlist(predict(RegressionSqCode4, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqCode5 = round(unlist(predict(RegressionSqCode5, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstSqCode6 = round(unlist(predict(RegressionSqCode6, type = "response", se.fit = FALSE), use.names = FALSE),3)



#Write to file
write.csv(data.frame(xCor, yCor, ProbEstSq,ProbEstSqWinter, ProbEstSqSpring,ProbEstSqSummer, ProbEstSqFall), file = "SlopeSeason.csv")
write.csv(data.frame(xCor, yCor, ProbEstSqCode1,ProbEstSqCode2, ProbEstSqCode3,ProbEstSqCode3, ProbEstSqCode4, ProbEstSqCode5, ProbEstSqCode6), file = "SlopeCode.csv")
#Coefficients
#Regression  --> Non Squares
riverDistCoeff1 = as.numeric(coef(Regression)["riverDist"])
georgeDistCoeff1 = as.numeric(coef(Regression)["sqrt(georgeDistSq)"])
portDistCoeff1 = as.numeric(coef(Regression)["sqrt(portDistSq)"])
histAleDistCoeff1 = as.numeric(coef(Regression)["histAleDist"])
slopeCoeff1 = as.numeric(coef(Regression)["slope"])
depthCoeff1 = as.numeric(coef(Regression)["depth"])
constantCoeff1 = as.numeric(coef(Regression)["(Intercept)"])


par(mfrow = c(2,2))
hist(ProbEstSqWinter, breaks = 100, main = "Winter Likelihood Estimates", xlab = "Winter Estimates")
hist(ProbEstSqSpring, breaks = 100, main = "Spring Likelihood Estimates",xlab = "Spring Estimates")
hist(ProbEstSqSummer, breaks = 100, main = "Summer Likelihood Estimates",xlab = "Summer Estimates")
hist(ProbEstSqFall, breaks = 100, main = "Fall Likelihood Estimates",xlab = "Fall Estimates")

par(mfrow = c(3,2))
hist(ProbEstSqCode1, breaks = 100, main = "Code1 Likelihood Estimates",xlab = "Code1 Estimates")
hist(ProbEstSqCode2, breaks = 100, main = "Code2 Likelihood Estimates",xlab = "Code2 Estimates")
hist(ProbEstSqCode3, breaks = 100, main = "Code3 Likelihood Estimates",xlab = "Code3 Estimates")
hist(ProbEstSqCode4, breaks = 100, main = "Code4 Likelihood Estimates",xlab = "Code4 Estimates")
hist(ProbEstSqCode5, breaks = 100, main = "Code5 Likelihood Estimates",xlab = "Code5 Estimates")
hist(ProbEstSqCode6, breaks = 100, main = "Code6 Likelihood Estimates",xlab = "Code6 Estimates")




#Psuedo R-squared
library(pscl)
psuedoSq = pR2(RegressionSq)
psuedoSq.Winter = pR2(RegressionSqWinter)
psuedoSq.Summer = pR2(RegressionSqSummer)
psuedoSq.Spring = pR2(RegressionSqSpring)
psuedoSq.Fall = pR2(RegressionSqFall)
pseudoSqCode1 = pR2(RegressionSqCode1)
pseudoSqCode2 = pR2(RegressionSqCode2)
pseudoSqCode3 = pR2(RegressionSqCode3)
pseudoSqCode4 = pR2(RegressionSqCode4)
pseudoSqCode5 = pR2(RegressionSqCode5)
pseudoSqCode6 = pR2(RegressionSqCode6)


###########SUBSAMPLE STUFF
########################## CREATING A SUBSAMPLE ##########################################

######
#400m#
######

n = length(xCor)#length of column
c = seq(1,n,2)

#sampling every alternate x-entry

ObjectIDAlt = ObjectID[c] 
depthAlt = depth[c]
slopeAlt = slope[c]
shoreDistAlt = shoreDist[c]
portDistAlt = portDist[c]
georgeDistAlt = georgeDist[c]
xCorAlt = xCor[c]
yCorAlt = yCor[c]
pointIDAlt = pointID[c]
histAleDistAlt = histAleDist[c]
winterCodAlt = winterCod[c]
springCodAlt = springCod[c]
summerCodAlt = summerCod[c]
fallCodAlt = fallCod[c]
riverDistAlt = riverDist[c]
fishGroundAlt = fishGround[c]
codeAlt = code[c]
winterCodPAlt = winterCodP[c]
springCodPAlt = springCodP[c]
summerCodPAlt = summerCodP[c]
fallCodPAlt = fallCodP[c]

dataAlt = data.frame(ObjectIDAlt,depthAlt,slopeAlt,shoreDistAlt,portDistAlt,georgeDistAlt,xCorAlt,yCorAlt,pointIDAlt,histAleDistAlt,winterCodAlt,springCodAlt,summerCodAlt,fallCodAlt,riverDistAlt,fishGroundAlt,codeAlt,winterCodPAlt,springCodPAlt,summerCodAlt,fallCodAlt)
uni = unique(yCorAlt)
uni = uni[seq(1,length(yCor)-1,2)]
dataSub = subset(dataAlt,yCorAlt %in% uni)

ObjectIDSub = dataSub[,1]
depthSub = dataSub[,2]
slopeSub = dataSub[,3]
shoreDistSub = dataSub[,4]
portDistSub = dataSub[,5]
georgeDistSub = dataSub[,6]
xCorSub = dataSub[,7]
yCorSub = dataSub[,8]
pointIDSub = dataSub[,9]
histAleDistSub = dataSub[,10]
winterCodSub = dataSub[,11]
springCodSub = dataSub[,12]
summerCodSub = dataSub[,13]
fallCodSub = dataSub[,14]
riverDistSub = dataSub[,15]
fishGroundSub = dataSub[,16]
codeSub = dataSub[,17]
winterCodPSub = dataSub[,18]
springCodPSub = dataSub[,19]
summerCodPSub = dataSub[,20]
fallCodPSub = dataSub[,21]

#Converting to meters
shoreDistSub = shoreDistSub/1000
portDistSub = portDistSub/1000
georgeDistSub = georgeDistSub/1000
histAleDistSub = histAleDistSub/1000
riverDistSub = riverDistSub/1000

#Squaring the values

depthSubSq = depthSub^2
slopeSubSq = slopeSub^2
shoreDistSubSq = shoreDistSub^2
portDistSubSq = portDistSub^2
georgeDistSubSq = georgeDistSub^2
histAleDistSubSq = histAleDistSub^2
riverDistSubSq = riverDistSub^2


######
#800m#
######

n = length(xCor)#length of column
c = seq(1,n,4)

#sampling every alternate x-entry

ObjectIDAlt800 = ObjectID[c] 
depthAlt800 = depth[c]
slopeAlt800 = slope[c]
shoreDistAlt800 = shoreDist[c]
portDistAlt800 = portDist[c]
georgeDistAlt800 = georgeDist[c]
xCorAlt800 = xCor[c]
yCorAlt800 = yCor[c]
pointIDAlt800 = pointID[c]
histAleDistAlt800 = histAleDist[c]
winterCodAlt800 = winterCod[c]
springCodAlt800 = springCod[c]
summerCodAlt800 = summerCod[c]
fallCodAlt800 = fallCod[c]
riverDistAlt800 = riverDist[c]
fishGroundAlt800 = fishGround[c]
codeAlt800 = code[c]
winterCodPAlt800 = winterCodP[c]
springCodPAlt800 = springCodP[c]
summerCodPAlt800 = summerCodP[c]
fallCodPAlt800 = fallCodP[c]

dataAlt800 = data.frame(ObjectIDAlt800,depthAlt800,slopeAlt800,shoreDistAlt800,portDistAlt800,georgeDistAlt800,xCorAlt800,yCorAlt800,pointIDAlt800,histAleDistAlt800,winterCodAlt800,springCodAlt800,summerCodAlt800,fallCodAlt800,riverDistAlt800,fishGroundAlt800,codeAlt800,winterCodPAlt800,springCodPAlt800,summerCodAlt800,fallCodAlt800)
uni = unique(yCorAlt800)
uni = uni[seq(1,length(yCor)-1,4)]
dataSub800 = subset(dataAlt800,yCorAlt800 %in% uni)

ObjectIDSub800 = dataSub800[,1]
depthSub800 = dataSub800[,2]
slopeSub800 = dataSub800[,3]
shoreDistSub800 = dataSub800[,4]
portDistSub800 = dataSub800[,5]
georgeDistSub800 = dataSub800[,6]
xCorSub800 = dataSub800[,7]
yCorSub800 = dataSub800[,8]
pointIDSub800 = dataSub800[,9]
histAleDistSub800 = dataSub800[,10]
winterCodSub800 = dataSub800[,11]
springCodSub800 = dataSub800[,12]
summerCodSub800 = dataSub800[,13]
fallCodSub800 = dataSub800[,14]
riverDistSub800 = dataSub800[,15]
fishGroundSub800 = dataSub800[,16]
codeSub800 = dataSub800[,17]
winterCodPSub800 = dataSub800[,18]
springCodPSub800 = dataSub800[,19]
summerCodPSub800 = dataSub800[,20]
fallCodPSub800 = dataSub800[,21]

#Converting to meters
shoreDistSub800 = shoreDistSub800/1000
portDistSub800 = portDistSub800/1000
georgeDistSub800 = georgeDistSub800/1000
histAleDistSub800 = histAleDistSub800/1000
riverDistSub800 = riverDistSub800/1000

#Squaring the values

depthSubSq800 = depthSub800^2
slopeSubSq800 = slopeSub800^2
shoreDistSubSq800 = shoreDistSub800^2
portDistSubSq800 = portDistSub800^2
georgeDistSubSq800 = georgeDistSub800^2
histAleDistSubSq800 = histAleDistSub800^2
riverDistSubSq = riverDistSub^2

############
#SubDummies#
############

#WinterCod
num.iter = length(winterCodSub)

fishGroundWinterSub = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(winterCodSub[i] > 0 )
  {
    fishGroundWinterSub[i] = 1
  }
}

#SpringCod
num.iter = length(winterCodSub)

fishGroundSpringSub = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(springCodSub[i] > 0 )
  {
    fishGroundSpringSub[i] = 1
  }
}


##Summer Cod
num.iter = length(winterCodSub)

fishGroundSummerSub = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(summerCodSub[i] > 0 )
  {
    fishGroundSummerSub[i] = 1
  }
}

#FallCod
num.iter = length(winterCodSub)

fishGroundFallSub = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(fallCodSub[i] > 0 )
  {
    fishGroundFallSub[i] = 1
  }
}

###For 800m Apart

#WinterCod
num.iter = length(winterCodSub800)

fishGroundWinterSub800 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(winterCodSub800[i] > 0 )
  {
    fishGroundWinterSub800[i] = 1
  }
}

#SpringCod
num.iter = length(winterCodSub800)

fishGroundSpringSub800 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(springCodSub800[i] > 0 )
  {
    fishGroundSpringSub800[i] = 1
  }
}


##Summer Cod
num.iter = length(winterCodSub800)

fishGroundSummerSub800 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(summerCodSub800[i] > 0 )
  {
    fishGroundSummerSub800[i] = 1
  }
}

#FallCod
num.iter = length(winterCodSub800)

fishGroundFallSub800 = rep(0,num.iter)

for(i in 1:num.iter)
{
  if(fallCodSub800[i] > 0 )
  {
    fishGroundFallSub800[i] = 1
  }
}


#Regressions on subsample
Regression = glm(fishGroundSub ~ riverDistSub + sqrt(georgeDistSqSub) + sqrt(portDistSqSub) + histAleDistSub +  sqrt(slopeSqSub) + sqrt(depthSqSub), family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSq400 = glm(fishGroundSummerSub ~ sqrt(riverDistSqSub) + riverDistSqSub + sqrt(georgeDistSqSub) + georgeDistSqSub + sqrt(portDistSqSub) + portDistSqSub + histAleDistSub + histAleDistSqSub + sqrt(slopeSqSub) + slopeSqSub + sqrt(depthSqSub) + depthSqSub,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqWinter400 = glm(fishGroundWinterSub ~ sqrt(riverDistSqSub) + riverDistSqSub + sqrt(georgeDistSqSub) + georgeDistSqSub + sqrt(portDistSqSub) + portDistSqSub + histAleDistSub + histAleDistSqSub + sqrt(slopeSqSub) + slopeSqSub + sqrt(depthSqSub) + depthSqSub,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqSpring400 = glm(fishGroundSpringSub ~ sqrt(riverDistSqSub) + riverDistSqSub + sqrt(georgeDistSqSub) + georgeDistSqSub + sqrt(portDistSqSub) + portDistSqSub + histAleDistSub + histAleDistSqSub + sqrt(slopeSqSub) + slopeSqSub + sqrt(depthSqSub) + depthSqSub,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqSummer400 = glm(fishGroundSummerSub ~ sqrt(riverDistSqSub) + riverDistSqSub + sqrt(georgeDistSqSub) + georgeDistSqSub + sqrt(portDistSqSub) + portDistSqSub + histAleDistSub + histAleDistSqSub + sqrt(slopeSqSub) + slopeSqSub + sqrt(depthSqSub) + depthSqSub,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqFall400 = glm(fishGroundFallSub ~ sqrt(riverDistSqSub) + riverDistSqSub + sqrt(georgeDistSqSub) + georgeDistSqSub + sqrt(portDistSqSub) + portDistSqSub + histAleDistSub + histAleDistSqSub + sqrt(slopeSqSub) + slopeSqSub + sqrt(depthSqSub) + depthSqSub,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

#Slope Estimates

ProbEstSqSub400 = unlist(predict(RegressionSq400, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqWinterSub400 = unlist(predict(RegressionSqWinter400, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqSpringSub400 = unlist(predict(RegressionSqSpring400, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqSummerSub400 = unlist(predict(RegressionSqSummer400, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqFallSub400 = unlist(predict(RegressionSqFall400, type = "response", se.fit = FALSE), use.names = FALSE)

#800mRegression
Regression = glm(fishGroundSub800 ~ riverDistSub + sqrt(georgeDistSqSub800) + sqrt(portDistSqSub800) + histAleDistSub800 +  sqrt(slopeSqSub800) + sqrt(depthSqSub800), family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSq800 = glm(fishGroundSummerSub800 ~ sqrt(riverDistSqSub800) + riverDistSqSub800 + sqrt(georgeDistSqSub800) + georgeDistSqSub800 + sqrt(portDistSqSub800) + portDistSqSub800 + histAleDistSub800 + histAleDistSqSub800 + sqrt(slopeSqSub800) + slopeSqSub800 + sqrt(depthSqSub800) + depthSqSub800,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqWinter800 = glm(fishGroundWinterSub800 ~ sqrt(riverDistSqSub800) + riverDistSqSub800 + sqrt(georgeDistSqSub800) + georgeDistSqSub800 + sqrt(portDistSqSub800) + portDistSqSub800 + histAleDistSub800 + histAleDistSqSub800 + sqrt(slopeSqSub800) + slopeSqSub800 + sqrt(depthSqSub800) + depthSqSub800,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqSpring800 = glm(fishGroundSpringSub800 ~ sqrt(riverDistSqSub800) + riverDistSqSub800 + sqrt(georgeDistSqSub800) + georgeDistSqSub800 + sqrt(portDistSqSub800) + portDistSqSub800 + histAleDistSub800 + histAleDistSqSub800 + sqrt(slopeSqSub800) + slopeSqSub800 + sqrt(depthSqSub800) + depthSqSub800,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqSummer800 = glm(fishGroundSummerSub800 ~ sqrt(riverDistSqSub800) + riverDistSqSub800 + sqrt(georgeDistSqSub800) + georgeDistSqSub800 + sqrt(portDistSqSub800) + portDistSqSub800 + histAleDistSub800 + histAleDistSqSub800 + sqrt(slopeSqSub800) + slopeSqSub800 + sqrt(depthSqSub800) + depthSqSub800,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

RegressionSqFall800 = glm(fishGroundFallSub800 ~ sqrt(riverDistSqSub800) + riverDistSqSub800 + sqrt(georgeDistSqSub800) + georgeDistSqSub800 + sqrt(portDistSqSub800) + portDistSqSub800 + histAleDistSub800 + histAleDistSqSub800 + sqrt(slopeSqSub800) + slopeSqSub800 + sqrt(depthSqSub800) + depthSqSub800,  family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")

#Slope Estimates

ProbEstSq800 = unlist(predict(RegressionSq800, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqWinter800 = unlist(predict(RegressionSqWinter800, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqSpring800 = unlist(predict(RegressionSqSpring800, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqSummer800 = unlist(predict(RegressionSqSummer800, type = "response", se.fit = FALSE), use.names = FALSE)
ProbEstSqFall800 = unlist(predict(RegressionSqFall800, type = "response", se.fit = FALSE), use.names = FALSE)


#Checking for Heteroscedasticity 
#Which subsample entries are in the main sample
subMain = intersect(ObjectIDSub, ObjectID)
sub800Main = intersect(ObjectIDSub800,ObjectID)

Reg1 = lm(ProbEstSq400~ProbEstSq[subMain])
Slope1 = as.numeric(coef(Reg1)["ProbEstSq[subMain]"])
Intercept1= as.numeric(coef(Reg1)["(Intercept)"])
Pred.ProbEstSq.sub = unlist(predict(Reg1, type = "response", se.fit = FALSE), use.names = FALSE)
E_i.hat.1 = Pred.ProbEstSq.sub - ProbEstSq.sub

##############################################################################################################################################################################################################################################################################################################################################################################################

##############
#RandomForest#
##############

library(randomForest)

fit = randomForest(fishGround ~ riverDist + riverDistSq + sqrt(georgeDistSq) + georgeDistSq + sqrt(portDistSq) + portDistSq + histAleDist + histAleDistSq + sqrt(slopeSq) + slopeSq + sqrt(depthSq) + depthSq, data = data.set, family = binomial(link = "logit"),control = list(maxit = 100), na.action = na.fail, importance = TRUE, proximity = TRUE, replace = TRUE)
varImpPlot(fit,type=1)


#############
#   PLOTS   #
#############
#LINKS: https://github.com/Pakillo/R-GIS-tutorial/blob/master/R-GIS_tutorial.md 

install.packages("dismo")
library(dismo)
locs = data.frame(xCor,yCor)
coordinates(locs) = c("xCor","yCor")
plot(locs)


################
#  Rpart       #
################
library(rpart)
library(rpart.plot)

# create train and test sets (~40% for training, ~60% for testing)
data.set$randu <- runif(nrow(data.set),0,1)
data.set.train <- data.set[data.set$randu < .4,]
data.set.test <- data.set[data.set$randu >= .6,]

fit1 = rpart(as.factor(fishGround) ~ riverDist + shoreDist +  portDist + histAleDist +  sqrt(slopeSq) + sqrt(depthSq), data = data.set.train)
rpart.plot(fit1, type= 0, extra = 2)

fitWinter = rpart(as.factor(fishGroundWinter) ~ riverDist + shoreDist + sqrt(georgeDistSq) + sqrt(portDistSq) + histAleDist +  sqrt(slopeSq) + sqrt(depthSq), data = data.set)
rpart.plot(fitWinter, type= 0, extra = 2)

fitSpring = rpart(as.factor(fishGroundSpring) ~ riverDist + shoreDist + sqrt(georgeDistSq) + sqrt(portDistSq) + histAleDist +  slope + Depth, data = data.set)
rpart.plot(fitSummer, type= 0, extra = 2)

fitSummer = rpart(as.factor(fishGroundSummer)  ~ riverDist + shoreDist + sqrt(georgeDistSq) + sqrt(portDistSq) + histAleDist +  slope + Depth, data = data.set)
rpart.plot(fitSummer, type= 0, extra = 2)

fitFall = rpart(as.factor(fishGroundFall) ~ ~ riverDist + shoreDist + sqrt(georgeDistSq) + sqrt(portDistSq) + histAleDist +  slope + Depth, data = data.set)
rpart.plot(fitFall, type= 0, extra = 2)


################
#BOOTSTRAP CODE#
################

#Bootstrapping the standard errors
logit.bootstrap <- function(data, indices)
{
  data.boot = data[indices, ]
  fit = glm(FishGround ~ RiverDistSq + GeorgeDistSq + PortDistSq + HistAleDistSq +  SlopeSq + DepthSq, data = data.boot, family = binomial(link = "logit"),control = list(maxit = 100), na.action = "na.exclude")
  return(coef(fit))
}

set.seed(12345) # seed for the RNG to ensure that you get exactly the same results as here

logit.boot <- boot(data=data.set, statistic=logit.bootstrap, R=100) # 10'000 samples

logit.boot

#Alternative
library(boot)
#Boot(Regression, R = 200)