library(foreign)
data.set = read.dbf("sample_points_200m_all_data_10_16updated.dbf", as.is = FALSE)

code5 = which(data.set$code==5)
data5 = data.set[code5,]
data5 = data5[which(data5$depth<0),]
data5 = data5[which(data5$depth >-9999),]
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

###########SUBSAMPLE STUFF
########################## CREATING A SUBSAMPLE ##########################################

######
#400m#
######

n = length(data5$ale)#length of column
c = seq(1,n,2)

data5400 = data5[c,]
######
#800m#
######


c2 = seq(1,n,4)

data5800 = data5[c2,]
#Regressions on subsample
RegressionCode5 = glm(FishGround~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
RegressionSq400 = glm(FishGround~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5400, family= binomial(link = "logit"), na.action = "na.exclude")
RegressionSq800 = glm(FishGround~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5800, family= binomial(link = "logit"), na.action = "na.exclude")

ProbEstCode5 = round(unlist(predict(RegressionCode5, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstCode5400 = round(unlist(predict(RegressionSq400, type = "response", se.fit = FALSE), use.names = FALSE),3)
ProbEstCode5800 = round(unlist(predict(RegressionSq800, type = "response", se.fit = FALSE), use.names = FALSE),3)

#Checking for Heteroscedasticity 
#Which subsample entries are in the main sample
sub400 = intersect(data5$OBJECTID, data5400$OBJECTID)
sub800 = intersect(data5$OBJECTID,data5800$OBJECTID)

Reg1 = lm(ProbEstCode5400~ProbEstCode5[sub400])
Slope1 = as.numeric(coef(Reg1)["ProbEstCode5[sub400]"])
Intercept1= as.numeric(coef(Reg1)["(Intercept)"])
#Pred.ProbEstSq.400 = unlist(predict(Reg1, type = "response", se.fit = FALSE), use.names = FALSE)
#E_i.hat.1 = Pred.ProbEstSq.sub - ProbEstSq.sub


Reg2 = lm(ProbEstCode5800~ProbEstCode5[sub800])
Slope2 = as.numeric(coef(Reg2)["ProbEstCode5[sub800]"])
Intercept2= as.numeric(coef(Reg2)["(Intercept)"])
#Pred.ProbEstSq.400 = unlist(predict(Reg1, type = "response", se.fit = FALSE), use.names = FALSE)

par(mfrow= c(2,1))

plot = plot(ProbEstCode5400, ProbEstCode5[sub400],xlab = "MainSample200", ylab = "Subset400", main = " Sample 200 vs SubSample 400 m apart")
abline(a= 0,b =1, col ="Red")
abline(Intercept1, Slope1, col ="blue")
box()

plot = plot(ProbEstCode5800, ProbEstCode5[sub800],xlab = "MainSample200", ylab = "Subset800", main = "Sample 200 vs SubSample 800 m apart")
abline(a= 0,b =1, col ="Red")
abline(Intercept2, Slope2, col ="blue")
box()


##############################################################################################################################################################################################################################################################################################################################################################################################
