library(foreign)
data.set = read.dbf("sample_points_200m_all_data_10_16updated.dbf", as.is = FALSE)

code5 = which(data.set$code==5)
data5 = data.set[code5,]
data5 = data5[which(data5$depth<0),]
data5 = data5[which(data5$depth >-9999),]
river = data5$RiverDistC/1000
geor = data5$GeorgeDist/1000
port = data5$PortDist/1000
shore = data5$shoredista/1000
hist = data5$HistAleDis/1000
ale = data5$HistAleDis/1000


riverSq = river^2
georSq = geor^2
portSq = port^2
shoreSq = shore^2
histSq = hist^2
aleSq = ale^2
slopSq = (data5$slope)^2
depSq = (data5$depth)^2



RegressionCode5 = glm(data5$FishGround~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5)
write.csv(data.frame(summary(RegressionCode5)$coefficients[,1],summary(RegressionCode5)$coefficients[,2],summary(RegressionCode5)$coefficients[,3],summary(RegressionCode5)$coefficients[,4]), file= "Code5AnyResults.csv")

RegressionCode5Fall = glm(data5$FallCodPre~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5Fall)
write.csv(data.frame(summary(RegressionCode5Fall)$coefficients[,1],summary(RegressionCode5Fall)$coefficients[,2],summary(RegressionCode5Fall)$coefficients[,3],summary(RegressionCode5Fall)$coefficients[,4]), file= "Code5FallResults.csv")

RegressionCode5FallLag = glm(data5$FallCodPre~ SummerCodP + slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5FallLag)
write.csv(data.frame(summary(RegressionCode5FallLag)$coefficients[,1],summary(RegressionCode5FallLag)$coefficients[,2],summary(RegressionCode5FallLag)$coefficients[,3],summary(RegressionCode5FallLag)$coefficients[,4]), file= "Code5FallLagResults.csv")

RegressionCode5Winter = glm(data5$WinterCodP~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5Winter)
write.csv(data.frame(summary(RegressionCode5Winter)$coefficients[,1],summary(RegressionCode5Winter)$coefficients[,2],summary(RegressionCode5Winter)$coefficients[,3],summary(RegressionCode5Winter)$coefficients[,4]), file= "Code5WinterResults.csv")

RegressionCode5WinterLag = glm(data5$WinterCodP~ FallCodPre + slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5WinterLag)
write.csv(data.frame(summary(RegressionCode5WinterLag)$coefficients[,1],summary(RegressionCode5WinterLag)$coefficients[,2],summary(RegressionCode5WinterLag)$coefficients[,3],summary(RegressionCode5WinterLag)$coefficients[,4]), file= "Code5WinterLagResults.csv")

RegressionCode5Spring = glm(data5$SpringCodP~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5Spring)
write.csv(data.frame(summary(RegressionCode5Spring)$coefficients[,1],summary(RegressionCode5Spring)$coefficients[,2],summary(RegressionCode5Spring)$coefficients[,3],summary(RegressionCode5Spring)$coefficients[,4]), file= "Code5SpringResults.csv")

RegressionCode5SpringLag = glm(data5$SpringCodP~ WinterCodP + slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5SpringLag)
write.csv(data.frame(summary(RegressionCode5SpringLag)$coefficients[,1],summary(RegressionCode5SpringLag)$coefficients[,2],summary(RegressionCode5SpringLag)$coefficients[,3],summary(RegressionCode5SpringLag)$coefficients[,4]), file= "Code5SpringLagResults.csv")



RegressionCode5Summer = glm(data5$SummerCodP~ slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5Summer)
write.csv(data.frame(summary(RegressionCode5Summer)$coefficients[,1],summary(RegressionCode5Summer)$coefficients[,2],summary(RegressionCode5Summer)$coefficients[,3],summary(RegressionCode5Summer)$coefficients[,4]), file= "Code5SummerResults.csv")


RegressionCode5SummerLag = glm(data5$SummerCodP~ SpringCodP + slope + slopSq+ depth + depSq+ river+riverSq+geor+georSq+port+portSq+shore+shoreSq+hist+histSq, data=  data5, family= binomial(link = "logit"), na.action = "na.exclude")
summary(RegressionCode5SummerLag)
write.csv(data.frame(summary(RegressionCode5SummerLag)$coefficients[,1],summary(RegressionCode5SummerLag)$coefficients[,2],summary(RegressionCode5SummerLag)$coefficients[,3],summary(RegressionCode5SummerLag)$coefficients[,4]), file= "Code5SummerLagResults.csv")


ProbEstCode5 = round(unlist(predict(RegressionCode5, type = "response", se.fit = FALSE), use.names = FALSE),3)

write.csv(data.frame(data5$POINT_X,data5$POINT_Y, ProbEstCode5))

