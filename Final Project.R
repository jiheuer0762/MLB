

MLB <- read.table("C:/Users/JHeuer/Documents/Jacey/Business Analytics Courses/STAT 526/Final Project/Option 1/MLB_Data.csv", head = T,sep = ",", quote = "/")
summary(MLB)
head(MLB,5)

plot(MLB)

MLB.2 <- MLB[,-1:-5]
head(MLB.2,5)
summary(MLB.2)
plot(MLB.2)

MLB.3 <- MLB.2[,-9:-10]
head(MLB.3,5)
plot(MLB.3)
summary(MLB.3)
MLB.3.1 <- cbind(MLB.3[,1:6],MLB.3[,8:10])
cor(MLB.3.1)

lower.mod <- lm(salary~1, data = MLB.3)
upper.mod <- lm(salary~., data = MLB.3)

summary(lower.mod)
summary(upper.mod)

library(leaps)
library(MASS)

step.MLB.3 <- stepAIC(lower.mod,scope = list(lower = lower.mod,upper=upper.mod), direction = "forward")
step.MLB.3$anova
step.MLB.3$coefficients

final.mod <- lm(salary ~ POS + RBI, data = MLB.3)
#final.mod <- lm(salary ~ HR + POS + Fielding_G + E, data = MLB.3)
summary(final.mod)
library(car)
vif(final.mod)


#log-cubed approach
plot(I(MLB.3$Batting_G^(1/3)),log(MLB.3$salary))
plot(I(MLB.3$AB^(1/3)),log(MLB.3$salary))
plot(I(MLB.3$R^(1/3)),log(MLB.3$salary))
plot(I(MLB.3$H^(1/3)),log(MLB.3$salary))
plot(I(MLB.3$HR^(1/3)),log(MLB.3$salary))
plot(I(MLB.3$RBI^(1/3)),log(MLB.3$salary))
plot(I(MLB.3$Fielding_G^(1/3)),log(MLB.3$salary))
plot(I(MLB.3$E^(1/3)),log(MLB.3$salary))

#,MLB.3$POS    ,"POS"
MLB.4 <- cbind(log(MLB.3$salary),I(MLB.3$Batting_G^(1/3)),I(MLB.3$AB^(1/3)),I(MLB.3$R^(1/3)),I(MLB.3$H^(1/3)),I(MLB.3$HR^(1/3)),I(MLB.3$RBI^(1/3)),I(MLB.3$Fielding_G^(1/3)),I(MLB.3$E^(1/3)))
colnames(MLB.4) <- c("log Salary","Cubed Batting_G","Cubed AB","Cubed R","Cubed H","Cubed HR","Cubed RBI","Cubed Fielding_G","Cubed E")
summary(MLB.4)
MLB.4 <- data.frame(MLB.4)
cor(MLB.4)
hist(MLB$salary)

#full
full.mod <- lm(log.Salary~., data = MLB.4)
summary(full.mod)
vif(full.mod)
AIC(full.mod)
plot(full.mod)

#forward
lower.mod <- lm(log.Salary~1, data = MLB.4)
upper.mod <- lm(log.Salary~., data = MLB.4)

#summary(lower.mod)
#summary(upper.mod)
step.MLB.4 <- stepAIC(lower.mod,scope = list(lower = lower.mod,upper=upper.mod), direction = "forward")
step.MLB.4$anova
step.MLB.4$coefficients

final.mod.For <- lm(log.Salary ~ Cubed.RBI + Cubed.E, data = MLB.4)
summary(final.mod.For)
vif(final.mod.For)
AIC(final.mod.For)
plot(final.mod.For)
#confint(final.mod.For)


#backwards
lower.mod <- lm(log.Salary~1, data = MLB.4)
upper.mod <- lm(log.Salary~., data = MLB.4)

#summary(lower.mod)
#summary(upper.mod)
step.MLB.4 <- stepAIC(upper.mod,scope = list(upper=upper.mod, lower = lower.mod), direction = "backward")
step.MLB.4$anova
step.MLB.4$coefficients

final.mod.Bac <- lm(log.Salary ~ Cubed.RBI + Cubed.E, data = MLB.4)
summary(final.mod.Bac)
vif(final.mod.Bac)
AIC(final.mod.Bac)
plot(final.mod.Bac)
confint(final.mod.Bac)


#mixed
lower.mod <- lm(log.Salary~1, data = MLB.4)
upper.mod <- lm(log.Salary~., data = MLB.4)

#summary(lower.mod)
#summary(upper.mod)
step.MLB.4 <- stepAIC(lower.mod,scope = list(lower = lower.mod,upper=upper.mod), direction = "both")
step.MLB.4$anova
step.MLB.4$coefficients

final.mod.Both <- lm(log.Salary ~ Cubed.RBI + Cubed.E, data = MLB.4)
summary(final.mod.Both)
vif(final.mod.Both)
AIC(final.mod.Both)
plot(final.mod.Both)
confint(final.mod.Both)


#All methods
MLB.sub.All<-regsubsets(log.Salary~ Cubed.Batting_G+Cubed.AB+Cubed.R+Cubed.H+Cubed.HR+Cubed.RBI+Cubed.Fielding_G+Cubed.E,data= MLB.4,nbest=1,nvmax = 9)
summary(MLB.sub.All)
plot(MLB.sub.All, scale = "bic")
final.mod.All <- lm(log.Salary ~ Cubed.RBI + Cubed.E, data = MLB.4)
summary(final.mod.All)
vif(final.mod.All)
AIC(final.mod.All)
plot(final.mod.All)

summary(MLB.4)

library(effects)
plot(allEffects(final.mod),multiline=TRUE)

###########################leverage and cooks distance##########################################################################################

#####################Final Mod Residuals studentized#######################
#Studentized Residuals (take into account that the variance of each residual is not the same)
rstu  <-  rstudent(final.mod.For)
plot(rstu~final.mod.For$fitted,ylab="Studentized Residauls", xlab="Final Mod Resid Fitted",col=0)
abline(a = -2,b=0,lty=2)
abline(a = 2,b=0,lty=2)
outlier<-(abs(rstudent(final.mod.For)))>2
points(rstu[outlier]~final.mod.For$fitted[outlier],col = 2,pch = 19)
notoutlier <-(abs(rstudent(final.mod.For)))<2
points(rstu[notoutlier]~final.mod.For$fitted[notoutlier],pch = 19)

#indentify the 5 largest residuals
tbl.outlier <- data.frame(outlier)
#tbl.outlier
tbl.rstu <- data.frame(rstu)
#tbl.rstu
tbl.rstu_outlier <- cbind(tbl.outlier,abs(tbl.rstu),final.mod.For$fitted)
tbl.rstu_outlier <- tbl.rstu_outlier[order(-abs(rstu)),]
#tbl.rstu_outlier
head(tbl.rstu_outlier,5)

2*(4+1)/415 
### Scatterplot of Standardized Residuals --- ####################&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%
### Red points Outliers

n=415
k=4
RMSE <- sigma(final.model)
outlier <- (abs(summary(final.model)$residuals/RMSE))>2
plot(final.model$fitted,summary(final.model)$residuals/RMSE,ylim = c(-5,5),pch = 19, ylab = "Standardized Residuals", xlab = "Mod1 Fitted",col = 0)
abline(a = -2,b=0,lty=2)
abline(a = 2,b=0,lty=2)
outlier <- (abs(summary(final.model)$residuals/RMSE))>2
points((summary(final.model)$residuals/RMSE)[outlier]~final.model$fitted[outlier],col = 2,pch = 19)
notoutlier <- (abs(summary(final.model)$residuals/RMSE))<2
points((summary(final.model)$residuals/RMSE)[notoutlier]~final.model$fitted[notoutlier],pch = 19)

#indentify the 5 largest residuals
outlier1 <- abs(summary(final.model)$residuals/RMSE)
outlier1 <- data.frame(outlier1)
#outlier1
tbl.outlier_outlier1 <- cbind(outlier,outlier1,final.model$residuals)
tbl.outlier_outlier1 <- tbl.outlier_outlier1[order(-abs(outlier1)),]
#tbl.outlier_outlier1
head(tbl.outlier_outlier1,5)

### Investigate Points with High Leverage
#plot(1:n,hatvalues(final.mod.For),pch=16,cex=1,xlab="Index",ylab="Hat Values")
#abline(h=2*(k+1)/n,lty=2)
#abline(h=3*(k+1)/n,lty=2)
#Rule of thumb is 2*average hat value or 3*average hat value are points with high leverage.
#How to Identify Points Interactively
#identify(1:n,hatvalues(mod1),row.names(Boston))
#hit Esc once all points identified

tbl.hatvalues <- data.frame(hatvalues(final.model))
tbl.hatvalues

lev<-c(hatvalues(final.model))
tbl.hatvalues <- data.frame(hatvalues(final.model))
colnames(tbl.hatvalues) <- c("hatvalues")
tbl.hatvalues
tbl.hatvalues <- order(hatvalues)
head(tbl.hatvalues,20)

### Plot of Standardized Residuals vs Leverage --- 
### Red points Outliers, Blue Pointes high Leverage, Green Points Both
plot(c(5,summary(final.model)$residuals/RMSE)~c(.1,hatvalues(final.model)),
     pch = 19, ylab = "Standardized Residuals", 
     xlab = "Leverage",col = 0, ylim = c(-5,5))
abline(a = -2,b=0 , col = 1)
abline(a = 2,b=0 , col = 1)
outlier <- (abs(summary(final.model)$residuals/RMSE))>2
points((summary(final.model)$residuals/RMSE)[outlier]~hatvalues(final.model)[outlier],col = 2,pch = 19)
notoutlier <- (abs(summary(final.model)$residuals/RMSE))<2
points((summary(final.model)$residuals/RMSE)[notoutlier]~hatvalues(final.model)[notoutlier],pch = 19)
lev <- c(hatvalues(final.model))> 2*(k+1)/n
points((summary(final.model)$residuals/RMSE)[lev]~hatvalues(final.model)[lev],pch = 19,col = 5)
abline(v = 2*(k+1)/n,col = 5) #Line to Identify high leverage obs
un<- (abs(summary(final.model)$residuals/RMSE))>3&c(hatvalues(final.model))> 2*(k+1)/n
points((summary(final.model)$residuals/RMSE)[un]~hatvalues(final.model)[un],pch = 19,col = 3)
tbl.hatvalues <- data.frame(hatvalues(final.model))
tbl.hatvalues

hatvalues(final.model)> 2*(k+1)/n

#Cook's Distance--Effect of Deleting an Individual Observation on the Model Fit
cd <- cooks.distance(final.model)
plot(cd, ylab='Cook Distance', xlab='Observation number',pch=19)
#Typically a Cook's Distance greater than 1 is significan
tbl.cd <- data.frame(cd)
tbl.cd1 <- tbl.cd
#tbl.cd1
colnames(tbl.cd1)[1] <- "cd"
tbl.seq <- seq(1:415)
tbl.seq <- data.frame(tbl.seq)
colnames(tbl.seq)[1] <- "obs"
#tbl.seq
tbl.cd2 <- cbind(tbl.cd1,tbl.seq)
tbl.cd3 <- tbl.cd2[order(-cd),]
head(data.frame(tbl.cd3),5)


####################################################################################
MLB.45 <- cbind(log(MLB.3$salary),MLB.3$Batting_G,MLB.3$AB,MLB.3$R,MLB.3$H,MLB.3$HR,MLB.3$RBI,MLB.3$Fielding_G,MLB.3$E)
colnames(MLB.45) <- c("log Salary","Batting_G","AB","R","H","HR","RBI","Fielding_G","E")
summary(MLB.45)
MLB.45 <- data.frame(MLB.45)
cor(MLB.4)
hist(MLB$salary)

#full
full.mod <- lm(log.Salary~ I(MLB.3$Batting_G^(1/3))+I(MLB.3$AB^(1/3))+I(MLB.3$R^(1/3))+I(MLB.3$H^(1/3))+I(MLB.3$HR^(1/3))+I(MLB.3$RBI^(1/3))+I(MLB.3$Fielding_G^(1/3))+I(MLB.3$E^(1/3)), data = MLB.45)
summary(full.mod)
vif(full.mod)
AIC(full.mod)
plot(full.mod)

#forward
lower.mod <- lm(log.Salary~1, data = MLB.4)
upper.mod <- lm(log.Salary~., data = MLB.4)

#summary(lower.mod)
#summary(upper.mod)
step.MLB.4 <- stepAIC(lower.mod,scope = list(lower = lower.mod,upper=upper.mod), direction = "forward")
step.MLB.4$anova
step.MLB.4$coefficients

#Final Model
final.model <- lm(log.Salary ~ MLB.3$RBI+MLB.3$E+I(MLB.3$RBI^(1/3)) + I(MLB.3$E^(1/3)), data = MLB.45)
summary(final.model)
vif(final.model)
AIC(final.model)
plot(final.model)
#confint(final.mod.For)