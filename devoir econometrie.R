#####Multinomial Regression : Covid-19
library(readxl) 
library(nnet)
library(ggplot2) 
library(mlogit) 
library(stargazer)
# Importations des donnees 
covid_19 <- read_excel("/Users/amado/Downloads/covid19.xlsx")
covid_19
covid_19$TransmittingWay[covid_19$TransmittingWay %in% c("Information pending")]<-
  "contact of a confirmed case" 
covid_19$TransmittingWay[covid_19$TransmittingWay %in% c("Neither")]<-"Unkown"
covid_19$TransmittingWay[covid_19$TransmittingWay %in% c("Travel related")]<-"Travel Related" 

# Modelisation multinominal Logit 
mydata.logit = mlogit.data(covid_19, choice = "Outcome",shape = "wide",alt.levels = c(1,2,3))
mydata.logit
mnl.regression = mlogit(Outcome ~ 0 | Age + Gender, data = mydata.mnl, reflevel = 1 )
summary(mnl.regression)

# Predicted probabilities (1st syntax) 
n = NROW(covid_19)	# number of observations 
pp.logit = fitted(mnl.regression, outcome = FALSE) 
head(pp.logit)
pp.success = array(NA, c(n ,3) )	# good prediction = 1, bad prediction = 0 
for (i in 1:n) {
  pp.success[i,1] = ( covid_19 $Outcome[i]==1 & pp.mnl[i,1]==max(pp.logit[i,]))
  pp.success[i,2] = ( covid_19 $Outcome[i]==2 & pp.mnl[i,2]==max(pp.logit[i,])) 
  pp.success[i,3] = ( covid_19 $Outcome[i]==3 & pp.mnl[i,3]==max(pp.logit[i,]))
}
print("percent predicted,LOGIT"); sum(pp.success)/n

# Predicted probabilities (2nd syntax)
pp.success2 = rep(NA,n)	# predicted status (1, 2 or 3) 
for (i in 1:n) {
  if ( pp.logit[i,1]==max(pp.logit[i,]) ) { pp.success2[i] = 1 } 
  if ( pp.logit[i,2]==max(pp.logit[i,]) ) { pp.success2[i] = 2 } 
  if ( pp.logit[i,3]==max(pp.logit[i,]) ) { pp.success2[i] = 3 }
}
with(covid_19, table(covid_19 $Outcome ,pp.success2)) # Observed (status) versus Predicted (pp.success2)
print("Percent correctly predicted"); sum(diag(with(covid_19, table(Outcome,pp.success2)) ))/n

# Modelisation multionomial regression 
covid_19$OutcomeF <- factor(covid_19 $Outcome)
covid_19$OutcomeF
###conversion en variable categorique
covid_19$out <- relevel(covid_19$OutcomeF, ref = 1) ###la reference est le patient saint (1)
modelisation <- multinom(out ~ Age + Gender, data = covid_19)
summary(modelisation)

# Prediction 
k=predict(modelisation, data = covid_19, type = "prob")
k

# Comparaison 
predict(modelisation,dimen=1)
confusionmatrix=table(predict(modelisation),covid_19$OutcomeF)
print(confusionmatrix)
1-sum(diag(confusionmatrix))/sum(confusionmatrix) 
sum (pp.success)/n +1- sum (diag(confusionmatrix))/sum (confusionmatrix)

# Two tail Z-Test
z_test <- summary(modelisation) $coefficients/summary(modelisation) $standard.errors 
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Graphs 
v=ggplot(covid_19,aes(x = Age, fill = OutcomeF))+ ylab("Density") + geom_density(
  alpha = 0.6)+scale_fill_manual(values = c(scales::alpha( "green" ,.3),scales::alpha( "white" ,.3), "skyblue" )) + theme(axis.text.y = element_blank(), axis.ticks =
                                                                                                                            element_blank())
v
o=ggplot(covid_19, aes(x = Age, fill=TransmittingWay))+ geom_density(alpha =0.8)+
  ylab("Density") +scale_fill_manual(values = c(scales::alpha( "red" ,.8),scales
                                                :: alpha ( "white" ,.5) , scales :: alpha ( "green" ,.5), "skyblue" ) ) + theme ( axis.text.y = element_blank () , axis.ticks =
                                                                                                                                       element_blank())
o



# Initialisation 
age <- c(10, 20, 30, 40, 50, 60, 70, 80, 90) 
death <- c(0, 0 ,0 ,0 ,0 ,0, 0, 0, 0)
womenDeathHist <- array(c(age,death), dim = c(9, 2))
womenAliveHist <- array(c(age,death), dim = c(9, 2))
menDeathHist <- array(c(age,death), dim = c(9, 2)) 
menAliveHist <- array(c(age,death), dim = c(9, 2)) 
deathHist <- array(c(age,death), dim = c(9, 2)) 
aliveHist <- array(c(age,death), dim = c(9, 2))

# Function 
for(i in 1:length(covid_19 $Age)) {
  if (covid_19 $Outcome[i] == 3) 
    deathHist[(covid_19 $Age[i])/10,2] = deathHist[(covid_19 $Age[i])/10,2] + 1
  else if (covid_19 $Outcome[i] == 1)
    aliveHist[(covid_19 $Age[i])/10,2] = aliveHist[(covid_19 $Age[i])/10,2] + 1
  
  if (covid_19 $Outcome[i] == 3 & covid_19 $Gender[i] == 0)
    womenDeathHist[(covid_19 $Age[i])/10,2] = womenDeathHist[(covid_19 $Age[i])
                                                             /10,2] + 1 
  else if ( covid_19 $ Outcome [ i ] == 1 & covid_19 $ Gender [ i ] == 0)
    womenAliveHist [( covid_19 $ Age [ i ])/10,2] = womenAliveHist [( covid_19 $ Age [ i ])/10,2] + 1
  else if (covid_19 $Outcome[i] == 3 & covid_19 $Gender[i] == 1) 
    menDeathHist[(covid_19 $Age[i])/10,2] = menDeathHist[(covid_19 $Age[i])/10,2] + 1
  else if (covid_19 $Outcome[i] == 1 & covid_19$Gender[i] == 1) 
    menAliveHist[(covid_19 $Age[i])/10,2] = menAliveHist[(covid_19 $Age[i])/10 ,2] + 1
}

#Plot 
barplot(aliveHist[,2], xlab = "Age", ylab = "Individus",names.arg = c(10,20,30,40,50,60,70,80,90), cex.names = 0.7,col ="skyblue" )
barplot(deathHist[,1], col=scales::alpha("White" ,.5), add=T) 
legend("topright", c("Alive","Dead"), fill = c(col="skyblue" ,col=scales::alpha( "White",.5)))

par(mfrow=c(1,2)) 
barplot(womenAliveHist[,2], xlab = "Age", ylab = "Male individu",names.arg = c(10,20,30,40,50,60,70,80,90), cex.names = 0.7,col= "skyblue" ) 
barplot(womenDeathHist[,1], col=scales::alpha( "White" ,.5), add=T) 
legend("topright", c("Alive","Dead"), fill = c( "skyblue" ,col=scales::alpha( "White" ,.4)
))

barplot(menAliveHist[,2], xlab = "Age", ylab = "Female individu",names.arg = c(10,20,30,40,50,60,70,80,90), cex.names = 0.7,col= "skyblue" ) 
barplot(menDeathHist[,1], col=scales::alpha( "White" ,.4), add=T) 
legend("topright", c("Alive","Dead"), fill = c( "skyblue" ,col=scales::alpha( "White" ,.5) 
))

