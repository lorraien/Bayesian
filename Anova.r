   ##### DATA
low = c(91, 46, 95, 60, 33, 410, 105, 43, 189,1097, 54,178, 114, 137, 233, 101, 25,70,357)
normal = c(370, 267, 99,157, 75,1281, 48, 298, 268, 62,804,430,171,694,404)
high = c(75, 52, 1378, 555, 331, 231, 472, 263, 120, 46, 650, 349, 251, 492, 759,96, 627, 171, Diasorin <- c(low,normal,high)
logD <- log(Diasorin)
loghigh = log(high)
loglow = log(low)
lognormal = log(normal)
##### ANALYSIS
library(nortest)
g1 <- c(rep(1,length(low)),rep(2,length(normal)),rep(3,length(high))) g <- factor(g1, 1:3, c("Low","Normal","High"))
## Test for equal variance
bartlett.test(logD,g)
Bartlett test of homogeneity of variances
data: logD and g
Bartlett's K-squared = 1.0467, df = 2, p-value = 0.5925
## Boxplots and qq plot from least squares fit to ANOVA on log D
par(mfrow=c(1,2),pty="s")
boxplot(logD~g,range=0, ylab="Diasorin Score",lwd=1.5)
## ANOVA
fit <- aov(logD~g) summary(fit)

### equal variance
X.mat=model.matrix(~ g)
jags.data=list(Y=logD, Xmat=X.mat, n=19+15+50) model_diasorin <- "model
{
for(i in 1:n){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- beta[1] + beta[2]*Xmat[i,2]+beta[3]*Xmat[i,3]
}
beta[1] ~ dnorm(0, 0.00001) beta[2] ~ dnorm(0, 0.00001) beta[3] ~ dnorm(0, 0.00001) mu_g[1]=beta[1] mu_g[2]=beta[1]+beta[2] mu_g[3]=beta[1]+beta[3]
tau ~ dgamma(0.001,0.001)
lowf ~ dlnorm(beta[1], tau)
normalf ~ dlnorm(beta[1] + beta[2], tau) highf ~ dlnorm(beta[1] + beta[3], tau)
}"
jags.param <- c("beta", "mu_g", "tau")
Dias.fit1 <- jags(jags.data, parameters.to.save=jags.param, model.file=textConnection(model_diasorin),n.chains=1,
n.iter=12000, n.thin=1, n.burnin=1000)

######## unequal variance
X.mat=model.matrix(~ g)
Ind_1=rep(NA, length(g)); Ind_1=ifelse(g=="Low", 1, 0) jags.data=list(Y=logD, Xmat=X.mat, Ind_1=Ind_1, n=19+15+50) model_diasorin <- "model
{
for(i in 1:n){
Y[i] ~ dnorm(mu[i],tau[i])
mu[i] <- beta[1] + beta[2]*Xmat[i,2]+beta[3]*Xmat[i,3]
tau[i] <- tau_g[1]*Ind_1[i]+tau_g[2]*Xmat[i,2]+tau_g[3]*Xmat[i,3]
}
beta[1] ~ dnorm(0, 0.00001)
beta[2] ~ dnorm(0, 0.00001)
beta[3] ~ dnorm(0, 0.00001)
mu_g[1]=beta[1]
mu_g[2]=beta[1]+beta[2] mu_g[3]=beta[1]+beta[3]
tau_g[1] ~ dgamma(0.001,0.001)
tau_g[2] ~ dgamma(0.001,0.001)
tau_g[3] ~ dgamma(0.001,0.001)
lowf ~ dlnorm(beta[1], tau_g[1])
normalf ~ dlnorm(beta[1] + beta[2], tau_g[2]) highf ~ dlnorm(beta[1] + beta[3], tau_g[3])
}"
jags.param <- c("beta", "mu_g", "tau_g")
Dias.fit2 <- jags(jags.data, parameters.to.save=jags.param, model.file=textConnection(model_diasorin),
n.chains=1, n.iter=12000, n.thin=1, n.burnin=1000);
Dias.fit2


### model choice
X.mat=model.matrix(~ g)
jags.data=list(Y=logD, Xmat=X.mat, n=19+15+50) model_diasorin <- "model
{
for(i in 1:n){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- beta[1] + beta[2]*Xmat[i,2]+beta[3]*Xmat[i,3]
}
beta[1] ~ dnorm(0, 0.00001) beta[2] ~ dnorm(0, 0.00001) beta[3] ~ dnorm(0, 0.00001) mu_g[1]=beta[1] mu_g[2]=beta[1]+beta[2] mu_g[3]=beta[1]+beta[3]
tau ~ dgamma(0.001,0.001)
lowf ~ dlnorm(beta[1], tau)
normalf ~ dlnorm(beta[1] + beta[2], tau) highf ~ dlnorm(beta[1] + beta[3], tau) diff21 <- mu_g[2]-mu_g[1]
diff31 <- mu_g[3]-mu_g[1]
diff32 <- mu_g[3]-mu_g[2]
P <- step(diff21)*step(diff31)*step(diff32) prob21 <- step(diff21)
prob31 <- step(diff31)
prob32 <- step(diff32)
med1 <- exp(mu_g[1])
med2 <- exp(mu_g[2])
med3 <- exp(mu_g[3])
relmed21 <- med2/med1
relmed31 <- med3/med1
relmed32 <- med3/med2
}
"

#### prediction and diagnosis
X.mat=model.matrix(~ g)
jags.data=list(Y=logD, Xmat=X.mat, n=19+15+50, piL=0.40, piN=0.20, piH=0.40,
x=c(50,150,250,350,450,550,650,750,850,950,1050)) model_diasorin <- "model
{
for(i in 1:n){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- beta[1] + beta[2]*Xmat[i,2]+beta[3]*Xmat[i,3]
}
beta[1] ~ dnorm(0, 0.00001) beta[2] ~ dnorm(0, 0.00001) beta[3] ~ dnorm(0, 0.00001) mu_g[1]=beta[1] mu_g[2]=beta[1]+beta[2] mu_g[3]=beta[1]+beta[3]
tau ~ dgamma(0.001,0.001)
lowf ~ dlnorm(beta[1], tau)
normalf ~ dlnorm(beta[1] + beta[2], tau) highf ~ dlnorm(beta[1] + beta[3], tau) for(i in 1:11){
aL[i] <- piL*sqrt(tau/(2*3.14159))*(1/x[i])*exp(-(tau/2)*(log(x[i])-mu_g[1])*(log(x[i]) - mu_g[1])) bL[i] <- piN*sqrt(tau/(2*3.14159))*(1/x[i])*exp(-(tau/2)*(log(x[i])-mu_g[2])*(log(x[i])-mu_g[2])) cL[i] <- piH*sqrt(tau/(2*3.14159))*(1/x[i])*exp(-(tau/2)*(log(x[i])-mu_g[3])*(log(x[i])-mu_g[3])) pL[i] <- aL[i]/(aL[i] + bL[i] + cL[i])
pN[i] <- bL[i]/(aL[i] + bL[i] + cL[i])
pH[i] <- cL[i]/(aL[i] + bL[i] + cL[i])
} }
"
 ### Figure 9.3. Plot classification probabilities (posterior means) versus log diasorin score for the low, norma
pH <- Dias.fit3$BUGSoutput$sims.matrix[,2:12] pL <- Dias.fit3$BUGSoutput$sims.matrix[,13:23] pN <- Dias.fit3$BUGSoutput$sims.matrix[,24:34] g1 <- apply(pL,2,mean)
g2 <- apply(pN,2,mean) g3 <- apply(pH,2,mean)
x=c(50,150,250,350,450,550,650,750,850,950,1050)
plot(log(x),g1, type='l', xlab="", main="", ylab="", xlim=c(3,7.5), ylim=c(0,1), lwd=3) lines(log(x),g2,lty=2,lwd=3)
lines(log(x),g3,lty=3,lwd=3)
mtext("log (Diasorin Score)",line=3,side=1,cex=1.2) mtext("Probability",line=2.5,side=2,cex=1.2) legend("topleft",c("Low","Normal","High"),lty=1:3,lwd=c(2,2,2))


