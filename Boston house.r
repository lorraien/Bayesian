library(MASS) attach(Boston) Boston_model <- "model{
for(i in 1:nrows){
medv[i]~dnorm(mu[i],tau) mu[i]=int+indus[i]*b[1]+nox[i]*b[2]+rm[i]*b[3]+
tax[i]*b[4]+ptratio[i]*b[5]+lstat[i]*b[6]
}
int~dnorm(0,0.01) # int has prior variance 1/0.01. tau~dgamma(0.1,0.1) # taue is a precision (inverse variance)
for(j in 1:6){
prec[j]=(1-gamma[j])*100+gamma[j]*0.1
b[j]~dnorm(0,prec[j])
gamma[j]~dbern(0.5)
}
}"

jags.data=list(
medv=medv, indus=indus, nox=nox, rm=rm, tax=tax, ptratio=ptratio, lstat=lstat, nrows=dim(Boston)[1]
)
jags.param <- c("b", "gamma")
Boston.fit1 <- jags(jags.data, parameters.to.save=jags.param,
model.file=textConnection(Boston_model),n.chains=2, n.iter=10000, n.thin=1, n.burnin=1000)

###########
There are multiple packages in R that perform spike-and-slab variable selection with a point mass at zero.
Check the vignettes carefully before using them. Here, we use BoomSpikeSlab for its simplicity

spike.Boston=BoomSpikeSlab::lm.spike(medv ~ ., data=Boston,
summary(spike.Boston)
plot(spike.Boston)
plot(spike.Boston, "coefficients")
plot(spike.Boston, "residuals")

######Lasso
X.mat=as.matrix(subset(Boston, select=c(-medv))) Boston_model_BL <- "model{
# Likelihood for(i in 1:n){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- beta0 + inprod(X[i,],beta[])
}
# Prior for beta for(j in 1:p){
beta[j] ~ ddexp(0,tau.b) prob_pos[j]=step(beta[j]) prob_neg[j]=step(-beta[j])
}
# Prior for the inverse variance tau ~ dgamma(0.01, 0.01)
tau.b ~ dgamma(0.01, 0.01)
beta0 ~ dnorm(0, 0.01)
}"

jags.data=list(
Y=medv, X=X.mat, n=dim(Boston)[1], p=dim(X.mat)[2] )
jags.param <- c("beta", "prob_pos", head(X.mat)
Boston.fit.BL <- jags(jags.data, parameters.to.save=jags.param, model.file=textConnection(Boston_model_BL),n.chains=2,
n.iter=10000, n.thin=1, n.burnin=1000)



##### Horseshoe Shrinkage prior
library(horseshoe)
Boston.horseshoe=horseshoe(medv, X.mat, method.tau = "truncatedCauchy", method.sigma = "Jeffreys", burn=

n=13; prefix="beta"; suffix=seq(1:n); index=paste(prefix, suffix, sep=".")
quantile.horseshoe=t(apply(Boston.horseshoe$BetaSamples, 1, quantile, prob=c(0.025, 0.5, 0.975))) 
post.mean.horseshoe=apply(Boston.horseshoe$BetaSamples, 1, mean)
post.summary.horseshoe=cbind(index, post.mean.horseshoe, quantile.horseshoe)
 kable(post.summary.horseshoe, format="latex")

