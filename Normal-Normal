title: "Normal-Normal Model & Posterior Credible Intervals"


output: 
  html_document: 
    toc: yes
---
### The Normal-Normal model

For simplicity let's start by considering the (unlikely) case of a single observation from a Normal distribution, say
$y = 0.1$, where we assume that

$$
Y \sim N(\mu, \frac{1}{\tau^*})
$$
We assume to know that the standard deviation $\sigma^*= 0.015$.

We then consider a Normal prior
$$
\mu \sim N(\mu_0, \frac{1}{\tau_0})
$$
We assume that a priori we may center the prior on $\mu_0=0.05$, where $\tau_0=\frac{1}{\sigma_0}=0.025$ 
represents the uncertainty in our prior elicitation.

```{r}
Y      <- 0.10   ## data
sigma  <- 0.015  ## standard deviation (!!rnorm!!)
m0      <- 0.05   ## prior mean
s0     <- 0.025  ## prior standard deviation
grid   <- seq(0,0.15,.001)

like   <- dnorm(Y,grid,sigma)   
like   <- like/sum(like) #standardize

prior  <- dnorm(grid,m0,s0) 
prior  <- prior/sum(prior) #standardize

post   <- like*prior
post   <- post/sum(post)

 plot(grid,like,type="l",lty=2,
     xlab="mu",ylab="Density", ylim=c(0,0.03))
     lines(grid,prior)
     lines(grid,post,lwd=2)

      legend("topleft",c("Likelihood","Prior","Posterior"),
                  lwd=c(1,1,2),lty=c(2,1,1),inset=0.05)
```

Let's compute the posterior mean and posterior variance

```{r}
 post_var <- 1/(sigma^{-2}+s0^{-2})          # Posterior var 
 post_sd  <- sqrt(post_var)                 # Posterior sd 
 post_mn  <- post_var*(Y/sigma^2+m0/s0^2)     # Posterior mean 
 post_mn; post_sd
```


We may also be interested in the 95% **credible intervals** of the  posterior distribution
```{r}
 ## 95% Posterior Interval 
 ## --> Credible Interval 
 
 qnorm(c(0.025,0.975),post_mn,post_sd) 
 ```

**Check:** how the results change by changing $\mu_0, \tau_0$ and our assumptions on $\sigma^2_*=\frac{1}{\tau^*}$.

**Link:** [ A simple Shiny application to try out different examples - Matthew Stephens](https://stephens999.github.io/fiveMinuteStats/shiny_normal_example.html)
 
#### Further data arriving 

Now let's suppose to have obtained a further random sample of 25 drivers, tested on a stretch of a highway that is notorious for the number of accidents caused by driving while intoxicated.

```{r}
xx = read.csv("BAC.csv")
data=xx$bloodAlcohol
hist(data, main="BAC",  xlab="y", prob=TRUE, breaks=6, col="blue")
lines(density(data), col="red", lwd=3) 
```


Let's determine the posterior distribution. The posterior distribution of $\mu$ is still normal with updated parameters:

```{r}
n=25;
sigma  <- 0.015  ## standard deviation 
m0      <- 0.098   ## this was the posterior mean
                  ## based on one observation only
s0      <- 0.0049  ## this was the standard deviation
                  ## based on one observation only

prior.precision = 1/s0^2; 
post.precision = prior.precision + (n/sigma^2)
post.sd=sqrt(1/post.precision)
post.mean = (prior.precision/post.precision * m0) + 
  ((n/sigma^2)/post.precision * 
        mean(data))
post.mean
post.sd

prior_samples <- rnorm(10000, m0, s0)
posterior_samples <- rnorm(10000, post.mean, post.sd)
plot(density(prior_samples), col="red", lwd=2, main="", xlim=c(0.02, 0.12), ylim=c(0,120))
lines(density(posterior_samples), col="blue", lwd=3)
 legend("topleft",c("New Prior","Posterior"),
                  lwd=c(2,3),col=c("red", "blue"),inset=0.05)
```
 
### Facebook CPC
 
 The average cost for each link click (CPC) is a metric used in the online advertising industry for benchmarking ad efficiency and performance. The metric is calculated as the total amount spent divided by link clicks.
 
 Suppose that you are the CEO of  "Pistacchio International", an international ice cream firm who wants to promote its business on Facdebook. For budgeting purposes, you want to know the true CPC for the ads you have placed. 
 
 You know, from Facebook reporting, that the CPC across several different countries varies as follows:
 
![optional caption text](CPC_International.png)

 
The average CPC for all countries came in at $0.97$, with Croatia, Cyprus, and Switzerland having the lowest CPC of $0.75$. However, there is some marked variability across the countries.

We can consider $\mu|\tau^* \sim N(\mu_0=0.97, \frac{1}{\tau_0})$
  
What would be your likely choice of $\tau_0$?

Now suppose that you have data over the CPC for your firm in the last year across several countries:
  
```{r}
data=c(1.7156439, 1.3610588, 1.3974624, 0.9263681, 1.1465699, 1.1081685, 1.5275430, 1.0164914, 1.3704110, 0.6092907, 0.8200065, 1.3394218, 1.5534802, 1.9116454, 1.2950351, 1.0243660, 1.8814660, 1.4002373, 0.8744721, 1.7239122)
```

and you know that $\sigma_*^2=0.25$, i.e. $\tau_*=4$.

What is your posterior inference about the cost per click of the advertisements placed by your company?

```{r}
hist(data, main="",  xlab="y", prob=TRUE, breaks=6, col="blue")
lines(density(data), col="red", lwd=3) 
```

Let's determine the posterior distribution. The posterior distribution of $\mu$ is still normal with updated parameters:

```{r}
n=25;
sigma  <- 0.5  ## standard deviation 
m0      <- 0.97   
s0      <- 0.5
prior.precision = 1/s0^2; 
post.precision = prior.precision + (n/sigma^2)
post.sd=sqrt(1/post.precision)
post.mean = (prior.precision/post.precision * m0) + 
  ((n/sigma^2)/post.precision * 
        mean(data))
post.mean
post.sd

prior_samples <- rnorm(10000, m0, s0)
posterior_samples <- rnorm(10000, post.mean, post.sd)
plot(density(prior_samples), col="red", lwd=2, main="", ylim=c(0, 5))
lines(density(posterior_samples), col="blue", lwd=3)
 legend("topleft",c("New Prior","Posterior"),
                  lwd=c(2,3),col=c("red", "blue"),inset=0.05)
```
 

####### hw

y=c(38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 64.1, 40.1, 40.7, 6.4) ybar=mean(y)
n=length(y)
prior.precision = 1; sigma=10; m=40;
post.precision = prior.precision + (n/sigma^2) post.sd=sqrt(1/post.precision)
post.mean = (prior.precision/post.precision * m) +((n/sigma^2)/post.precision * ybar)

curve(dnorm(x, m, 1/sqrt(prior.precision)) , from=10, to=100, type="l", lty=2, ylim=c(0, 1), 
xl curve(dnorm(x, post.mean, 1/sqrt(post.precision)) , from=10, to=100,add=TRUE, type="l", col="re abline(v=ybar, 
lty=2, col="blue")
axis(1, at=ybar, labels=expression(bar(y)))
legend("topright", legend=c("prior", "posterior"), lty=c(2,1), col=c("black", "red"))

 CI=qnorm(c(0.1, 0.9), mean=post.mean, sd=sqrt(1/post.precision)) 
 
M=10000 # number of samples we want to generate from the posterior 
sample_from_posterior=rnorm(M, mean=post.mean, sd=sqrt(1/post.precision)) 
MC_CI=quantile(sample_from_posterior, probs=c(0.1, 0.9))

B=10000
post.samples <- rnorm(B, mean=post.mean, sd=sqrt(1/post.precision)) mean(log(post.samples))
 
