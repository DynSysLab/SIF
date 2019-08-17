# jags model to estimate holling's disc equation parameters
holling <- function() {

	# hyperparameter
	mu_a ~ dunif(0, 100)
	mu_h ~ dunif(0, 1000)
	tau_a ~ dunif(0, 1000)
	tau_h ~ dunif(0, 1000)

	p_N0 ~ dunif(0.0001, 1)
	r_N0 ~ dunif(0, 1E4)

	# tansformed parameters
	sigma_a <- tau_a^-2
	sigma_h <- tau_h^-2

	# for each individual
	for(j in 1:J) {
		
		a[j] ~ dnorm(mu_a, tau_a); T(0.0001,)
		h[j] ~ dnorm(mu_h, tau_h); T(0.0001,)
	
	}

	# for each image
	N0_image[1] ~ dnegbin(p_N0, r_N0);T(21,)
	N0_image[2] ~ dnegbin(p_N0, r_N0);T(19,)
	N0_image[3] ~ dnegbin(p_N0, r_N0);T(24,)
	N0_image[4] ~ dnegbin(p_N0, r_N0);T(20,)
	N0_image[5] ~ dnegbin(p_N0, r_N0);T(23,)
	N0_image[6] ~ dnegbin(p_N0, r_N0);T(38,)

	# prior
	beta ~ dunif(0,1000)

	# likelihood
	for(i in 1:N) {
		# intake rate
		N_image[i] <- N0_image[image[i]] - x[i,image[i]]
		mu[i] <- a[ind[i]]*N_image[i]/(1+a[ind[i]]*h[ind[i]]*N_image[i])
		# intake interval
		inv_mu[i] <- 1/mu[i]
		# model
		y[i] ~ dgamma(inv_mu[i]*beta, beta)
	}
}

# import data
summary <- read.csv('summary_exit.csv')
dat <- read.csv('dat.csv')
residence <- read.csv('residence.csv')

dat$uniqueID <- paste(dat$batch, dat$gameID, dat$player, dat$condition)
dat$ind <- as.numeric(as.factor(dat$uniqueID))

residence$uniqueID <- paste(residence$batch, residence$gameID, residence$player, residence$condition)
residence$ind <- dat$ind[match(residence$uniqueID, dat$uniqueID)]

J <- max(dat$ind)
x <- as.matrix(dat[,9:14])
y <- dat$interval/1000 	# in second
image <- dat$image
ind <- dat$ind
N <- nrow(dat)

mydata <- list(J=J, y=y, N=N, ind=ind, image=image, x=x)

# parameters to save
myparams <- c('mu_a', 'mu_h', 'N0_image', 'p_N0', 'r_N0', 'a', 'h',
	'sigma_a', 'sigma_h', 'beta')
# initial values
myinit <- list('mu_a'=1, 'tau_a'=1, 'mu_h'=20, 'tau_h'=1, 'beta'=1,
	'p_N0'=0.5, 'r_N0'=1)

myinits <- list(myinit, myinit, myinit)

# run
library(R2jags)
set.seed(1212)
fit <- jags(data=mydata,
	model.file=holling,
	inits=myinits,
	parameters.to.save=myparams,
	n.iter=1E6,
	n.burnin=0.5E6,
	n.thin=100,
	n.chains=3)

# extract estimated attack rate (a) and handling time (h)
par <- fit$BUGSoutput$sims.matrix
a <- h <- c()
for(i in 1:210) {
	a_name <- paste('a[', i, ']', sep='')
	h_name <- paste('h[', i, ']', sep='')
	a[i] <- median(par[,colnames(par)==a_name])
	h[i] <- median(par[,colnames(par)==h_name])
}

# extract estimated numbers of resources at t = 0
N0_image <- c()
N0_image[1] <- median(par[,colnames(par)=='N0_image[1]'])
N0_image[2] <- median(par[,colnames(par)=='N0_image[2]'])
N0_image[3] <- median(par[,colnames(par)=='N0_image[3]'])
N0_image[4] <- median(par[,colnames(par)=='N0_image[4]'])
N0_image[5] <- median(par[,colnames(par)=='N0_image[5]'])
N0_image[6] <- median(par[,colnames(par)=='N0_image[6]'])


# get the expected residence time using marginal value theorem
## a: search rate
## h: handling time
## N0: Initial resource level at entry
## T: traveling time
rt <- function(a,h,N0,T,dt) {

	tt <- 0
	Nt <- N0
	gain_t <- 0

	diff <- 1

	while(diff > 0) {
		tt <- tt + dt

		gain_t_1 <- gain_t
		dNt <- a*Nt/(1+a*h*Nt)
		gain_t <- gain_t + dNt * dt
		Nt <- Nt - dNt * dt

		# compare slope
		slope1 <- (gain_t - gain_t_1)/dt
		slope2 <- gain_t/(tt + T)

		diff <- slope1 - slope2
	}	

	return(tt)
}


# get expected residence time
for(i in 1:nrow(residence)) {

	player <- residence$ind[i]
	aa <- a[player]
	hh <- h[player]
	NN0 <- N0_image[residence$imageID[i]] - residence$n_tags[i]
	TT <- residence$travel[i]/1000

	residence$expected_duration[i] <- rt(aa, hh, NN0, TT, dt=0.001)

}

#---------------------------------------------
# stats
library(lme4)
library(optimx)
library(car)
library(emmeans)

# Is residence time different between the condition?
residence$duration <- residence$duration/1000
coll <- residence[residence$condition=='collaboration',]
comp <- residence[residence$condition=='competition',]

model <- glmer(duration ~ condition + (1|ind), data=residence, family=Gamma('log'))
model0 <- glmer(duration ~ 1 + (1|ind), data=residence, family=Gamma('log'))
anova(model, model0)

## Is the proportion to MVT different from 1?
coll$proportion <- coll$duration/coll$expected_duration
comp$proportion <- comp$duration/comp$expected_duration

### for collaboration
model <- lmer(I(duration/60) ~ 0 + I(expected_duration/60) + (0+I(expected_duration/60)|ind), data=coll)
model0 <- lmer(I(duration/60) ~ 0 + offset(I(expected_duration/60)) + (0+I(expected_duration/60)|ind), data=coll)
anova(model, model0)

### for competition
model <- lmer(I(duration/60) ~ 0 + I(expected_duration/60) + (0+I(expected_duration/60)|ind), data=comp)
model0 <- lmer(I(duration/60) ~ 0 + offset(I(expected_duration/60)) + (0+I(expected_duration/60)|ind), data=comp)
anova(model, model0)

## for each patch trait, is the proportion influenced by the rank and social condition?
var_name <- c('myTags', 'othersTags', 'myTime', 'othersTime')
foo <- summary[summary$var==var_name[1],]
foo$duration <- residence2$duration
foo$expected_duration <- residence2$expected_duration
foo$remaining <- 10 - residence2$exit_time/60000  	# in minutes
foo$rank <- as.factor(foo$rank)
foo$proportion <- foo$duration/foo$expected_duration

model <- glmer(I(duration/60) ~ condition * rank + 
	offset(I(log(expected_duration/60))) +  
	(1|playerID), data=foo, family=Gamma('log'),
	control = glmerControl(optimizer = "optimx",
     optCtrl = list(method = "bobyqa")))

Anova(model)

## posthocs
means <- emmeans(model, ~ rank)
contrast(means, method='pairwise', adj='fdr')