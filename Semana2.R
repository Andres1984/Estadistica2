### Segunda Semana


## Muestreo aleatorio Simple
n <- 20                       
nreps <- 10000                 
sample.mean <- numeric(nreps)  
par(mfrow = c(3,2))

sample.normal <- rnorm(n = 1000, mean = 35, sd = 15)  
for (i in 1:nreps) {
  sample <- rnorm(n = n,mean =  35, sd = 15)  
  sample.mean[i] <- mean(sample)     
}  


hist(sample.normal, breaks = 50, main = "Distribución normal Población", xlab = "")
hist(sample.mean, breaks = 50, main = "Distribución de la muestra", xlab = "Mean")

#### Ejemplo

#############################
#### SAMPLING TECHNIQUES ####
#############################

set.seed(1)
X1 = cbind(1, rnorm(100, 1, 1))
X2 = cbind(2, rnorm(200, 2, 1))
X3 = cbind(3, rnorm(400, 3, 1))
X4 = cbind(4, rnorm(800, 4, 1))
X5 = cbind(5, rnorm(3500, 5, 1))
df = rbind(X1, X2, X3, X4, X5)
df = data.frame(Strat = df[, 1], Values = df[, 2])
x = nrow(df)
true_mean = sum((c(nrow(X1), nrow(X2), nrow(X3), nrow(X4), nrow(X5))/x) *
                  (1:5)) ### True Mean
true_mean

################################
#### Simple Random Sampling ####
################################

n = 500 # Extracted Sample Size
set.seed(1)
sub = sample(x, size = n, replace = TRUE)
mean_with = mean(df[sub, 2])
c(mean_with, true_mean)
# Without Replacement
set.seed(1)
sub = sample(x, size = n, replace = FALSE)
mean_without = mean(df[sub, 2])
c(mean_without, true_mean)
####################################
#### Systematic Random Sampling ####
####################################

set.seed(1)
sequence = x/n
start = sample(x=c(1:sequence), size=1, replace = FALSE)
sub = seq(start,x,sequence)
mean_system = mean(X[sub,2]) # 4.111508
mean_system; true_mean


####################################
#### Stratified Random Sampling ####
####################################

library("sampling")
set.seed(1)
SAM = MU = NULL
for (i in 1:max(df[, 1])) {
  X1 = df[which(df$Strat == i), ]
  sam = round((nrow(X1)/x) * n)
  SAM = c(SAM, sam)
  mu = X1[sample(x = dim(X1)[1], size = sam), 2]
  MU = c(MU, mu)
}
mean_strat = mean(MU)
c(mean_strat, true_mean)


