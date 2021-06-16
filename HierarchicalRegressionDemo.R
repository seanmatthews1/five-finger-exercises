          ############################################################
          #
          # Short note on hiearchical models demo using synthetic data
          # 
          # for Marion
          #
          # Sean Matthews Easter 2020
          #
          #
          #
          ############################################################
          #
          # We  construct linear  models by  sampling from  a Gaussian
          # (intercept, slope)  centred at (1,0) and  stretched in the
          # direction of the origin, so that the slopes vary more than
          # the intercept (but both vary).
          #
          # The distribution looks as follows:

library(rstan)
library(assertthat)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


TOTAL_OBSERVATIONS <- 8 * 12 # total number of observations
N_DATASETS         <- 9      # number of data sets
RR                 <- 0:5    # range of explanatory variable
SIG                <- 0.4    # variance of observational noise
                             
VAR                <- matrix(c(0.1, 0, 0, 0.1), nrow=2)
                             # scale1 of coefficient space

OFFSET             <- 0.5    # offset of coefficient space (on intercept)
TESTV_LEN          <- 10     # length of test vector for each data set regression

XX = c(0.5, 4.5)

unit_circle =
    t(as.matrix(
        data.frame(x = mapply(sin, seq(0, 2*pi, length.out=100)),
                   y = mapply(cos, seq(0, 2*pi, length.out=100)))))

Plot <- function (data_set, intercept, slope, color, show_samples = FALSE, new_graph = TRUE) {

    if (new_graph) plot(c(-2,2), c(0,1), xlab = "intercept", ylab="slope", pch=NA, main=paste("dataset ", data_set, sep=""))
    samples = data.frame(intercept, slope)
    centre = apply(as.matrix(samples), 2, mean)
    pp = (2 * t(chol(cov(samples)))) %*% unit_circle 
    if (show_samples) points(extract(fit)$alpha, extract(fit)$beta, col="grey")
    lines(pp[1,] + centre[1], pp[2,] + centre[2], col=color)
    points(SS[2, data_set], SS[1, data_set],  col=color, pch=19)
    centre = apply(as.matrix(samples), 2, mean)
    points(SS[2,data_set], SS[1,data_set],  col=color, pch=3)
    points(centre[1], centre[2], col=color, pch=3)
#    lines(c(SS[2,data_set],centre[1]), c(SS[1,data_set], centre[2]), col=color)
}

BasicLinearRegressionModel = "
data {
  int<lower=0> N;       // number of data items
  int<lower=0> K;       // number of predictors
  matrix[N, K] x;       // predictor matrix
  vector[N] y;          // outcome vector

  int<lower=0> M;       // number of test items
  matrix[M, K] x_hat;   // predictor matrix
}
parameters {
  real alpha;           // intercept
  vector[K] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}
model {
  y ~ normal(x * beta + alpha, sigma);  // likelihood
}
generated quantities {
  vector[M] y_hat;
  y_hat = x_hat * beta + alpha;
}"

HierarchicalLinearRegressionModel = "
data {
int<lower=0>                N        ; // num individuals
int<lower=1>                K        ; // num ind predictors
int<lower=1>                J        ; // num groups
int<lower=1,upper=J>        jj[N]    ; // group for individual
matrix               [N, K] x        ; // individual predictors
vector               [N   ] y        ; // outcomes

int<lower=0>                M        ; // num individual tests
matrix               [M, K] x_hat    ; // individual tests
int<lower=1,upper=J>        jj_hat[M]; // group for individual tests
}
parameters {
  matrix                      [K, J] z       ;
  cholesky_factor_corr        [K   ] L_Omega ;
  vector<lower=0,upper=pi()/2>[K   ] tau_unif;
  vector                      [K   ] gamma   ;
  real<lower=0>                      sigma   ;
}
transformed parameters {
  matrix         [J, K] beta;
  vector<lower=0>[K   ] tau ;
  
  for (k in 1:K) tau[k] = 2.5 * tan(tau_unif[k]);
  beta = rep_matrix(gamma, J)' + (diag_pre_multiply(tau,L_Omega) * z)';
}
model {
  to_vector(z) ~ std_normal();
  gamma        ~ normal(0,5);
  L_Omega      ~ lkj_corr_cholesky(2);
  y            ~ normal(rows_dot_product(beta[jj] , x), sigma);
}
generated quantities {
  vector [M] y_hat;
  cholesky_factor_cov[K] gamma_cov_chol;
  y_hat = rows_dot_product(beta[jj_hat], x_hat);
  gamma_cov_chol = diag_pre_multiply(tau,L_Omega);
}
"

          #
          ############################################################
          #
          # S[1,] - Slope, S[1,] - Intercept

SS     <- VAR %*% matrix(rnorm(N_DATASETS * 2),nrow=2)

SS[1,] <- SS[1,] + OFFSET

observations <-
    with(list(),
    {
            indicator = sample(1:N_DATASETS, TOTAL_OBSERVATIONS, replace=TRUE)
            x_values  = runif(TOTAL_OBSERVATIONS) * (max(RR) - min(RR)) + min(RR)
            y_values  = mapply(function (x, i) SS[1,i] * x + SS[2,i] + rnorm(1) * SIG,
                               x_values,
                               indicator)
            data.frame(indicator = indicator,
                       x_values  = x_values,
                       y_values  = y_values)
    })


par(mfrow = c(1,2))

example  <-
    with(list(),
    {
        t  <- table(observations$indicator)
        match(max(t), t)
    })

OB = subset(observations, indicator == example)

plot(RR, RR * 3 / 5, xlab = "X", ylab = "Y", pch=NA)

lines(XX,  SS[1,example] * XX + SS[2,example], lwd=2)

with(OB, points(x_values, y_values, pch=3))

DD = list(N = nrow(OB),
          K = 1,
          x = as.matrix(OB[,c("x_values")]),
          y = OB$y_values,
          M = 100,
          x_hat = as.matrix(data.frame(seq(0.5, 4.5, length.out=100))))

fit <- stan(model_code = BasicLinearRegressionModel, data = DD)
    
with(list(x_hat      = seq(0.5, 4.5, length.out = 100)   ,
          y_hat_mean = apply(extract(fit)$y_hat, 2, mean),
          y_hat_sd   = apply(extract(fit)$y_hat, 2, sd  )),
{
    lines(x_hat, y_hat_mean, col="red", lwd=2)
    lines(x_hat, y_hat_mean + y_hat_sd, col="red")
    lines(x_hat, y_hat_mean - y_hat_sd, col="red")
})

Plot(example, extract(fit)$alpha, extract(fit)$beta, "red", show_samples = TRUE)

par(mfrow = c(1,1))
invisible(readline(prompt="Press [enter] to continue"))



          # Not clear  exactly what playing  fair in this  case really
          # means, but we  will use (or at least  start with) standard
          # models from  the Stan  manual which seems  reasonable (and
          # also makes  it easy  to generate credible  intervals. Note
          # generated quantities added.


par(mfrow = c(1,2))

plot(RR, RR * 3 / 5, xlab = "X", ylab = "Y", pch=NA)

for (i in 1:N_DATASETS) {
    OB = subset(observations, indicator == i)
    lines(RR,  SS[1,i] * RR + SS[2,i], lwd=1.5, col=i)
    with(OB, points(x_values, y_values, pch=3,  col=i))
}

plot(c(-2,2), c(0,1), xlab = "intercept", ylab="slope", pch=NA)

for (i in 1:N_DATASETS) {
    points(SS[2,i], SS[1,i], pch=19, col=i)
}

par(mfrow = c(1,1))
invisible(readline(prompt="Press [enter] to continue"))

par(mfrow = c(1,2))

OB = observations

plot(RR, RR * 3 / 5, xlab = "X", ylab = "Y", pch=NA)

for (i in 1:N_DATASETS) {
    with(subset(OB, indicator == i),
         points(x_values, y_values, pch=3, col=i)
         )}

DD = list(N = nrow(OB),
          K = 1,
          x = as.matrix(OB[,c("x_values")]),
          y = OB$y_values,
          M = 100,
          x_hat = as.matrix(data.frame(seq(0.5, 4.5, length.out=100))))

fit <- stan(model_code = BasicLinearRegressionModel, data = DD)
    
with(list(x_hat      = seq(0.5, 4.5, length.out = 100)   ,
          y_hat_mean = apply(extract(fit)$y_hat, 2, mean),
          y_hat_sd   = apply(extract(fit)$y_hat, 2, sd  )),
{
    lines(x_hat, y_hat_mean, col="red", lwd=2)
    lines(x_hat, y_hat_mean + 2 * y_hat_sd, col="red")
    lines(x_hat, y_hat_mean - 2 * y_hat_sd, col="red")
})

intercept = extract(fit)$alpha
slope = extract(fit)$beta
plot(c(-2,2), c(0,1), xlab = "intercept", ylab="slope", pch=NA)
samples = data.frame(intercept, slope)
centre = apply(as.matrix(samples), 2, mean)
pp = (2 * t(chol(cov(samples)))) %*% unit_circle 
points(extract(fit)$alpha, extract(fit)$beta, col="grey")
lines(pp[1,] + centre[1], pp[2,] + centre[2], col="red")
centre = apply(as.matrix(samples), 2, mean)
points(centre[1], centre[2], col="red", pch=3)

for (i in 1:N_DATASETS) {
    points(SS[2,i], SS[1,i], pch=19, col=i)
}

par(mfrow = c(1,1))
invisible(readline(prompt="Press [enter] to continue"))

par(mfrow = c(3,3))

memoise  <- list(1:N_DATASETS)


for (i in 1:N_DATASETS) {

    OB = subset(observations, indicator == i)

    DD = list(N = nrow(OB),
              K = 1,
              x = as.matrix(OB[,c("x_values")]),
              y = OB$y_values,
              M = 100,
              x_hat = as.matrix(data.frame(seq(0.5, 4.5, length.out=100))))

    fit <- stan(model_code = BasicLinearRegressionModel, data = DD)

    memoise[[i]]  <- fit

    Plot(i, extract(fit)$alpha, extract(fit)$beta, "red", show_samples = TRUE)
}

par(mfrow = c(1,1))
invisible(readline(prompt="Press [enter] to continue"))

par(mfrow = c(1,2))

plot(c(-2,2), c(0,1), xlab = "intercept", ylab="slope", pch=NA)

for (i in 1:N_DATASETS) {
    points(SS[2,i], SS[1,i], pch=19, col=i)
}

samples = data.frame(SS[2,], SS[1,])
centre = apply(as.matrix(samples), 2, mean)
pp = (2 * t(chol(cov(samples)))) %*% unit_circle 
lines(pp[1,] + centre[1], pp[2,] + centre[2], col="grey")

par(mfrow = c(1,1))
invisible(readline(prompt="Press [enter] to continue"))

observations$one <- 1

tests <- with(merge(x = seq(min(RR), max(RR), length.out = TESTV_LEN),
                    y = unique(observations$indicator)),
              data.frame(x_hat     = x,
                         indicator = y))

tests$one  <- 1

DD = list(N      = nrow(observations)                            ,
          K      = 2                                             ,
          J      = length(unique(observations$indicator))        ,
          jj     = observations$indicator                        ,
          x      = as.matrix(observations[,c("x_values", "one")]),
          y      = observations$y_values                         ,
          M      = nrow(tests)                                   ,
          x_hat  = as.matrix(tests[,c("x_hat", "one")])          ,
          jj_hat = tests$indicator                               )


fit <- stan(model_code = HierarchicalLinearRegressionModel,
            data = DD,
            iter=10000,
            pars = c("beta", "y_hat", "sigma", "gamma", "gamma_cov_chol"))

par(mfrow = c(3,3))

with(extract(fit),
     for (i in 1:N_DATASETS) {

         with(extract(memoise[[i]]),
              Plot(i, alpha, beta, "black"))

         Plot(i, beta[,i,2], beta[,i,1], "red", new_graph = FALSE)
     }
     )

par(mfrow = c(1,1))
invisible(readline(prompt="Press [enter] to continue"))


par(mfrow = c(1,2))

plot(c(-2,2), c(0,1), xlab = "intercept", ylab="slope", pch=NA)

gg = with(extract(fit), apply(gamma, 2, mean))

for (i in 1:N_DATASETS) {
    points(SS[2,i], SS[1,i], pch=19, col=i)
}

cc = apply(extract(fit)$gamma_cov_chol, c(2,3), mean)

pp = (2 * cc %*% unit_circle)
lines(pp[1,] + gg[2], pp[2,] + gg[1], col="red")

par(mfrow = c(1,1))


