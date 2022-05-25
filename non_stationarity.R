#########################################
## Run Simulations & Perform Inference ##
#########################################

# Load packages and necessary functions ----------------------------
library(tidyr)
library(pbapply)

source("simulation_inference_reward_functions.R")

# Illustrative examples with non-stationarity -----------------------------

# set parameters and hyperparameters
M = 10000 # number of MC simulations
my_nb = 3 # batch size (for BOLS works only for batch sizes > 2)
my_N = 200 # sample size
my_clip = 0 # clipping parameter for TS
my_sigma2 = c(10,10)

my_Nbatches = floor(my_N/my_nb)
#define batches for which to perform inference
my_plot_batches3 = c(1, 3, 5, 10, 15, 20, 30, 40, 50)
my_batches3 = c(1,3,5,10,15,20,30,40,50,60)

# Stationarity -----------------------------
my_mu_H0 = c(0, 0) # true arm means parameters
plot(rep(my_mu_H0[1], my_Nbatches), type="l", ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Stationarity ("*H[0]*":"~mu[1]==mu[0]*"=0)"), xlab = "Step (t)")

H0_stat = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(10,10)), 
                                         true_params = list(mu=my_mu_H0, sigma2=my_sigma2), clip = my_clip, atleastone = F)))

my_mu_H1 = c(0,0.5)
plot(rep(my_mu_H1[1], my_Nbatches), type="l", ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Stationarity ("*H[1]*":"~mu[1]*"=0.5; "*mu[0]*"=0)"), xlab = "Batch (t)")
lines(rep(my_mu_H1[2], my_Nbatches), type="l", col="red")
legend("bottomright", legend = c(expression(mu[0]), expression(mu[1])), 
       bty = "n", lty = 1, col=c("black", "red"))

H1_stat = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(10,10)), 
                                         true_params = list(mu=my_mu_H1, sigma2=my_sigma2), clip = my_clip, atleastone = F)))

typeIer_stat = mean_typeIer(my_data = H0_stat, 
                           which_test="Pi_test", 
                           batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                           plot_batches = my_plot_batches3)

power_stat = mean_power(my_data = H1_stat, 
                       which_test="Pi_test", 
                       batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                       plot_batches = my_plot_batches3,
                       Prob_adj_typeI = typeIer_stat$Prob_adj
                       #Prob_adj_dx = try_typeIer$Prob_adj_dx,
                       #Prob_adj_sx = try_typeIer$Prob_adj_sx
)


# Non-Stationarity 1: same arm means difference with time trend -----------------------------

normal_trend = function(mean0, horizon, c){ mean0/(1:horizon)^c}

my_mu_H0_nt1 = rbind(normal_trend(1, my_Nbatches, 0.5), normal_trend(1, my_Nbatches, 0.5))

par(mfrow = c(1,2))
plot(my_mu_H0_nt1[1,], type="l", ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Non-Stationarity ("*H[0]*":"~mu["1,t"]==mu["0,t"]*" = "*frac(mu,t^c)*")"), xlab = "Step (t)")

H0_nt1 = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(10,10)), 
                                         true_params = list(mu=my_mu_H0_nt1, sigma2=my_sigma2), clip = 0.1, atleastone = T)))

my_mu_H1_nt1 = my_mu_H0_nt1
my_mu_H1_nt1[2,] = my_mu_H1_nt1[2,] + 0.5
plot(my_mu_H1_nt1[1,], type="l", ylim = c(0, 1.5), ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Non-Stationarity NS1"), xlab = "Step (t)")

#plot(my_mu_H1_nt1[1,], type="l", ylim = c(0, 1.5), ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Non-Stationarity ("*H[1]*":"~mu["1,t"]==mu["0,t"]*"+0.5))"), xlab = "Step (t)")
lines(my_mu_H1_nt1[2,], type="l", col="red")
legend("topright", legend = c(expression(mu["0,t"]), expression(mu["1,t"])), 
       bty = "n", lty = 1, col=c("black", "red"))

H1_nt1 = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(10,10)), 
                                         true_params = list(mu=my_mu_H1_nt1, sigma2=my_sigma2), clip = 0.1, atleastone = T)))

typeIer_nt1 = mean_typeIer(my_data = H0_nt1, 
                           which_test="Z_BOLS", 
                           batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                           plot_batches = my_plot_batches3)

power_nt1 = mean_power(my_data = H1_nt1, 
                       which_test="Z_BOLS", 
                       batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                       plot_batches = my_plot_batches3,
                       #Prob_adj_typeI = typeIer_nt1$Prob_adj
                       Prob_adj_dx = typeIer_nt1$Prob_adj_dx,
                       Prob_adj_sx = typeIer_nt1$Prob_adj_sx
)


# Non-Stationarity 2: varying arm means difference with time trend -----------------------------

normal_trend = function(mean0, horizon, c){ mean0/(1:horizon)^c}

my_mu_H0_nt2 = rbind(normal_trend(1, my_Nbatches, 0.5), normal_trend(1, my_Nbatches, 0.5))

plot(my_mu_H0_nt2[1,], type="l", ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Non-Stationarity ("*H[0]*":"~mu["1,t"]==mu["0,t"]*" = "*frac(mu,t^c)*")"), xlab = "Step (t)")

H0_nt2 = H0_nt1

my_mu_H1_nt2 = rbind(rep(0, my_Nbatches), my_mu_H0_nt2[2,])
#plot(my_mu_H1_nt2[1,], type="l", ylim = c(0, 1.5), ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Non-Stationarity ("*H[1]*":"~mu["1,t"]==mu["0,t"]*"+0.5))"), xlab = "Step (t)")
plot(my_mu_H1_nt2[1,], type="l", ylim = c(0, 1.5), ylab = bquote(mu[1]*", "*mu[0]), main = bquote("Non-Stationarity NS2"), xlab = "Step (t)")
lines(my_mu_H1_nt2[2,], type="l", col="red")
legend("topright", legend = c(expression(mu["0,t"]), expression(mu["1,t"])), 
       bty = "n", lty = 1, col=c("black", "red"))

H1_nt2 = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(10,10)), 
                                         true_params = list(mu=my_mu_H1_nt2, sigma2=my_sigma2), clip = 0.1, atleastone = T)))

typeIer_nt2 = mean_typeIer(my_data = H0_nt2, 
                           which_test="Z_BOLS", 
                           batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                           plot_batches = my_plot_batches3)

power_nt2 = mean_power(my_data = H1_nt2, 
                       which_test="Z_BOLS", 
                       batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                       plot_batches = my_plot_batches3,
                       #Prob_adj_typeI = typeIer_nt2$Prob_adj
                       Prob_adj_dx = typeIer_nt2$Prob_adj_dx,
                       Prob_adj_sx = typeIer_nt2$Prob_adj_sx
)

