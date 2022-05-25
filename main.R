############################################
## Run Simulations &/or Perform Inference ##
############################################


# Load packages and necessary functions ----------------------------
library(tidyr)
library(pbapply)

source("simulation_inference_reward_functions.R")

# Main function for simulating Normal-TS data and performing inference -----------------------------

do_inference = function(which_data="standard_TS", which_test = "Pi_test", M, my_N, my_nb, my_mu, my_clip, my_batches, my_alpha = 0.05, power = F){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # which_data: if available, list of M dbs with the M trial data by batch obtained with TS, each containing:
                  # variable "Batch": the batch (i.e., time or update) number
                  # variable "Arm": selected arm 
                  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
                # If not available specify how data should be simulated, i.e.,:
                  # one of: c("standard_TS", "BOLS_tunedTS")
  # which_test: name of the test statistics to be computed. Can be one of c("Pi_test", "Z_OLS", "Z_BOLS", "Z_AWAIPW")
  # M: number of independent datasets to be generated if they are not available
  # my_N: sample size of each dataset
  # my_nb: batch size of each dataset
  # my_mu: true reward means for generating the datasets (can be a 2-dim vector or a matrix in case of non-stationarity)
  # my_clip: whether any clipping for the probabilities should be applied (always if data are generate for BOLS)
  # my_batches: all batches for each inference should be computed
  # alpha:: significativity level (by default 0.05)
  # power: specify if you want to compute power or typeIerror. If typeIer, power should be set to F
  
  # Function output: a list with 2 elements
  # a vector with the typeI error rates for each of the specified "batches"
  # a plot with the distribution of the test statistic at some fixed batches
  
  # parameters for the printed text
  if(length(my_mu)>2){ 
    arm_diff = my_mu[1,2] - my_mu[1,1]
    stationary = "nonstationary"
  } else {
    arm_diff = my_mu[2]-my_mu[1]
    stationary = "stationary"
  }
  
  
  # Simulate data trajectories on which inference will be performed. 
  if(which_data == "standard_TS"){
    print(paste0("Simulate ", stationary, " data according to standard TS with ", my_clip, " clipping: N=", my_N, "; n=", my_nb, "; Arm diff=", arm_diff))
    data_TS = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(1,1)), 
                                                true_params = list(mu=my_mu, sigma2=c(1,1)), clipping = my_clip, atleastone = F)))
  } else if(which_data == "BOLS_tunedTS"){
    print(paste0("Simulate ", stationary, " data according to BOLS-tuned TS with ", my_clip, " clipping: N=", my_N, "; n=", my_nb, "; Arm diff=", arm_diff))
    data_TS = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(1,1)), 
                                              true_params = list(mu=my_mu, sigma2=c(1,1)), clipping = my_clip, atleastone = T)))
  } else if (typeof(which_data) == "list"){
    data_TS = which_data
  } else {stop("You asked to compute a test, but you do not have data specified! Please specify a DB list or what data you want to simulate")}
  
  
  if(power == F){
    if(which_test=="Pi_test"){
      typeIer = mean_typeIer(my_data = data_TS, which_test="Pi_test", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(which_test=="Z_OLS"){
      typeIer = mean_typeIer(my_data = data_TS, which_test="Z_OLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(which_test=="Z_BOLS"){
      typeIer = mean_typeIer(my_data = data_TS, which_test="Z_BOLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(which_test=="Z_AWAIPW"){
      typeIer = mean_typeIer(my_data = data_TS, which_test="Z_AWAIPW", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } 
    
    res = typeIer 
    
  } else {
    if(which_test=="Pi_test"){
      power = mean_power(my_data = data_TS, which_test="Pi_test", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(which_test=="Z_OLS"){
      power = mean_power(my_data = data_TS, which_test="Z_OLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(which_test=="Z_BOLS"){
      power = mean_power(my_data = data_TS, which_test="Z_BOLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(which_test=="Z_AWAIPW"){
      power = mean_power(my_data = data_TS, which_test="Z_AWAIPW", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    }
    
    res = power
  }
  
  return(res)
}



do_inference_nb1 = function(data_TS, data_clippedTS, data_tunedTS, which_test = c("Pi_test", "Z_OLS", "Z_BOLS", "Z_AWAIPW"), 
                        M, my_N, my_nb, my_mu, my_clip, my_batches, my_alpha = 0.05, power = F){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # data_TS, data_tunedTS: if available, list of M dbs with the M trial data by batch obtained with 
  #                         standard TS (for Pi-test) and tuned TS (BOLS-test), each containing:
  # variable "Batch": the batch (i.e., time or update) number
  # variable "Arm": selected arm 
  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  #                         If not available set data_TS/data_tunedTS = T if you want to simulate them (F if not)
  # which_test:: a vector with the name of the test statistics to be computed. 
  #              Can be any combination of c("Pi_test", "Z_OLS", "Z_BOLS", "Z_AWAIPW")
  # M: number of independent datasets to be generated if they are not available
  # my_N: sample size of each dataset
  # my_nb: batch size of each dataset
  # my_mu: true reward means for generating the datasets (can be a 2-dim vector or a matrix in case of non-stationarity)
  # my_clip: whether any clipping for the probabilities should be applied (always if data are generate for BOLS)
  # my_batches: all batches for each inference should be computed
  # alpha:: significativity level (by default 0.05)
  # power: specify if you want to compute power or typeIerror. If typeIer, power should be set to F
  
  # Function output: a list with 2 elements
  # a vector with the typeI error rates for each of the specified "batches"
  # a plot with the distribution of the test statistic at some fixed batches
  
  # parameters for the printed text
  if(length(my_mu)>2){ 
    arm_diff = my_mu[1,2] - my_mu[1,1]
    stationary = "nonstationary"
  } else {
    arm_diff = my_mu[2]-my_mu[1]
    stationary = "stationary"
  }
  
  # simulate data trajectories on which inference will be performed. If data_TS = T they are simulated.
  if(is.logical(data_TS)){
    if((("Pi_test" %in% which_test)|("Z_OLS" %in% which_test)) & (data_TS == T)){
      print(paste0("Simulate standard TS ", stationary, " data: N=", my_N, "; n=", my_nb, "; Arm diff=", arm_diff))
      data_TS = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(1,1)), 
                                                true_params = list(mu=my_mu, sigma2=c(1,1)), clipping = 0, atleastone = F)))
    }
  }
  
  if(is.logical(data_tunedTS)){
    if((("Z_BOLS" %in% which_test)|("Z_AWAIPW" %in% which_test)) & (data_tunedTS == T)){
      print(paste0("Simulate tuned TS ", stationary, " data: N=", my_N, "; n=", my_nb, "; Arm diff=", arm_diff,"; Clipping = ", my_clip))
      data_tunedTS = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(1,1)), 
                                                     true_params = list(mu=my_mu, sigma2=c(1,1)), clipping = my_clip, atleastone = T)))
    }
  }

  # simulate clipped TS for AW_AIPW if nb=1
  if(is.logical(data_clippedTS)){
    if(("Z_AWAIPW" %in% which_test) & (data_clippedTS == T)){
      print(paste0("Simulate clipped TS ", stationary, " data: N=", my_N, "; n=", my_nb, "; Arm diff=", arm_diff,"; Clipping = ", my_clip))
      data_clippedTS = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(1,1)), 
                                                       true_params = list(mu=my_mu, sigma2=c(1,1)), clipping = my_clip, atleastone = F)))
    }
  }
  
  if(power == F){
    if(("Pi_test" %in% which_test)&(is.logical(data_TS)==F)){
      Pi_test = mean_typeIer(my_data = data_TS, which_test="Pi_test", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Pi_test" %in% which_test)&(is.logical(data_TS)==T)){ 
      stop("You asked to compute the Pi_test, but you do not have data specified or simulated! Please specify the DB name or set data_TS = T if you want to simulate them")
    } else {Pi_test = NA}
    if(("Z_OLS" %in% which_test)&(is.logical(data_TS)==F)){
      Zols_test = mean_typeIer(my_data = data_TS, which_test="Z_OLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Z_OLS" %in% which_test)&(is.logical(data_TS)==T)){ 
      stop("You asked to compute the Z_OLS, but you do not have data specified or simulated! Please specify the DB name or set data_TS = T if you want to simulate them")
    } else {Zols_test = NA}
    if(("Z_BOLS" %in% which_test)&(is.logical(data_tunedTS)==F)){
      Zbols_test = mean_typeIer(my_data = data_tunedTS, which_test="Z_BOLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Z_BOLS" %in% which_test)&(is.logical(data_tunedTS)==T)){ 
      stop("You asked to compute the Z_BOLS, but you do not have data specified or simulated! Please specify the DB name or set data_tunedTS = T if you want to simulate them")
    } else {Zbols_test = NA}
    if(("Z_AWAIPW" %in% which_test)&(is.logical(data_clippedTS)==F)){
      Zawaipw_test = mean_typeIer(my_data = data_clippedTS, which_test="Z_AWAIPW", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Z_AWAIPW" %in% which_test)&(is.logical(data_clippedTS)==T)){ 
      stop("You asked to compute the Z_AWAIPW, but you do not have data specified or simulated! Please specify the DB name or set data_clippedTS = T if you want to simulate them")
    } else {Zawaipw_test = NA}
    
  } else {
    if(("Pi_test" %in% which_test)&(is.logical(data_TS)==F)){
      Pi_test = mean_power(my_data = data_TS, which_test="Pi_test", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Pi_test" %in% which_test)&(is.logical(data_TS)==T)){ 
      stop("You asked to compute the Pi_test, but you do not have data specified or simulated! Please specify the DB name or set data_TS = T if you want to simulate them")
    } else {Pi_test = NA}
    if(("Z_OLS" %in% which_test)&(is.logical(data_TS)==F)){
      Zols_test = mean_power(my_data = data_TS, which_test="Z_OLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Z_OLS" %in% which_test)&(is.logical(data_TS)==T)){ 
      stop("You asked to compute the Z_OLS, but you do not have data specified or simulated! Please specify the DB name or set data_TS = T if you want to simulate them")
    } else {Zols_test = NA}
    if(("Z_BOLS" %in% which_test)&(is.logical(data_tunedTS)==F)){
      Zbols_test = mean_power(my_data = data_tunedTS, which_test="Z_BOLS", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Z_BOLS" %in% which_test)&(is.logical(data_tunedTS)==T)){ 
      stop("You asked to compute the Z_BOLS, but you do not have data specified or simulated! Please specify the DB name or set data_tunedTS = T if you want to simulate them")
    } else {Zbols_test = NA}
    if(("Z_AWAIPW" %in% which_test)&(is.logical(data_clippedTS)==F)){
      Zawaipw_test = mean_power(my_data = data_clippedTS, which_test="Z_AWAIPW", batches = my_batches, alpha = my_alpha, my_nb = my_nb)
    } else if(("Z_AWAIPW" %in% which_test)&(is.logical(data_clippedTS)==T)){ 
      stop("You asked to compute the Z_AWAIPW, but you do not have data specified or simulated! Please specify the DB name or set data_clippedTS = T if you want to simulate them")
    } else {Zawaipw_test = NA}
  }
  res = list(Pi_test = Pi_test, Zols_test = Zols_test, 
             Zbols_test = Zbols_test, Zawaipw_test = Zawaipw_test)
  return(res)
}


# Illustrative example -----------------------------

# set parameters and hyperparameters
M = 10000 # number of MC simulations
my_mu = c(0, 0) # true arm means parameters
my_nb = 1 # batch size (for BOLS works only for batch sizes > 2)
my_N = 100 # sample size
my_clip = 0 # clipping parameter for TS

#define batches for which to perform inference
my_batches = sort(unique(c(1:9, seq(10, (my_N/my_nb), 10))))
pos = my_batches %in% c(1:9, seq(10, 80, 10))


# simulate TS data and do inference with Pi-test
Pi_nb1_H0 = do_inference(which_data="BOLS_tunedTS", which_test = "Pi_test", M, my_N, my_nb, my_mu, my_clip, my_batches, my_alpha = 0.05, power = F)


my_mu = c(0, 0.75) # true arm means parameters
Pi_nb1_H1 = do_inference_nb1(data_TS = T, data_clippedTS = F, data_tunedTS = F, which_test = c("Pi_test"), 
                             M, my_N, my_nb, my_mu, my_clip=0.0, my_batches, power = T)


Hadad_nb1_H1 = do_inference_nb1(data_TS = F, data_clippedTS = T, data_tunedTS = F, which_test = c("Z_AWAIPW"), 
                             M, my_N, my_nb, my_mu, my_clip=0.01, my_batches, power = T)


my_mu = c(0, 0) # true arm means parameters
Hadad_nb1_H0 = do_inference_nb1(data_TS = F, data_clippedTS = T, data_tunedTS = F, which_test = c("Z_AWAIPW"), 
                                M, my_N, my_nb, my_mu, my_clip=0.01, my_batches, power = F)
