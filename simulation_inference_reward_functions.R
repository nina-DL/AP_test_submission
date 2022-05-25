############################################
## Script with all the useful functions   ##
## 1. Simulating data with Normal-TS:     ##
##    - Stationary and Non-stationary     ##
## 2. Doing inference (superiority test): ##
##    - OLS, BOLS, AW-AIPW, AP-test       ##
## 3. Computing performance data:         ##
##    - Reward & Optimal arm allocation   ##
############################################

# 1. Simulating data trajectories with Normal-TS ----------------------
library(pbapply)

# MC estimate of the allocation probabilities
rho1_est_NormNorm = function(M=10000, means, sds){
  # this function provides MC estimates of TS arm1 allocation probability in a 2-arm case
  # M:: number of MC simulations
  # means:: 2-dim vector with posterior means of the two arms at time t
  # sds:: 2-dim vector with posterior standard deviations of the two arms at time t
  arm1 = rnorm(M, means[1], sds[1])
  arm2 = rnorm(M, means[2], sds[2])
  return(rho_hat = sum(arm1>arm2)/M)
}


# TS with Normal rewards
TS_NormNorm = function(n_arms = 2, N, n_b, normal_priors = list(mu=c(0,0), sigma2=c(1,1)), 
                       true_params = list(mu, sigma2), clip = 0, atleastone = F){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # N:: total sample size
  # n_b:: (fixed) batch size
  # normal_priors:: a list of two vectors, i.e., mu:: prior means (2-dim vector) and sigma2:: variances (2-dim vector)
  # true_params:: a list of two elements, i.e., mu:: reward true means and sigma2:: reward variances (2-dim vector)
  #               In case of stationarity mu is a 2-dim vector (one mean for each arm)
  #               In case of non-stationarity mu is a (Nbatches X n_arms) matrix (one mean at each batch for each arm)
  # clip:: forces for a minimum probability of selecting each arm. 
    #By default it is 0 (no clipping); 
    #If clip = 0.5 it is equivalent to equal randomization
    #If you want adaptive clipping according to Hadad et al. 2021, set clip = "adaptive"
  # atleastone:: parameter for tuning TS for the BOLS, i.e., ensure at least one arm sampled in each batch
  
  # Function output: a dataframe with N rows and the following 4 columns
  # Batch:: number of current batch
  # Arm:: selected arm
  # Reward:: reward associated to the selected arm
  # AlloProb1:: allocation probability of arm 1
  
  Nbatches = floor(N/n_b)
  
  #initialize normal parameters based on the prior
  normal_params = list(mu=matrix(NA, nrow=Nbatches, ncol=length(normal_priors)),
                       sigma2=matrix(NA, nrow=Nbatches, ncol=length(normal_priors)))
  
  normal_params$mu = normal_priors$mu
  normal_params$sigma2 = normal_priors$sigma2
  
  y=c()
  a_hat = c()
  batch = c()
  rho_1 = c()
  for (i in 1:Nbatches){
    range = (i*n_b+1-n_b):(i*n_b)
    batch[range] = i 
    
    #this applies in case of non-stationarity, when the true mean change with time
    if(length(true_params$mu)>n_arms){
      mu_t = true_params$mu[,i]
    } else {mu_t = true_params$mu}
    
    #update adaptive clipping based on iteration
    if(clip == "adaptive"){clipping = 1/n_arms*(i)^(-0.7)} else {clipping = clip}
    
    # check is clipping is NA or not. If it is NA alloc prob is not computed and standard TS is followed.
    # if is not NA, but 0, the allocation prob is computed.
    if(!is.na(clipping)){
      rho1_hat = rho1_est_NormNorm(means = normal_params$mu, sds = sqrt(normal_params$sigma2))
      # sample arm according to clipping probs; this is equivalent to rho1 = max(clipping, min(rho1_hat, 1-clipping))
      rho1 = min((1-clipping), max(rho1_hat, clipping))
      a_hat[range] = sample(1:2, n_b, prob = c(rho1, (1-rho1)), replace = T)
      rho_1[range] = rho1
    } else {
      mu_hat = replicate(n_b, rnorm(n_arms, normal_params$mu, sqrt(normal_params$sigma2)))
      a_hat[range] = (mu_hat[1,] < mu_hat[2,])*1 + 1
      rho_1[range] = NA
    }
    
    #this ensures that in each batch, each arm is sampled at least once
    if((atleastone == T)&(length(unique(a_hat[range]))!=n_arms)&(n_b>1)){
      missing = setdiff(1:n_arms, unique(a_hat[range]))
      a_hat[sample(range, length(missing))] = missing
    }
    #however, if batch size is 1 this cannot be performed --> stop
    if((atleastone == T)&(n_b==1)){
      stop("You have a batch size of 1, but you asked to have each arm sampled at least one in each batch! \n 
           Please change the batch size or set atleastone == F")
    }
    
    y[range] = rnorm(n_b, mean = mu_t[a_hat[range]], sd = sqrt(true_params$sigma2[a_hat[range]]))
    n=c(sum(a_hat==1), sum(a_hat==2))
    
    y_mean = c(mean(y[a_hat==1]), mean(y[a_hat==2]))
    y_mean[is.na(y_mean)] = 0
    
    if(i < Nbatches){
      normal_params$sigma2 = 1/(1/normal_priors$sigma2 + n/true_params$sigma2)
      normal_params$mu = normal_params$sigma2*(normal_priors$mu/normal_priors$sigma2+y_mean*n/(true_params$sigma2))
    }
    
  }
  return(data.frame(Batch = batch, Arm = a_hat, Reward = y, AlloProb1 = rho_1))
}


# 2a. Doing inference: Computing test statistics for a single data traj ----------------

# Pi-test statistic -------------------

compute_Pi_stat = function(my_data, to_batch, from_batch = 1, exp_arm=F){
  
  # Note: this function is for the two-arm case (n_arms = 2)
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
    # variable "Batch": the batch (i.e., time or update) number
    # variable "Arm": selected arm 
    # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  # from_batch:: first batche starting from which the test has to be computed
  # exp_arm:: allocation probability ("AlloProb1") refers to the experimental arm (T) or not (F)
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # N1:: number of times arm 1 was selected
  # N2:: number of times arm 2 was selected
  # Pi_stat:: value of the test statistic
  
  my_data_to_batch = my_data[my_data$Batch %in% from_batch:to_batch,]
  N1 = nrow(my_data_to_batch[my_data_to_batch$Arm==1,])
  N2 = nrow(my_data_to_batch[my_data_to_batch$Arm==2,])
  
  allo_prob = aggregate(AlloProb1 ~ Batch, my_data_to_batch, mean)$AlloProb1
  
  if(exp_arm==T){
    alphas = (allo_prob>.5)*1
  } else {
    alphas = ((1-allo_prob)>.5)*1
  }
  
  return(c(Batch = to_batch, N1 = N1, N2 = N2, Pi_stat = sum(alphas)))
}


# OLS based Z-test statistic -------------------

# The two functions produce the same output, but v2 is based on R's lm function (and is slower)
# Use the first one for time efficiency

compute_Z_ols = function(my_data, to_batch, sigma2 = "unknown", exp_arm=2){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
    # variable "Batch": the batch (i.e., time or update) number
    # variable "Arm": selected arm 
    # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  # sigma2:: assumption on reward variance. Can have a positive value if it is known, otherwise "unknown".
  # exp_arm:: the experimental arm (1 or 2) 
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # N1, N2:: number of times arm 1 and arm 2 were selected
  # OLS_1; OLS_2:: OLS estimate of arm 1 and amr 2 
  # Z_OLS_diff:: value of the Z-test statistic for arm differences
  
  # Reduce DB up to batch "to_batch"
  my_data_to_batch = my_data[my_data$Batch %in% 1:to_batch,]
  
  # get OLS estimates = sample mean for normal reward
  # we assume (simulated adaptive data accordigly) that we have N1, N2 > 0
  S1 = sum(my_data_to_batch[my_data_to_batch$Arm==1,]$Reward)
  S2 = sum(my_data_to_batch[my_data_to_batch$Arm==2,]$Reward)
  
  N1 = nrow(my_data_to_batch[my_data_to_batch$Arm==1,])
  N2 = nrow(my_data_to_batch[my_data_to_batch$Arm==2,])
  
  OLS_1 = S1/N1
  OLS_2 = S2/N2
  
  # compute residuals variance to be used as estimated sigma2 when variance is unkown
  if(sigma2 == "unknown"){
    residuals = my_data_to_batch$Reward - OLS_1*(my_data_to_batch$Arm==1) - OLS_2*(my_data_to_batch$Arm==2)
    s2_res = sum(residuals^2)/(N1+N2-2) # 2 is the dof
  } else{
    s2_res = sigma2
  }
  
  #compute Z test stat as in Zhang: (ols_diff - true_diff)/sqrt((N*s2)/(N1*N2))
  #they use a sort of pooled version
  SE_est = sqrt(s2_res*(N1 + N2)/(N1*N2))
  
  if(exp_arm==2){
    Z_OLS_diff = (OLS_2 - OLS_1)/SE_est
  } else {
    Z_OLS_diff = (OLS_1 - OLS_2)/SE_est
  }
  
  return(c(Batch = to_batch, N1 = N1, N2 = N2, OLS_1 = OLS_1, OLS_2 = OLS_2, Z_OLS_diff = Z_OLS_diff))
}

compute_Z_ols_v2 = function(my_data, to_batch, sigma2 = "unknown", exp_arm=2){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
  # variable "Batch": the batch (i.e., time or update) number
  # variable "Arm": selected arm 
  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  # sigma2:: assumption on reward variance. Can have a positive value if it is known, otherwise "unknown".
  # exp_arm:: the experimental arm (1 or 2) 
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # N1, N2:: number of times arm 1 and arm 2 were selected
  # ols1; ols2:: OLS estimate of arm 1 and amr 2 
  # Z_OLS_diff:: value of the Z-test statistic for arm differences
  
  #create binary arm variables to fit regression model
  my_data$Arm2 = my_data$Arm -1 
  my_data$Arm1 = abs(my_data$Arm2 -1) 
  
  #fit linear model without intercept (as in Zhang)
  res = lm(Reward ~ Arm1 + Arm2 - 1, data = my_data[my_data$Batch %in% 1:to_batch,])
  
  #estimate std when unknown
  if(sigma2 == "unknown"){
    s2_res = sigma(res)^2
  } else{
    s2_res = sigma2
  }
  
  N1 = sum(my_data[my_data$Batch %in% 1:to_batch,]$Arm1)
  N2 = sum(my_data[my_data$Batch %in% 1:to_batch,]$Arm2)
  
  #compute Z test stat as in Zhang: (ols_diff - true_diff)/sqrt((N*s2)/(N1*N2))
  #they use a sort of pooled version
  SE_est = sqrt(s2_res*(N1 + N2)/(N1*N2))
  
  if(exp_arm==2){
    Z_OLS_diff = (res$coef[2] - res$coef[1])/SE_est
  } else {
    Z_OLS_diff = (res$coef[1] - res$coef[2])/SE_est
  }
  
  return(c(Batch = to_batch, N1 = N1, N2 = N2, ols1 = res$coef[1], ols2 = res$coef[2], Z_OLS_diff = Z_OLS_diff))
}


# BOLS based Z-test statistic -------------------

# The three functions produce the same output, but v2 is based on R's lm function (and is the slowest)
# Use the first one for time efficiency

compute_Z_bols = function(my_data, to_batch, sigma2 = "unknown", exp_arm=2){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
    # variable "Batch": the batch (i.e., time or update) number
    # variable "Arm": selected arm 
    # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  # sigma2:: assumption on reward variance. Can have a positive value if it is known, otherwise "unknown".
  # exp_arm:: the experimental arm (1 or 2) 
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # N1, N2:: number of times arm 1 and arm 2 were selected
  # Z_BOLS_diff:: value of the Z-test statistic for arm differences
  
  Zb_OLS_diff = c()
  s2_res = c()
  N1 =c(); N2 =c()
  for(i in 1:to_batch){
    # Reduce DB to only batch i
    my_data_to_batch = my_data[my_data$Batch %in% i,]
    
    # get OLS estimates = sample mean for normal reward
    # we assume (simulated adaptive data accordigly) that we have N1, N2 > 0
    S1 = sum(my_data_to_batch[my_data_to_batch$Arm==1,]$Reward)
    S2 = sum(my_data_to_batch[my_data_to_batch$Arm==2,]$Reward)
    
    N1[i] = nrow(my_data_to_batch[my_data_to_batch$Arm==1,])
    N2[i] = nrow(my_data_to_batch[my_data_to_batch$Arm==2,])
    
    OLS_1 = S1/N1[i]
    OLS_2 = S2/N2[i]
    
    #get the ols diff in each batch
    if(exp_arm==2){
      Zb_OLS_diff[i] = OLS_2 - OLS_1
    } else {
      Zb_OLS_diff[i] = OLS_1 - OLS_2
    }
    
    if(sigma2 == "unknown"){
      #get the residuals and estimated variance in each batch
      residuals_b = my_data_to_batch$Reward - OLS_1*(my_data_to_batch$Arm==1) - OLS_2*(my_data_to_batch$Arm==2)
      s2_res[i] = sum(residuals_b^2)/(length(residuals_b)-2) # 2 is the dof
    } else{
      s2_res[i] = sigma2
    }
    
  }
  
  #compute Z BOLS test stat as in Zhang: (ols_diff - true_diff)/sqrt((N*s2)/(N1*N2))
  #and average among them
  SE_est = sqrt(s2_res*(N1 + N2)/(N1*N2))
  
  Z_BOLS_diff = sum(Zb_OLS_diff/SE_est)/sqrt(to_batch)
  
  return(c(Batch = to_batch, N1 = sum(N1), N2 = sum(N2), Z_BOLS_diff = Z_BOLS_diff))
}

compute_Z_bols_v2 = function(my_data, to_batch, sigma2 = "unknown"){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
  # variable "Batch": the batch (i.e., time or update) number
  # variable "Arm": selected arm 
  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  # sigma2:: assumption on reward variance. Can have a positive value if it is known, otherwise "unknown".
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # N1, N2:: number of times arm 1 and arm 2 were selected
  # Z_BOLS_diff:: value of the Z-test statistic for arm differences
  
  #create binary arm variables to fit regression model
  my_data$Arm2 = my_data$Arm -1 
  my_data$Arm1 = abs(my_data$Arm2 -1) 
  
  Zb_BOLS_diff = c()
  for(i in 1:to_batch){
    #fit linear model without intercept (as in Zhang) in the i batch 
    res = lm(Reward ~ Arm1 + Arm2 - 1, data = my_data[my_data$Batch %in% i,])
    
    #estimate std when unknown
    if(sigma2 == "unknown"){
      s2_res = sigma(res)^2
    } else{
      s2_res = sigma2
    }
    
    N1 = sum(my_data[my_data$Batch %in% i,]$Arm1)
    N2 = sum(my_data[my_data$Batch %in% i,]$Arm2)
    
    #compute Z test stat as in Zhang: (ols_diff - true_diff)/sqrt((N*s2)/(N1*N2))
    #they use a sort of pooled version
    SE_est = sqrt(s2_res*(N1 + N2)/(N1*N2))
    
    #get the ols diff standardized by the SE in each batch
    if(exp_arm==2){
      Zb_BOLS_diff[i] = (res$coef[2] - res$coef[1])/SE_est
    } else {
      Zb_BOLS_diff[i] = (res$coef[1] - res$coef[2])/SE_est
    }
    
  }
  
  Z_BOLS_diff = sum(Zb_BOLS_diff)/sqrt(to_batch)
  
  N1 = sum(my_data[my_data$Batch %in% 1:to_batch,]$Arm1)
  N2 = sum(my_data[my_data$Batch %in% 1:to_batch,]$Arm2)
  
  return(c(Batch = to_batch, N1 = N1, N2 = N2, Z_BOLS_diff = Z_BOLS_diff))
}

compute_Z_bols_v3 = function(my_data, to_batch, sigma2 = "unknown", exp_arm=2){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
  # variable "Batch": the batch (i.e., time or update) number
  # variable "Arm": selected arm 
  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  # sigma2:: assumption on reward variance. Can have a positive value if it is known, otherwise "unknown"
  # exp_arm:: the experimental arm (1 or 2) 
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # N1, N2:: number of times arm 1 and arm 2 were selected
  # Z_BOLS_diff:: value of the Z-test statistic for arm differences
  
  Zb_OLS_diff = c()
  s2_res = c()
  N1 =c(); N2 =c()
  for(i in 1:to_batch){
    # Reduce DB to only batch i
    my_data_to_batch = my_data[my_data$Batch %in% i,]
    
    # get OLS estimates = sample mean for normal reward
    # we assume (simulated adaptive data accordigly) that we have N1, N2 > 0
    S1 = sum(my_data_to_batch[my_data_to_batch$Arm==1,]$Reward)
    S2 = sum(my_data_to_batch[my_data_to_batch$Arm==2,]$Reward)
    
    N1[i] = nrow(my_data_to_batch[my_data_to_batch$Arm==1,])
    N2[i] = nrow(my_data_to_batch[my_data_to_batch$Arm==2,])
    
    OLS_1 = S1/N1[i]
    OLS_2 = S2/N2[i]
    
    #get the ols diff standardized by sample sizes in each batch
    if(exp_arm==2){
      Zb_OLS_diff[i] = (OLS_2 - OLS_1)/sqrt((N1[i]+N2[i])/(N1[i]*N2[i]))
    } else {
      Zb_OLS_diff[i] = (OLS_1 - OLS_2)/sqrt((N1[i]+N2[i])/(N1[i]*N2[i]))
    }
    
    
    if(sigma2 == "unknown"){
      #get the residuals and estimated variance in each batch
      residuals_b = my_data_to_batch$Reward - OLS_1*(my_data_to_batch$Arm==1) - OLS_2*(my_data_to_batch$Arm==2)
      s2_res[i] = sum(residuals_b^2)/(length(residuals_b)-2) # 2 is the dof
    } else{
      s2_res[i] = sigma2
    }
    
  }
  
  #compute Z BOLS test stat as in Zhang: (ols_diff - true_diff)/sqrt((N*s2)/(N1*N2))
  #and average among them
  Z_BOLS_diff = sum(Zb_OLS_diff/sqrt(s2_res))/sqrt(to_batch)
  
  return(c(Batch = to_batch, N1 = sum(N1), N2 = sum(N2), Z_BOLS_diff = Z_BOLS_diff))
}

# AW_AIPW Z-test statistic -------------------

#According to original hadad's paper
compute_Z_AWAIPW = function(my_data, to_batch){
  # Note: this function is for the two-arm case (n_arms = 2) and normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
    # variable "Batch": the batch (i.e., time or update) number
    # variable "Arm": selected arm 
    # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # AI_AIPW1, AI_AIPW2:: AI-AIPW estimates of arm 1 and arm 2
  # Z_AWAIPW_diff:: value of the Z-test statistic for arm differences
  
  
  # reduce DB to batch of interest
  my_data_to_batch = my_data[my_data$Batch %in% 1:to_batch,]
  
  # get the IPW portion at each time t
  my_data_to_batch$ipw1 = (my_data_to_batch$Arm==1)*(my_data_to_batch$Reward)/(my_data_to_batch$AlloProb1)
  my_data_to_batch$ipw2 = (my_data_to_batch$Arm==2)*(my_data_to_batch$Reward)/(1-my_data_to_batch$AlloProb1)

  my_data_to_batch$mu2_hat = my_data_to_batch$mu1_hat = c(NA)
  my_data_to_batch$gamma2_hat = my_data_to_batch$gamma1_hat = c(NA)
  for (i in 2:nrow(my_data_to_batch)){
    # get the augumented estimate portion at each time t
    # it doeas not apply to t=1 as it is based on the sum up to t-1
    my_data_to_batch[i,]$mu1_hat = (1-(my_data_to_batch[i,]$Arm==1)/my_data_to_batch[i,]$AlloProb1)*sum((my_data_to_batch[1:(i-1),]$Arm==1)*(my_data_to_batch[1:(i-1),]$Reward), na.rm = T)/sum(my_data_to_batch[1:(i-1),]$Arm==1)
    my_data_to_batch[i,]$mu2_hat = (1-(my_data_to_batch[i,]$Arm==2)/(1-my_data_to_batch[i,]$AlloProb1))*sum((my_data_to_batch[1:(i-1),]$Arm==2)*(my_data_to_batch[1:(i-1),]$Reward), na.rm = T)/sum(my_data_to_batch[1:(i-1),]$Arm==2)
    
    # get the estimated value (Gamma)
    my_data_to_batch[i,]$gamma1_hat = sum(my_data_to_batch[i,]$ipw1, my_data_to_batch[i,]$mu1_hat, na.rm = T)
    my_data_to_batch[i,]$gamma2_hat = sum(my_data_to_batch[i,]$ipw2 + my_data_to_batch[i,]$mu2_hat, na.rm = T)
    
  }
  
  AI_AIPW1 = sum(sqrt(my_data_to_batch$AlloProb1)*my_data_to_batch$gamma1_hat, na.rm = T)/sum(sqrt(my_data_to_batch$AlloProb1))
  AI_AIPW2 = sum(sqrt(1-my_data_to_batch$AlloProb1)*my_data_to_batch$gamma2_hat, na.rm = T)/sum(sqrt(1-my_data_to_batch$AlloProb1))
  
  V1_hat = sum(my_data_to_batch$AlloProb1*(my_data_to_batch$gamma1_hat-AI_AIPW1)^2, na.rm = T)/sum(sqrt(my_data_to_batch$AlloProb1))^2
  V2_hat = sum((1-my_data_to_batch$AlloProb1)*(my_data_to_batch$gamma2_hat-AI_AIPW2)^2, na.rm = T)/sum(sqrt(1-my_data_to_batch$AlloProb1))^2
    
  Var_est = V1_hat + V2_hat
    
  Z_AWAIPW_diff = (AI_AIPW2 - AI_AIPW1)/sqrt(Var_est)

  return(c(Batch = to_batch, AI_AIPW1 = AI_AIPW1, AI_AIPW2 = AI_AIPW2, Z_AWAIPW_diff = Z_AWAIPW_diff))
}

#According to zhang's paper (a covariance estimate is introduced)
compute_Z_AWAIPW_v2 = function(my_data, to_batch){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
  # variable "Batch": the batch (i.e., time or update) number
  # variable "Arm": selected arm 
  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  
  # Function output: a 4-dim vector with
  # Batch:: (= to_batch) number of batches considered for the test computation
  # AI_AIPW1, AI_AIPW2:: AI-AIPW estimates of arm 1 and arm 2
  # Z_AWAIPW_diff:: value of the Z-test statistic for arm differences
  
  # reduce DB to batch of interest
  my_data_to_batch = my_data[my_data$Batch %in% 1:to_batch,]
  
  #my_data_to_batch$mu2_hat = my_data_to_batch$mu1_hat = c(NA)
  #my_data_to_batch$gamma2_hat = my_data_to_batch$gamma1_hat = c(NA)
  
  Y1 = Y2 = c(NA)
  for (i in 2:nrow(my_data_to_batch)){
    # get the IPW portion at each time t
    ipw1 = (my_data_to_batch[i,]$Arm==1)*(my_data_to_batch[i,]$Reward)/(my_data_to_batch[i,]$AlloProb1)
    ipw2 = (my_data_to_batch[i,]$Arm==2)*(my_data_to_batch[i,]$Reward)/(1-my_data_to_batch[i,]$AlloProb1)
    
    # get the augumented estimate portion at each time t
    # it doeas not apply to t=1 as it is based on the sum up to t-1
    w1 = (1-(my_data_to_batch[i,]$Arm==1)/my_data_to_batch[i,]$AlloProb1)
    w2 = (1-(my_data_to_batch[i,]$Arm==2)/(1-my_data_to_batch[i,]$AlloProb1))
    mu1_hat = w1*sum((my_data_to_batch[1:(i-1),]$Arm==1)*(my_data_to_batch[1:(i-1),]$Reward), na.rm = T)/sum(my_data_to_batch[1:(i-1),]$Arm==1)
    mu2_hat = w2*sum((my_data_to_batch[1:(i-1),]$Arm==2)*(my_data_to_batch[1:(i-1),]$Reward), na.rm = T)/sum(my_data_to_batch[1:(i-1),]$Arm==2)
    
    Y1[i] = ipw1 + mu1_hat
    Y2[i] = ipw2 + mu2_hat
  
  }
  
  
  AI_AIPW1 = sum(sqrt(my_data_to_batch$AlloProb1)*Y1, na.rm = T)/sum(sqrt(my_data_to_batch$AlloProb1))
  AI_AIPW2 = sum(sqrt(1-my_data_to_batch$AlloProb1)*Y2, na.rm = T)/sum(sqrt(1-my_data_to_batch$AlloProb1))
  
  V1_hat = sum(my_data_to_batch$AlloProb1*(Y1-AI_AIPW1)^2, na.rm = T)/sum(sqrt(my_data_to_batch$AlloProb1))^2
  V2_hat = sum((1-my_data_to_batch$AlloProb1)*(Y2-AI_AIPW2)^2, na.rm = T)/sum(sqrt(1-my_data_to_batch$AlloProb1))^2
  Cov_hat = sum(sqrt(my_data_to_batch$AlloProb1*(1-my_data_to_batch$AlloProb1))*(Y1-AI_AIPW1)*(Y2-AI_AIPW2), na.rm = T)/(sum(sqrt(my_data_to_batch$AlloProb1))*sum(sqrt(1-my_data_to_batch$AlloProb1)))
  
  Var_est = V1_hat + V2_hat - 2*Cov_hat
  
  Z_AWAIPW_diff = (AI_AIPW2 - AI_AIPW1)/sqrt(Var_est)
  
  return(c(Batch = to_batch, AI_AIPW1 = AI_AIPW1, AI_AIPW2 = AI_AIPW2, Z_AWAIPW_diff = Z_AWAIPW_diff))
}




# 2b. Doing inference: Computing Type-I Error & Power (superiority test) ----------------


compute_probs_RandTest = function(distr_values, alpha=0.05){
  Pi_cdf_rev = rev(cumsum(rev(distr_values)))
  
  #search for the non-extreme cutoff closer but higher to a 0.05 error
  cutoff_density = min(Pi_cdf_rev[(Pi_cdf_rev-alpha)>0])
  cutoff = as.numeric(names(which(Pi_cdf_rev==cutoff_density)))
  
  left_prob = sum(distr_values[as.numeric(names(distr_values))>cutoff])
  
  # save cutoff with its density and define the adjustment
  rand_adj = c(cutoff, cutoff_density, (alpha-left_prob)/(cutoff_density-left_prob))
  names(rand_adj) = c("Cutoff", "Cutoff cumulative density", "Correction probability")
  
  return(rand_adj)
}

correct_rejection_discr = Vectorize(function(cutoff, k, Pi_stat_val, prob){
  if(cutoff == k){
    if(Pi_stat_val == k){ 
      reject = (runif(1, 0,1)<=prob)*1
    } else {
      reject = 0
    }
    
  } else {
    if(Pi_stat_val > cutoff){ 
      reject = 1
      } else if (Pi_stat_val < cutoff){ 
        reject = 0
      } else {
        reject = (runif(1, 0,1)<=prob)*1
      }
        
  }
  return(reject)
})

correct_rejection_cont = Vectorize(function(Stat_val, cutoff, probdx, probsx){
  if(!is.na(Stat_val)){
    if(probdx>0){
      reject = 0*(Stat_val <= cutoff) + 1*(runif(1, 0,1)<=probdx)*(Stat_val > cutoff)
    } else {
      reject = 0*(runif(1, 0,1)<=probsx)*(Stat_val <= cutoff) + 1*(Stat_val > cutoff)
    } 
    } else {reject = NA}

  return(reject)
})


mean_typeIer = function(my_data, which_test, batches, alpha=0.05, my_nb, M,
                        plot_batches = c(3, 5, 8, 10, 20, 30, 50, 70, 90), adj_typeI = T){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: list of M dbs with the M trial data by batch, containing:
    # variable "Batch": the batch (i.e., time or update) number
    # variable "Arm": selected arm 
    # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # which_test:: a vector with the name of the test statistics to be computed. 
  #              Can be any of c("Pi_test", "Z_OLS", "Z_BOLS", "Z_AWAIPW")
  # batches:: specific batches numbers for which to compute typeIer
  # plot_batches:: specific 9 batches numbers for which to plot the test statistic distribution
  # alpha:: significativity level (by default 0.05)
  # my_nb:: number of batches
  
  # Function output: a list with 2 elements
  # a vector with the typeI error rates for each of the specified "batches"
  # a plot with the distribution of the test statistic at some fixed batches
  
  par(mfrow = c(3,3))
  
  if(which_test=="Pi_test"){
    
    Pi_H0_distr = list()
    Prob_adj = list()
    Pi_type_I_er = c()
    Pi_type_I_er_adj = c()
    Pi_type_I_er_se = c()
    Pi_type_I_er_adj_se = c()
    for(k in batches){
      #print(paste0("Computing Pi-test Type-I Error for Batch ", k))
      # compute Pi statistic & associated typeIer at batch k
      my_Pi = pblapply(my_data, compute_Pi_stat, to_batch = k+1, from_batch = 2)
      my_Pi = data.frame(matrix(unlist(my_Pi), nrow=length(my_Pi), byrow=TRUE))
      names(my_Pi) = c("Batches", "N1", "N2", "Pi_stat")
      
      varname = paste("batch", as.character(k), sep="_")
      Pi_H0_distr[[varname]] = table(my_Pi$Pi_stat)/M
      
      Pi_type_I_er = c(Pi_type_I_er, sum(my_Pi$Pi_stat>(k-1))/M)
      Pi_type_I_er_se = c(Pi_type_I_er_se, round(sd(my_Pi$Pi_stat>(k-1))/sqrt(M), 3))
      
      print(paste0("Pi-test Type-I Error for Batch ", k, " = ", Pi_type_I_er[length(Pi_type_I_er)], " (SE: ",Pi_type_I_er_se[length(Pi_type_I_er_se)], ")"))
      
      if(adj_typeI == T){
        Prob_adj[[varname]] = compute_probs_RandTest(Pi_H0_distr[[varname]], alpha = alpha)
        
        Pi_type_I_er_adj = c(Pi_type_I_er_adj, sum(correct_rejection_discr(cutoff = Prob_adj[[varname]][1], k=k, my_Pi$Pi_stat, prob = Prob_adj[[varname]][3]))/M)
        Pi_type_I_er_adj_se = c(Pi_type_I_er_adj_se, round(sd(correct_rejection_discr(cutoff = Prob_adj[[varname]][1], k=k, my_Pi$Pi_stat, prob = Prob_adj[[varname]][3]))/sqrt(M), 3))
        print(paste0("Pi-test Adjusted Type-I Error for Batch ", k, " = ", Pi_type_I_er_adj[length(Pi_type_I_er_adj)], " (SE: ",Pi_type_I_er_adj_se[length(Pi_type_I_er_adj_se)], ")"))
        
      }
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Pi_type_I_er[length(Pi_type_I_er)],3)
        er_adj = round(Pi_type_I_er_adj[length(Pi_type_I_er_adj)],3)
        barplot(table(my_Pi$Pi_stat)/M, main = bquote(Pi*"-test"~"distribution under"~H[0]), 
                xlab = "", density=65, cex.axis = 0.7, cex.names=0.9)
      
        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Type-I ER ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
        
        }
  
    }
    Pi_H0_distrPlot = recordPlot()
    res = list(Pi_type_I_er = Pi_type_I_er, Pi_type_I_er_se = Pi_type_I_er_se, Pi_type_I_er_adj = Pi_type_I_er_adj, Pi_type_I_er_adj_se = Pi_type_I_er_adj_se, Prob_adj = Prob_adj, Pi_H0_distrPlot = Pi_H0_distrPlot)
    
  } else if(which_test=="Z_OLS"){
    
    #Zols_type_I_er = c()
    #Zols_type_I_er_s2est = c()
    Zols_type_I_er_s2est_tcutoff = c()
    Zols_type_I_er_adj = c()
    Prob_adj_dx = c()
    Prob_adj_sx = c()
    Zols_type_I_er_se = c()
    Zols_type_I_er_adj_se = c()
    for(k in batches){
      #print(paste0("Computing OLS Type-I Error for Batch ", k))
      # compute Zols statistic (for sigma2 known) & associated type-I error at batch k
      # my_Zols = pblapply(my_data, compute_Z_ols, to_batch = k, sigma2 = 1)
      # my_Zols = data.frame(matrix(unlist(my_Zols), nrow=length(my_Zols), byrow=TRUE))
      # compute Zols statistic (for sigma2 known unknown) & associated type-I error at batch k
      my_Zols_s2est = pblapply(my_data, compute_Z_ols, to_batch = k, sigma2 = "unknown")
      my_Zols_s2est = data.frame(matrix(unlist(my_Zols_s2est), nrow=length(my_Zols_s2est), byrow=TRUE))
      #names(my_Zols) = names(my_Zols_s2est) = c("Batches", "N1", "N2", "OLS1", "OLS2", "Zols_stat")
      names(my_Zols_s2est) = c("Batches", "N1", "N2", "OLS1", "OLS2", "Zols_stat")
      
      #Zols_type_I_er = c(Zols_type_I_er, sum(my_Zols$Zols_stat>cutoff)/M)
      #Zols_type_I_er_s2est = c(Zols_type_I_er_s2est, mean(my_Zols_s2est$Zols_stat>qnorm(1-alpha), na.rm = T))
      
      if(my_nb*k > 2){
        t_cutoff = qt(1-alpha, df = my_nb*k-2)
      } else {t_cutoff = qnorm(1-alpha)}
      
      Zols_type_I_er_s2est_tcutoff = c(Zols_type_I_er_s2est_tcutoff, mean(my_Zols_s2est$Zols_stat>t_cutoff, na.rm = T))
      Zols_type_I_er_se = c(Zols_type_I_er_se, round(sd(my_Zols_s2est$Zols_stat>t_cutoff, na.rm = T)/sqrt(M), 3))
      print(paste0("OLS-test Type-I Error for Batch ", k, " = ", Zols_type_I_er_s2est_tcutoff[length(Zols_type_I_er_s2est_tcutoff)], " (SE: ",Zols_type_I_er_se[length(Zols_type_I_er_se)], ")"))
      
      if(adj_typeI == T){
        varname = paste("batch", as.character(k), sep="_")
        Prob_adj_dx[[varname]] = (alpha/Zols_type_I_er_s2est_tcutoff[length(Zols_type_I_er_s2est_tcutoff)])*(alpha<=Zols_type_I_er_s2est_tcutoff[length(Zols_type_I_er_s2est_tcutoff)])
        Prob_adj_sx[[varname]] = (alpha-Zols_type_I_er_s2est_tcutoff[length(Zols_type_I_er_s2est_tcutoff)])/(1-Zols_type_I_er_s2est_tcutoff[length(Zols_type_I_er_s2est_tcutoff)])*(alpha>Zols_type_I_er_s2est_tcutoff[length(Zols_type_I_er_s2est_tcutoff)])
        
        Zols_type_I_er_adj = c(Zols_type_I_er_adj, sum(correct_rejection_cont(my_Zols_s2est$Zols_stat, cutoff = t_cutoff, 
                                                                              probdx = Prob_adj_dx[length(Prob_adj_dx)], probsx = Prob_adj_sx[length(Prob_adj_sx)]), na.rm = T)/M)
        Zols_type_I_er_adj_se = c(Zols_type_I_er_adj_se, round(sd(correct_rejection_cont(my_Zols_s2est$Zols_stat, cutoff = t_cutoff, 
                                                                                         probdx = Prob_adj_dx[length(Prob_adj_dx)], probsx = Prob_adj_sx[length(Prob_adj_sx)]), na.rm = T)/sqrt(M), 3))
        print(paste0("OLS-test Adjusted Type-I Error for Batch ", k, " = ", Zols_type_I_er_adj[length(Zols_type_I_er_adj)], " (SE: ",Zols_type_I_er_adj_se[length(Zols_type_I_er_adj_se)], ")"))
        
      }
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Zols_type_I_er_s2est_tcutoff[length(Zols_type_I_er_s2est_tcutoff)],3)
        er_adj = round(Zols_type_I_er_adj[length(Zols_type_I_er_adj)],3)
        hist(my_Zols_s2est$Zols_stat, prob = T, main = bquote(Z[OLS]*"-test"~"distribution under"~H[0]), 
             xlab = "", cex.axis = 0.7)
        curve(dnorm(x), add=T, col = "blue")
        abline(v = t_cutoff, col = "red")

        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Type-I ER ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
      }
    }

    Zols_H0_distrPlot = recordPlot()
    res = list(Zols_type_I_er_s2est_tcutoff = Zols_type_I_er_s2est_tcutoff, Zols_type_I_er_se = Zols_type_I_er_se, Zols_type_I_er_adj = Zols_type_I_er_adj, Zols_type_I_er_adj_se = Zols_type_I_er_adj_se, Prob_adj_dx = Prob_adj_dx, Prob_adj_sx = Prob_adj_sx, Zols_H0_distrPlot = Zols_H0_distrPlot)
    
  } else if(which_test=="Z_BOLS"){
    #Zbols_type_I_er = c()
    #Zbols_type_I_er_s2est = c()
    Zbols_type_I_er_s2est_tcutoff = c()
    Zbols_type_I_er_adj = c()
    Zbols_type_I_er_se = c()
    Zbols_type_I_er_adj_se = c()
    Prob_adj_dx = c()
    Prob_adj_sx = c()
    for(k in batches){
      #print(paste0("Computing BOLS Type-I Error for Batch ", k))
      # compute Zols statistic (for sigma2 known and unknown) & associated Power or power at batch k
      #my_Zbols = pblapply(my_data, compute_Z_bols, to_batch = k, sigma2 = 1)
      #my_Zbols = data.frame(matrix(unlist(my_Zbols), nrow=length(my_Zbols), byrow=TRUE))
      
      my_Zbols_s2est = pblapply(my_data, compute_Z_bols, to_batch = k, sigma2 = "unknown")
      my_Zbols_s2est = data.frame(matrix(unlist(my_Zbols_s2est), nrow=length(my_Zbols_s2est), byrow=TRUE))
      #names(my_Zbols) = names(my_Zbols_s2est) = c("Batches", "N1", "N2", "Zbols_stat")
      names(my_Zbols_s2est) = c("Batches", "N1", "N2", "Zbols_stat")
      
      #Zbols_type_I_er = c(Zbols_type_I_er, sum(my_Zbols$Zbols_stat>cutoff)/M)
      #Zbols_type_I_er_s2est = c(Zbols_type_I_er_s2est, sum(my_Zbols_s2est$Zbols_stat>cutoff)/M)
      
      if(my_nb>2){
        # note: being a simulation based strategy, it will not end up always with the same exact result
        t_sim = apply(matrix(rt(1e5*k, df=my_nb-2), ncol = k, nrow = 1e5), 1, sum)/sqrt(k)
        t_cutoff = quantile(t_sim, prob=1-alpha) 
      } else {t_cutoff = qnorm(1-alpha)}
      
      Zbols_type_I_er_s2est_tcutoff = c(Zbols_type_I_er_s2est_tcutoff, mean(my_Zbols_s2est$Zbols_stat>t_cutoff, na.rm = T))
      Zbols_type_I_er_se = c(Zbols_type_I_er_se, round(sd(my_Zbols_s2est$Zbols_stat>t_cutoff, na.rm = T)/sqrt(M), 3))
      print(paste0("BOLS-test Type-I Error for Batch ", k, " = ", Zbols_type_I_er_s2est_tcutoff[length(Zbols_type_I_er_s2est_tcutoff)], " (SE: ",Zbols_type_I_er_se[length(Zbols_type_I_er_se)], ")"))
      
      if(adj_typeI == T){
        varname = paste("batch", as.character(k), sep="_")
        Prob_adj_dx[[varname]] = (alpha/Zbols_type_I_er_s2est_tcutoff[length(Zbols_type_I_er_s2est_tcutoff)])*(alpha<=Zbols_type_I_er_s2est_tcutoff[length(Zbols_type_I_er_s2est_tcutoff)])
        Prob_adj_sx[[varname]] = (alpha-Zbols_type_I_er_s2est_tcutoff[length(Zbols_type_I_er_s2est_tcutoff)])/(1-Zbols_type_I_er_s2est_tcutoff[length(Zbols_type_I_er_s2est_tcutoff)])*(alpha>Zbols_type_I_er_s2est_tcutoff[length(Zbols_type_I_er_s2est_tcutoff)])
        
        Zbols_type_I_er_adj = c(Zbols_type_I_er_adj, sum(correct_rejection_cont(my_Zbols_s2est$Zbols_stat, cutoff = t_cutoff, 
                                                                                probdx = Prob_adj_dx[length(Prob_adj_dx)], probsx = Prob_adj_sx[length(Prob_adj_sx)]), na.rm = T)/M)
        Zbols_type_I_er_adj_se = c(Zbols_type_I_er_adj_se, round(sd(correct_rejection_cont(my_Zbols_s2est$Zbols_stat, cutoff = t_cutoff, 
                                                                                           probdx = Prob_adj_dx[length(Prob_adj_dx)], probsx = Prob_adj_sx[length(Prob_adj_sx)]), na.rm = T)/sqrt(M), 3))
        print(paste0("BOLS-test Adjusted Type-I Error for Batch ", k, " = ", Zbols_type_I_er_adj[length(Zbols_type_I_er_adj)], " (SE: ",Zbols_type_I_er_adj_se[length(Zbols_type_I_er_adj_se)], ")"))
        
      }
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Zbols_type_I_er_s2est_tcutoff[length(Zbols_type_I_er_s2est_tcutoff)],3)
        er_adj = round(Zbols_type_I_er_adj[length(Zbols_type_I_er_adj)],3)
        hist(my_Zbols_s2est$Zbols_stat, prob = T, main = bquote(Z[BOLS]*"-test"~"distribution under"~H[0]), 
             xlab = "", cex.axis = 0.7)
        curve(dnorm(x), add=T, col = "blue")
        abline(v = t_cutoff, col = "red")
        
        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Type-I ER ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
      }
    }
    
    Zbols_H0_distrPlot = recordPlot()
    res = list(Zbols_type_I_er_s2est_tcutoff = Zbols_type_I_er_s2est_tcutoff, Zbols_type_I_er_se = Zbols_type_I_er_se, Zbols_type_I_er_adj = Zbols_type_I_er_adj, Zbols_type_I_er_adj_se = Zbols_type_I_er_adj_se, Prob_adj_dx = Prob_adj_dx, Prob_adj_sx = Prob_adj_sx, Zbols_H0_distrPlot = Zbols_H0_distrPlot)
    
  } else if(which_test=="Z_AWAIPW"){
    
    Z_awaipw_type_I_er = c()
    Z_awaipw_type_I_er_adj = c()
    Z_awaipw_type_I_er_se = c()
    Z_awaipw_type_I_er_adj_se = c()
    Prob_adj_dx = c()
    Prob_adj_sx = c()
    for(k in batches){
      print(paste0("Computing AW-AIPW Type-I Error for Batch ", k))
      
      my_Z_awaipw = pblapply(my_data, compute_Z_AWAIPW, to_batch = k)
      my_Z_awaipw = data.frame(matrix(unlist(my_Z_awaipw), nrow=length(my_Z_awaipw), byrow=TRUE))
      names(my_Z_awaipw) = c("Batches", "AI_AIPW1", "AI_AIPW2", "Z_AWAIPW_diff")
      
      Z_awaipw_type_I_er = c(Z_awaipw_type_I_er, mean(my_Z_awaipw$Z_AWAIPW_diff>qnorm(1-alpha), na.rm = T))
      Z_awaipw_type_I_er_se = c(Z_awaipw_type_I_er_se, round(sd(my_Z_awaipw$Z_AWAIPW_diff>qnorm(1-alpha), na.rm = T)/sqrt(M), 3))
      print(paste0("AWAIPW-test Type-I Error for Batch ", k, " = ", Z_awaipw_type_I_er[length(Z_awaipw_type_I_er)], " (SE: ",Z_awaipw_type_I_er_se[length(Z_awaipw_type_I_er_se)], ")"))

      if(adj_typeI == T){
        varname = paste("batch", as.character(k), sep="_")
        Prob_adj_dx[[varname]] = (alpha/Z_awaipw_type_I_er[length(Z_awaipw_type_I_er)])*(alpha<=Z_awaipw_type_I_er[length(Z_awaipw_type_I_er)])
        Prob_adj_sx[[varname]] = (alpha-Z_awaipw_type_I_er[length(Z_awaipw_type_I_er)])/(1-Z_awaipw_type_I_er[length(Z_awaipw_type_I_er)])*(alpha>Z_awaipw_type_I_er[length(Z_awaipw_type_I_er)])
        
        Z_awaipw_type_I_er_adj = c(Z_awaipw_type_I_er_adj, sum(correct_rejection_cont(my_Z_awaipw$Z_AWAIPW_diff, cutoff = qnorm(1-alpha), 
                                                                                      probdx = Prob_adj_dx[length(Prob_adj_dx)], probsx = Prob_adj_sx[length(Prob_adj_sx)]), na.rm = T)/M)
        Z_awaipw_type_I_er_adj_se = c(Z_awaipw_type_I_er_adj_se, round(sd(correct_rejection_cont(my_Z_awaipw$Z_AWAIPW_diff, cutoff = qnorm(1-alpha), 
                                                                                                 probdx = Prob_adj_dx[length(Prob_adj_dx)], probsx = Prob_adj_sx[length(Prob_adj_sx)]), na.rm = T)/sqrt(M), 3))
        print(paste0("AWAIPW-test Adjusted Type-I Error for Batch ", k, " = ", Z_awaipw_type_I_er_adj[length(Z_awaipw_type_I_er_adj)], " (SE: ",Z_awaipw_type_I_er_adj_se[length(Z_awaipw_type_I_er_adj_se)], ")"))
        
      }
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Z_awaipw_type_I_er[length(Z_awaipw_type_I_er)], 3)
        er_adj = round(Z_awaipw_type_I_er_adj[length(Z_awaipw_type_I_er_adj)],3)
        hist(my_Z_awaipw$Z_AWAIPW_diff, prob = T, main = bquote(Z[AW-AIPW]*" test"~"distribution under"~H[0]), 
             xlab = "", cex.axis = 0.7)
        curve(dnorm(x), add=T, col = "blue")
        abline(v = qnorm(1-alpha), col = "red")
        
        
        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Type-I ER ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
        
      }
      
    }
    
    Z_awaipw_H0_distrPlot = recordPlot()
    res = list(Z_awaipw_type_I_er = Z_awaipw_type_I_er, Z_awaipw_type_I_er_se = Z_awaipw_type_I_er_se, Z_awaipw_type_I_er_adj = Z_awaipw_type_I_er_adj, Z_awaipw_type_I_er_adj_se = Z_awaipw_type_I_er_adj_se, Prob_adj_dx = Prob_adj_dx, Prob_adj_sx = Prob_adj_sx, Z_awaipw_H0_distrPlot = Z_awaipw_H0_distrPlot)
    
  } else {print("Please specify the test")}
  
  return(res)
}


mean_power = function(my_data, which_test, batches, alpha=0.05, my_nb, M,
                      plot_batches = c(3, 5, 8, 10, 20, 30, 50, 60, 70, 90), 
                      adj_typeI = T, Prob_adj_typeI, Prob_adj_dx, Prob_adj_sx){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: list of M dbs with the M trial data by batch, containing:
  # variable "Batch": the batch (i.e., time or update) number
  # variable "Arm": selected arm 
  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # which_test:: a vector with the name of the test statistics to be computed. 
  #              Can be any of c("Pi_test", "Z_OLS", "Z_BOLS", "Z_AWAIPW")
  # batches:: specific batches numbers for which to compute typeIer
  # plot_batches:: specific 9 batches numbers for which to plot the test statistic distribution
  # alpha:: significativity level (by default 0.05)
  # my_nb:: number of batches
  
  # Function output: a list with 2 elements
  # a vector with the power rates for each of the specified "batches"
  # a plot with the distribution of the test statistic at some fixed batches
  par(mfrow = c(3,3))
  
  if(which_test=="Pi_test"){
    
    Pi_power = c()
    Pi_power_adj = c()
    Pi_power_se = c()
    Pi_power_adj_se = c()
    for(k in batches){
      # compute Pi statistic & associated Power at batch k
      my_Pi = pblapply(my_data, compute_Pi_stat, to_batch = k+1, from_batch = 2)
      my_Pi = data.frame(matrix(unlist(my_Pi), nrow=length(my_Pi), byrow=TRUE))
      names(my_Pi) = c("Batches", "N1", "N2", "Pi_stat")
      
      Pi_power = c(Pi_power,sum(my_Pi$Pi_stat>(k-1))/M)
      Pi_power_se = c(Pi_power_se, round(sd(my_Pi$Pi_stat>(k-1))/sqrt(M), 3))
      print(paste0("Pi-test Power for Batch ", k, " = ", Pi_power[length(Pi_power)], " (SE: ",Pi_power_se[length(Pi_power_se)], ")"))
      
      
      if(adj_typeI == T){
        varname = paste("batch", k, sep="_")
        Pi_power_adj = c(Pi_power_adj, round(sum(correct_rejection_discr(cutoff = Prob_adj_typeI[[varname]][1], k=k, my_Pi$Pi_stat, prob = Prob_adj_typeI[[varname]][3]))/M, 3))
        Pi_power_adj_se = c(Pi_power_adj_se, round(sd(correct_rejection_discr(cutoff = Prob_adj_typeI[[varname]][1], k=k, my_Pi$Pi_stat, prob = Prob_adj_typeI[[varname]][3]))/sqrt(M), 3))
        print(paste0("Pi-test Adjusted Power for Batch ", k, " = ", Pi_power_adj[length(Pi_power_adj)], " (SE: ",Pi_power_adj_se[length(Pi_power_adj_se)], ")"))
        
      }
      
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Pi_power[length(Pi_power)],3)
        er_adj = round(Pi_power_adj[length(Pi_power_adj)],3)
        barplot(table(my_Pi$Pi_stat)/M, main = bquote(Pi*"-test"~"distribution under"~H[1]), 
                xlab = "", density=65, cex.axis = 0.7, cex.names=0.9)
        
        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Power ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
        
      }
      
    }
    
    Pi_H1_distrPlot = recordPlot()
    res = list(Pi_power = Pi_power, Pi_power_se = Pi_power_se, Pi_power_adj = Pi_power_adj, Pi_power_adj_se = Pi_power_adj_se, Pi_H1_distrPlot = Pi_H1_distrPlot)
    
  } else if(which_test=="Z_OLS"){
    #cutoff = qnorm(1-alpha)
    
    #Zols_power = c()
    #Zols_power_s2est = c()
    Zols_power_s2est_tcutoff = c()
    Zols_power_adj = c()
    Zols_power_se = c()
    Zols_power_adj_se = c()
    for(k in batches){
      #print(paste0("Computing OLS Power for Batch ", k))
      # compute Zols statistic (for sigma2 known) & associated power at batch k
      # my_Zols = pblapply(my_data, compute_Z_ols, to_batch = k, sigma2 = 1)
      # my_Zols = data.frame(matrix(unlist(my_Zols), nrow=length(my_Zols), byrow=TRUE))
      
      # compute Zols statistic (for sigma2 known unknown) & associated type-I error at batch k
      my_Zols_s2est = pblapply(my_data, compute_Z_ols, to_batch = k, sigma2 = "unknown")
      my_Zols_s2est = data.frame(matrix(unlist(my_Zols_s2est), nrow=length(my_Zols_s2est), byrow=TRUE))
      #names(my_Zols) = names(my_Zols_s2est) = c("Batches", "N1", "N2", "OLS1", "OLS2", "Zols_stat")
      names(my_Zols_s2est) = c("Batches", "N1", "N2", "OLS1", "OLS2", "Zols_stat")
      
      #Zols_power = c(Zols_power, sum(my_Zols$Zols_stat>cutoff)/M)
      #Zols_power_s2est = c(Zols_power_s2est, mean(my_Zols_s2est$Zols_stat>cutoff, na.rm = T))
      
      if(my_nb*k > 2){
        t_cutoff = qt(1-alpha, df = my_nb*k-2)
      } else {t_cutoff = qnorm(1-alpha)}
      
      Zols_power_s2est_tcutoff = c(Zols_power_s2est_tcutoff, round(mean(my_Zols_s2est$Zols_stat>t_cutoff, na.rm = T),3))
      Zols_power_se = c(Zols_power_se, round(sd(my_Zols_s2est$Zols_stat>t_cutoff, na.rm = T)/sqrt(M), 3))
      print(paste0("OLS-test Power for Batch ", k, " = ", Zols_power_s2est_tcutoff[length(Zols_power_s2est_tcutoff)], " (SE: ",Zols_power_se[length(Zols_power_se)], ")"))
      
      if(adj_typeI == T){
        varname = paste("batch", as.character(k), sep="_")
        Zols_power_adj = c(Zols_power_adj, round(sum(correct_rejection_cont(my_Zols_s2est$Zols_stat, cutoff = t_cutoff,
                                                                            probdx = Prob_adj_dx[[varname]], probsx = Prob_adj_sx[[varname]]), na.rm = T)/M, 3))
        Zols_power_adj_se = c(Zols_power_adj_se, round(sd(correct_rejection_cont(my_Zols_s2est$Zols_stat, cutoff = t_cutoff, 
                                                                                 probdx = Prob_adj_dx[[varname]], probsx = Prob_adj_sx[[varname]]), na.rm = T)/sqrt(M), 3))
        print(paste0("OLS-test Adjusted Power for Batch ", k, " = ", Zols_power_adj[length(Zols_power_adj)], " (SE: ",Zols_power_adj_se[length(Zols_power_adj_se)], ")"))
        
      }
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Zols_power_s2est_tcutoff[length(Zols_power_s2est_tcutoff)],3)
        er_adj = round(Zols_power_adj[length(Zols_power_adj)],3)
        hist(my_Zols_s2est$Zols_stat, prob = T, main = bquote(Z[OLS]*"-test"~"distribution under"~H[1]), 
             xlab = "", cex.axis = 0.7)
        #curve(dnorm(x), add=T, col = "blue")
        #abline(v = t_cutoff, col = "red")
        
        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Power ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
      }
    }
    
    Zols_H1_distrPlot = recordPlot()
    res = list(Zols_power_s2est_tcutoff = Zols_power_s2est_tcutoff, Zols_power_se = Zols_power_se, Zols_power_adj = Zols_power_adj, Zols_power_adj_se = Zols_power_adj_se, Zols_H1_distrPlot = Zols_H1_distrPlot)
    
  } else if(which_test=="Z_BOLS"){
    #Zbols_power = c()
    #Zbols_power_s2est = c()
    Zbols_power_s2est_tcutoff = c()
    Zbols_power_adj = c()
    Zbols_power_se = c()
    Zbols_power_adj_se = c()
    for(k in batches){
      #print(paste0("Computing BOLS Power for Batch ", k))
      # compute Zols statistic (for sigma2 known and unknown) & associated Power at batch k
      #my_Zbols = pblapply(my_data, compute_Z_bols, to_batch = k, sigma2 = 1)
      #my_Zbols = data.frame(matrix(unlist(my_Zbols), nrow=length(my_Zbols), byrow=TRUE))
      
      my_Zbols_s2est = pblapply(my_data, compute_Z_bols, to_batch = k, sigma2 = "unknown")
      my_Zbols_s2est = data.frame(matrix(unlist(my_Zbols_s2est), nrow=length(my_Zbols_s2est), byrow=TRUE))
      #names(my_Zbols) = names(my_Zbols_s2est) = c("Batches", "N1", "N2", "Zbols_stat")
      names(my_Zbols_s2est) = c("Batches", "N1", "N2", "Zbols_stat")
      
      #Zbols_power = c(Zbols_power, sum(my_Zbols$Zbols_stat>cutoff)/M)
      #Zbols_power_s2est = c(Zbols_power_s2est, sum(my_Zbols_s2est$Zbols_stat>cutoff)/M)
      
      if(my_nb>2){
        # note: being a simulation based strategy, it will not end up always with the same exact result
        t_sim = apply(matrix(rt(1e5*k, df=my_nb-2), ncol = k, nrow = 1e5), 1, sum)/sqrt(k)
        t_cutoff = quantile(t_sim, prob=1-alpha) 
      } else {t_cutoff = qnorm(1-alpha)}
      
      Zbols_power_s2est_tcutoff = c(Zbols_power_s2est_tcutoff, mean(my_Zbols_s2est$Zbols_stat>t_cutoff, na.rm = T))
      Zbols_power_se = c(Zbols_power_se, round(sd(my_Zbols_s2est$Zbols_stat>t_cutoff, na.rm = T)/sqrt(M), 3))
      print(paste0("BOLS-test Power for Batch ", k, " = ", Zbols_power_s2est_tcutoff[length(Zbols_power_s2est_tcutoff)], " (SE: ",Zbols_power_se[length(Zbols_power_se)], ")"))
      
      if(adj_typeI == T){
        varname = paste("batch", as.character(k), sep="_")
        Zbols_power_adj = c(Zbols_power_adj, sum(correct_rejection_cont(my_Zbols_s2est$Zbols_stat, cutoff = t_cutoff,
                                                                        probdx = Prob_adj_dx[[varname]], probsx = Prob_adj_sx[[varname]]), na.rm = T)/M)
        Zbols_power_adj_se = c(Zbols_power_adj_se, round(sd(correct_rejection_cont(my_Zbols_s2est$Zbols_stat, cutoff = t_cutoff, 
                                                                                   probdx = Prob_adj_dx[[varname]], probsx = Prob_adj_sx[[varname]]), na.rm = T)/sqrt(M), 3))
        print(paste0("BOLS-test Adjusted Power for Batch ", k, " = ", Zbols_power_adj[length(Zbols_power_adj)], " (SE: ",Zbols_power_adj_se[length(Zbols_power_adj_se)], ")"))
        
      }
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Zbols_power_s2est_tcutoff[length(Zbols_power_s2est_tcutoff)],3)
        er_adj = round(Zbols_power_adj[length(Zbols_power_adj)],3)
        hist(my_Zbols_s2est$Zbols_stat, prob = T, main = bquote(Z[BOLS]*"-test"~"distribution under"~H[1]), 
             xlab = "", cex.axis = 0.7)
        #curve(dnorm(x), add=T, col = "blue")
        #abline(v = t_cutoff, col = "red")
        
        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Power ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
      }
    }
    
    Zbols_H1_distrPlot = recordPlot()
    res = list(Zbols_power_s2est_tcutoff = Zbols_power_s2est_tcutoff, Zbols_power_se = Zbols_power_se, Zbols_power_adj = Zbols_power_adj, Zbols_power_adj_se = Zbols_power_adj_se, Zbols_H1_distrPlot = Zbols_H1_distrPlot)
    
  } else if(which_test=="Z_AWAIPW"){
    
    Z_awaipw_power = c()
    Z_awaipw_power_se = c()
    Z_awaipw_power_adj = c()
    Z_awaipw_power_adj_se = c()
    for(k in batches){
      #print(paste0("Computing AW-AIPW Power for Batch ", k))
      
      my_Z_awaipw = pblapply(my_data, compute_Z_AWAIPW, to_batch = k)
      my_Z_awaipw = data.frame(matrix(unlist(my_Z_awaipw), nrow=length(my_Z_awaipw), byrow=TRUE))
      names(my_Z_awaipw) = c("Batches", "AI_AIPW1", "AI_AIPW2", "Z_AWAIPW_diff")
      
      #Zols_type_I_er = c(Zols_type_I_er, sum(my_Zols$Zols_stat>cutoff)/M)
      Z_awaipw_power = c(Z_awaipw_power, mean(my_Z_awaipw$Z_AWAIPW_diff>qnorm(1-alpha), na.rm = T))
      Z_awaipw_power_se = c(Z_awaipw_power_se, round(sd(my_Z_awaipw$Z_AWAIPW_diff>qnorm(1-alpha), na.rm = T)/sqrt(M), 3))
      print(paste0("AWAIPW-test Power for Batch ", k, " = ", Z_awaipw_power[length(Z_awaipw_power)], " (SE: ",Z_awaipw_power_se[length(Z_awaipw_power_se)], ")"))
      
      if(adj_typeI == T){
        varname = paste("batch", as.character(k), sep="_")
        Z_awaipw_power_adj = c(Z_awaipw_power_adj, sum(correct_rejection_cont(my_Z_awaipw$Z_AWAIPW_diff, cutoff = qnorm(1-alpha), 
                                                                              probdx = Prob_adj_dx[[varname]], probsx = Prob_adj_sx[[varname]]), na.rm = T)/M)
        Z_awaipw_power_adj_se = c(Z_awaipw_power_adj_se, round(sd(correct_rejection_cont(my_Z_awaipw$Z_AWAIPW_diff, cutoff = qnorm(1-alpha), 
                                                                                         probdx = Prob_adj_dx[[varname]], probsx = Prob_adj_sx[[varname]]), na.rm = T)/sqrt(M), 3))
        print(paste0("AWAIPW-test Adjusted Power for Batch ", k, " = ", Z_awaipw_power_adj[length(Z_awaipw_power_adj)], " (SE: ",Z_awaipw_power_adj_se[length(Z_awaipw_power_adj_se)], ")"))
      }
      
      if(k %in% plot_batches){
        ss = k*my_nb
        er = round(Z_awaipw_power[length(Z_awaipw_power)],3)
        er_adj = round(Z_awaipw_power_adj[length(Z_awaipw_power_adj)],3)
        hist(my_Z_awaipw$Z_AWAIPW_diff, prob = T, main = bquote(Z[AW-AIPW]*"-test"~"distribution under"~H[1]), 
             xlab = "", cex.axis = 0.7)
        
        mtext(side=1, bquote("N ="~.(ss)*" ("*n[b]~"="~.(my_nb)*";"~"T ="~.(k)*")"), line=2.3, cex = 0.7)
        mtext(side=1, bquote("Power ="~.(er)~"[Adj: "*.(er_adj)*"]"), line=3.3, col = "darkgray", cex = 0.7)
        
      }
    }
    
    Z_awaipw_H1_distrPlot = recordPlot()
    res = list(Z_awaipw_power = Z_awaipw_power, Z_awaipw_power_adj = Z_awaipw_power_adj, Z_awaipw_power_se = Z_awaipw_power_se, Z_awaipw_power_adj_se = Z_awaipw_power_adj_se, Z_awaipw_H1_distrPlot = Z_awaipw_H1_distrPlot)
    
  } else {print("Please specify the test")}
  
  return(res)
}


# 1. Simulating data trajectories with Normal-TS & compute TypeI ER: illustrative example ----------------------
M=10000
my_N = 200
my_nb = 3
my_sigma2 = c(5,5)
my_clip = 0

my_mu = c(0,0)
try_H0 = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(10,10)), 
                                         true_params = list(mu=my_mu, sigma2=my_sigma2), clip = my_clip, atleastone = F)))
my_mu = c(0,0.5)
try_H1 = pbreplicate(M, list(TS_NormNorm(n_arms = 2, N = my_N, n_b = my_nb, normal_priors = list(mu=c(0,0), sigma2=c(10,10)), 
                                         true_params = list(mu=my_mu, sigma2=my_sigma2), clip = my_clip, atleastone = F)))

#check if empirical average is consistent with the true value
length(which(try_H0[[1]]$Arm==1)); length(which(try_H0[[1]]$Arm==2))
mean(try_H1[[1]]$Reward[which(try_H1[[1]]$Arm==1)]); mean(try_H1[[1]]$Reward[which(try_H1[[1]]$Arm==2)])
var(try_H1[[1]]$Reward[which(try_H1[[1]]$Arm==1)]); var(try_H1[[1]]$Reward[which(try_H1[[1]]$Arm==2)])

my_nb = 3
my_plot_batches3 = c(1, 3, 5, 10, 15, 20, 30, 40, 50)
my_batches3 = c(1,3,5,10,15,20,30,40,50,60)


try_typeIer = mean_typeIer(my_data = try_H0, 
                           which_test="Pi_test", 
                           batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                           plot_batches = my_plot_batches3)

try_power = mean_power(my_data = try_H1, 
                       which_test="Pi_test", 
                       batches = my_batches3, alpha = 0.05, my_nb = my_nb, M=M,
                       plot_batches = my_plot_batches3,
                       Prob_adj_typeI = try_typeIer$Prob_adj
                       #Prob_adj_dx = try_typeIer$Prob_adj_dx,
                       #Prob_adj_sx = try_typeIer$Prob_adj_sx
)


# 3. Computing performance data: Reward and Prop Optimal action -------------------

# For a single dataset
get_reward = function(my_data, to_batch, opt_arm = 2){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # my_data:: db with the trial data by batch, containing:
  # variable "Batch": the batch (i.e., time or update) number
  # variable "Arm": selected arm 
  # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # to_batch:: number of batches for which the test has to be computed
  # opt_arm:: arm that is optimal (1 or 2)
  
  # Function output: a 4-dim vector with
  # N1, N2:: number of times arm 1 and arm 2 were selected
  # reward:: cumulative reward
  # prop_opt_arm:: proportion of times the optimal arm was selected

  my_data_to_batch = my_data[my_data$Batch %in% 1:to_batch,]
  cum_reward = sum(my_data_to_batch$Reward)
  
  N1 = sum(my_data_to_batch$Arm == 1)
  N2 = sum(my_data_to_batch$Arm == 2)
  
  if(opt_arm == 2){
    prop_opt_arm = N2/(N1+N2)
  } else {
    prop_opt_arm = N1/(N1+N2)
  }
  
  return(c(N1 = N1, N2 = N2, cum_reward = cum_reward, prop_opt_arm = prop_opt_arm))
}


# Average (across simulations) for a single TS version (test)
mean_rew_optarm = function(my_data, batches, arm_diff = 0.5){
  
  Avg_Cum_Reward = c()
  Avg_Prop_opt_arm = c()
  for(k in batches){
    
    print(paste0("Computing Reward Data for Batch ", k))
    Rew = pblapply(data, get_reward, to_batch = k)
    Rew = data.frame(matrix(unlist(Rew), nrow=length(Rew), byrow=TRUE))
    
    names(Rew) = c("N1", "N2", "Cum_Reward", "Prop_Optimal_Alloc")
    
    Rew_mean = apply(Rew, 2, mean)
    Reg_mean = (Rew_mean[1] + Rew_mean[2])*arm_diff - Rew_mean[3]
      
    Avg_Cum_Reward = rbind(Avg_Cum_Reward, c(Batch = k, Cum_Rew = Rew_mean[3], Cum_Reg = Reg_mean))
    Avg_Prop_opt_arm = rbind(Avg_Prop_opt_arm, c(Batch = k, my_Opt_arm = Rew_mean[4]))
    
  }
  
  return(list(Avg_Cum_Reward = Avg_Cum_Reward, Avg_Prop_opt_arm = Avg_Prop_opt_arm))
}

# Average (across simulations) for 
mean_rew_optarm = function(data_Pitest = Pi_H1, data_BOLS = BOLS_H1, data_AWAIPW = AWAIPW_H1, batches, mu = my_mu, M = 10000){
  # Note: this function is for the two-arm case (n_arms = 2) a normal rewards
  
  # Function inputs:
  # data_Pitest, data_BOLS:: db with the trial data by batch simulated according to TS for Pi-test and TS for BOLS based Z-test, containing:
    # variable "Batch": the batch (i.e., time or update) number
    # variable "Arm": selected arm 
    # variable "AlloProb1": the allocation probabilities of arm 1 in each batch
  # batches: batches for which reward performances have to be computed
  
  # Function output: a 2-dim list with Avg_Reward & Avg_Prop_opt_arm, each being a dataframe with columns
  # Batch (or time) number
  # Avg_Reward or Avg_Prop_opt_arm for standard TS (Pi-test)
  # Avg_Reward or Avg_Prop_opt_arm for tuned TS (BOLS-test)
  Avg_Reward = c()
  Avg_Reward_SE = c()
  Avg_Regret = c()
  Avg_Prop_opt_arm = c()
  for(k in batches){
    print(paste0("Computing Reward Data for Batch ", k))
    Rew_Pi = pblapply(data_Pitest, get_reward, to_batch = k)
    Rew_Pi = data.frame(matrix(unlist(Rew_Pi), nrow=length(Rew_Pi), byrow=TRUE))
    
    Rew_BOLS = pblapply(data_BOLS, get_reward, to_batch = k)
    Rew_BOLS = data.frame(matrix(unlist(Rew_BOLS), nrow=length(Rew_BOLS), byrow=TRUE))
    
    Rew_AWAIPW = pblapply(data_AWAIPW, get_reward, to_batch = k)
    Rew_AWAIPW = data.frame(matrix(unlist(Rew_AWAIPW), nrow=length(Rew_AWAIPW), byrow=TRUE))
    
    names(Rew_Pi) = names(Rew_BOLS) = names(Rew_AWAIPW) = c("N1", "N2", "Reward", "Prop_Optimal_Alloc")
    
    Rew_Pi_mean = apply(Rew_Pi, 2, mean) #get avg (across sim)
    Rew_Pi_se = sd(Rew_Pi$Reward)/sqrt(M)
    Rew_BOLS_mean = apply(Rew_BOLS, 2, mean) #get avg (across sim)
    Rew_BOLS_se = sd(Rew_BOLS$Reward)/sqrt(M)
    Rew_AWAIPW_mean = apply(Rew_AWAIPW, 2, mean) #get avg (across sim)
    Rew_AWAIPW_se = sd(Rew_AWAIPW$Reward)/sqrt(M)
    
    Avg_Reward = rbind(Avg_Reward, c(Batch = k, Pi = Rew_Pi_mean[3],
                                     BOLS = Rew_BOLS_mean[3],
                                     AWAIPW = Rew_AWAIPW_mean[3]))
    
    Avg_Regret = rbind(Avg_Regret, c(Batch = k, Pi = (Rew_Pi_mean[1] + Rew_Pi_mean[2])*mu[2] - Rew_Pi_mean[3],
                                     BOLS = (Rew_BOLS_mean[1] + Rew_BOLS_mean[2])*mu[2] - Rew_BOLS_mean[3],
                                     AWAIPW = (Rew_AWAIPW_mean[1] + Rew_AWAIPW_mean[2])*mu[2] - Rew_AWAIPW_mean[3]))
    #Reward SE = Regret SE
    Avg_Reward_SE = rbind(Avg_Reward_SE, c(Batch = k, Pi = Rew_Pi_se,
                                     BOLS = Rew_BOLS_se,
                                     AWAIPW = Rew_AWAIPW_se))
    
    Avg_Prop_opt_arm = rbind(Avg_Prop_opt_arm, c(Batch = k, Pi = Rew_Pi_mean[4],
                                                 BOLS = Rew_BOLS_mean[4],
                                                 AWAIPW = Rew_AWAIPW_mean[4]))
    
  }
  
  return(list(Avg_Reward = Avg_Reward, Avg_Reward_SE = Avg_Reward_SE, Avg_Regret = Avg_Regret, Avg_Prop_opt_arm = Avg_Prop_opt_arm))
}



# Other functions ----------------

# Get mean AlloProb
Alloprob_batch = function(my_data, batches=my_batches, nb=my_nb){
  Avg_Allo = c()
  for(k in batches){
    Avg_Allo = c(Avg_Allo, 1-my_data$AlloProb1[k*my_nb])
  }
  return(Avg_Allo)
}

