
#looking at TS results

par(mfrow = c(2,2))
p0 = unlist(lapply(try_H1, function(x) 1-x$AlloProb1[1]))
hist(p0, xlim=c(0,1), breaks = 50, freq = F)
sum(p0>0.5)/M #T = : Pi non computable
p1 = unlist(lapply(try_H1, function(x) 1-x$AlloProb1[2]))
hist(p1, xlim=c(0,1), breaks = 50, freq = F)
sum(p1>0.5)/M #T = 1: P(Pi = 1) = 0.67

p2 = unlist(lapply(try_H1, function(x) 1-x$AlloProb1[3]))
hist(p2, xlim=c(0,1), breaks = 50, freq = F)
mean(p2) #T = 2: P(A = 1) = 0.63
sum((p1>0.5)&(p2>0.5))/M #T = 2: P(Pi = 2) = 0.60
sum(p1>0.5)/M * sum(p2[p1>0.5]>0.5)/length(p2[p1>0.5]) #T = 2: P(Pi = 2) = 0.60 (with factorization formula)
hist(p2[p1>0.5], xlim=c(0,1), breaks = 50, freq = F)

#conditional p1.A
p1.A1 = unlist(lapply(try_H1, function(x) 1-x$AlloProb1[2][x$Arm[1]==2]))
hist(p1.A1, xlim=c(0,1), breaks = 50, freq = F)
mean(p1.A1) 
p1.A0 = unlist(lapply(try_H1, function(x) 1-x$AlloProb1[2][x$Arm[1]==1]))
hist(p1.A0, xlim=c(0,1), breaks = 50, freq = F)
mean(p1.A0) 



# trying pnorm
a = rbinom(10000, 1, 0.5)
A = list(mu1 = 1/(1+a)*rnorm(10000, mean = 1)*a, mu2 = 1/(1+(1-a))*rnorm(10000, mean = 0)*(1-a),
         sig1 = 1/(1+a), sig2 = 1/(1+(1-a)))
sum(pnorm((A$mu1-A$mu2)/sqrt(A$sig1+A$sig2))>0.5)/(M)
sum((A$mu1-A$mu2)/sqrt(A$sig1+A$sig2)>0)/(M)

A = list(mu1 = 1/(1+a)*rnorm(10000, mean = 1)*a, mu2 = 1/(1+(1-a))*rnorm(10000, mean = 0)*(1-a),
         sig1 = 1/(1+a), sig2 = 1/(1+(1-a)))
sum(pnorm((A$mu1-A$mu2)/sqrt(A$sig1+A$sig2))>0.5)/(M)
sum((A$mu1-A$mu2)/sqrt(A$sig1+A$sig2)>0)/(M)


# trying my function for TS
res = c()
for (i in 1:length(a)){
  res[i] = rho1_est_NormNorm(M = 10000, means = c(A$mu1[i], A$mu2[i]), sds = c(sqrt(A$sig1[i]), sqrt(A$sig2[i])))
}
sum(res > 0.5)/M


#trying formulas: T=1

(1-pnorm(0, mean = 1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1))))*1/2 + (1-pnorm(0, mean = -1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1))))*1/2

(pnorm(1))*1/2 + pnorm(0)*1/2
(pnorm(10))*1/2 + (1-pnorm(10))*1/2

(pnorm(1) + pnorm(0))*1/2


x = rnorm(M, mean = 1)
sum(x/(2*sqrt(1/2+1))>0)/M
x = rnorm(M, mean = 1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1)))
sum(x>0)/M

x2 = rnorm(M, mean = 0)
sum(-x2/(2*sqrt(1/2+1))>0)/M

x2 = rnorm(M, mean = -1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1)))
sum(x2>0)/M

#trying formulas: T=2

(1-pnorm(0, mean = 1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1))))*1/2 + (1-pnorm(0, mean = -1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1))))*1/2

(pnorm(1))*1/2 + pnorm(0)*1/2
(pnorm(10))*1/2 + (1-pnorm(10))*1/2

(pnorm(1) + pnorm(0))*1/2


x = rnorm(M, mean = 1)
sum(x/(2*sqrt(1/2+1))>0)/M
x = rnorm(M, mean = 1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1)))
sum(x>0)/M

x2 = rnorm(M, mean = 0)
sum(-x2/(2*sqrt(1/2+1))>0)/M

x2 = rnorm(M, mean = -1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1)))
sum(x2>0)/M


x = rnorm(M, mean = 1/(2*sqrt(1/2+1)), sd = 1/(2*sqrt(1/2+1)))
hist(pnorm(x)*1/2)


my_mu = c(0,1)
y0 = rnorm(M, mean = my_mu[1])
y1 = rnorm(M, mean = my_mu[1])


sum((y0+y1)[y0>0]/(3*sqrt(1/3+1))>0)/sum(y0>0)
sum(((y0+y1)/(3*sqrt(1/3+1))>0)&(y0>0))/M/mean(y0>0)

sum((y1>-y0)&(y0>0))/M/mean(y0>0)

sum((y0>-y1)&(-y1>0))/sum(-y1>0)*mean(-y1>0) + sum((y0>0)&(0>-y1))/sum(-y1<0)*mean(-y1<0)

sum((dnorm(y1)*(1-pnorm(y1)))>0)/sum(-y1>0)*mean(-y1>0)

hist(pnorm(y0[y0>0]/(2*sqrt(1.5)))) 
hist(pnorm(-y0[y0<0]/(2*sqrt(1.5))))



# trying pnorm
a0 = rbinom(10000, 1, 0.5); y0a0 = rnorm(10000, mean = 0); y0a1 = rnorm(10000, mean = 0) 
a1 = rbinom(10000, 1, 0.5); y1a0 = rnorm(10000, mean = 0); y1a1 = rnorm(10000, mean = 0) 

sum(((a0*y0a0+a1*y1a0)/(1+a0+a1) - ((1-a0)*y0a1+(1-a1)*y1a1)/(1+(1-a0)+(1-a1)))>0)


A = list(mu1 = 1/(1+a)*rnorm(10000, mean = 1)*a, mu2 = 1/(1+(1-a))*rnorm(10000, mean = 0)*(1-a),
         sig1 = 1/(1+a), sig2 = 1/(1+(1-a)))
sum(pnorm((A$mu1-A$mu2)/sqrt(A$sig1+A$sig2))>0.5)/(M)
sum((A$mu1-A$mu2)/sqrt(A$sig1+A$sig2)>0)/(M)


a = rnorm(M, 0, 1)
b = rnorm(M, 1, 2)
c = rnorm(M, -1, sqrt(5))

mean(a-b > 0); mean(c>0)

a0 = 1; y0a0 = rnorm(10000, mean = 0); #y0a1 = rnorm(10000, mean = 0) 
a1 = 1; y1a0 = rnorm(10000, mean = 0); #y1a1 = rnorm(10000, mean = 0)

sum(((a0*y0a0+a1*y1a0)>0)&(a0*y0a0>0))/M #3/8 = 0.37
sum(((a0*y0a0+a1*y1a0)[a0*y0a0>0]>0))/sum(a0*y0a0>0)*mean(a0*y0a0>0) #3/4


a0 = 1; y0.0 = rnorm(10000, mean = 0); y0.1 = rnorm(10000, mean = 0) 
a1 = 0; y1.0 = rnorm(10000, mean = 0); y1.1 = rnorm(10000, mean = 0)

sum(((y0.1-y1.0)>0)&(y0.1>0))/M #3/8 = 0.37

sum(((y1.1-y0.0)>0)&(-y0.0>0))/M #3/8 = 0.37

sum(((-y0.0-y1.0)>0)&(-y0.0>0))/M #3/8 = 0.37
