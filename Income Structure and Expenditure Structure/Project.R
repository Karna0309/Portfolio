set.seed(123)
###### General Setup ######
# Absolute value density
fun = function (y, k, m, s)
  return ((dt((y-m)/s, k)+ dt((-y-m)/s, k))/s)
# Income parameters
## University District
k1 = 3
m1 = 5
s1 = 3
## Downtown Seattle
k2 = 4
m2 = 4
s2 = 4
## Capitol Hill
k3 = 5
m3 = 8
s3 = 7
# Expenditure parameters
mu_exp = 2.5
beta = 0.7
fun2 = function(x) {
  mux = mean(x)
  e = rt(length(x), 4)
  return(mu_exp+beta*(x-mux)+1*e)
}
# University District
t_UD = rt(1000, k1)
income_UD = sort(abs(s1*t_UD+m1))
absolute_UD = fun(income_UD, k1, m1, s1)
# Downtown Seattle
t_DS = rt(1000, k2)
income_DS = sort(abs(s2*t_DS+m2))
absolute_DS = fun(income_DS, k2, m2, s2)
# Capitol Hill
t_CH = rt(1000, k3)
income_CH = sort(abs(s3*t_CH+m3))
absolute_CH = fun(income_CH, k3, m3, s3)
# Mixture
pp = c(0.42, 0.13, 0.45)
mix_pdf = pp[1]*absolute_UD+pp[2]*absolute_DS+pp[3]*absolute_CH
nd = rmultinom(1, 1000, pp)[, 1]
mix1 = sample(income_UD, nd[1])
mix2 = sample(income_DS, nd[2])
mix3 = sample(income_CH, nd[3])
mix_income = sort(c(mix1, mix2, mix3))
# Expenditure
expenditure = abs(fun2(mix_income))

###### KDE ######
kde_G = density(mix_income, bw="SJ", kernel="gaussian")
kde_E = density(mix_income, bw="SJ", kernel="epanechnikov")
kde_R = density(mix_income, bw="SJ", kernel="rectangular")
plot(mix_income, mix_pdf, 
     main="PDF of mix income",
     xlab="mix income",
     ylab="p",
     type="l", lwd=3)
lines(kde_G, col="red", lwd=2)
lines(kde_E, col="green", lwd=2)
lines(kde_R, col="blue", lwd=2)
legend("topright",
       c("Real", "Gaussian", "Epanechnikov", "Rectangular"),
       col=c("black", "red", "green", "blue"),
       lty=1, cex=0.7)
bw_G = kde_G$bw
bw_E = kde_E$bw
bw_R = kde_R$bw
cat(bw_G, bw_E, bw_R)
bias_G_vec = rep(NA, 1000)
MSE_G_vec = rep(NA, 1000)
bias_E_vec = rep(NA, 1000)
MSE_E_vec = rep(NA, 1000)
bias_R_vec = rep(NA, 1000)
MSE_R_vec = rep(NA, 1000)
for (i in 1:1000) {
  bias_G_vec[i] = kde_G$y[1] - mix_income[1]
  MSE_G_vec[i] = mean((kde_G$y[1] - mix_income[1])^2)
  bias_E_vec[i] = kde_E$y[1] - mix_income[1]
  MSE_E_vec[i] = mean((kde_E$y[1] - mix_income[1])^2)
  bias_R_vec[i] = kde_R$y[1] - mix_income[1]
  MSE_R_vec[i] = mean((kde_R$y[1] - mix_income[1])^2)
}
bias_G = mean(bias_G_vec)
MSE_G = mean(MSE_G_vec)
bias_E = mean(bias_E_vec)
MSE_E = mean(MSE_E_vec)
bias_R = mean(bias_R_vec)
MSE_R = mean(MSE_R_vec)

###### PDF ######
plot(income_UD, absolute_UD, 
     main="PDF of Income and Expenditure",
     xlab="Monthly in thousand dollars",
     ylab="Probability",
     ylim=c(0, 0.25),
     type="l", 
     col="red", 
     lwd=2)
lines(income_DS, absolute_DS, 
      type="l", 
      col="blue", 
      lwd=2)
lines(income_CH, absolute_CH, 
      type="l", 
      col="green", 
      lwd=2)
lines(mix_income, mix_pdf,
      type="l",
      col="purple", 
      lwd=2)
lines(density(expenditure, bw="SJ", kernel="gaussian"), 
      lwd=3, 
      col="orange") # Estimated by Gaussian kernel
legend("topright",
       c("University District Income", "Downtown Seattle Income",
         "Capitol Hill Income", "Mixture Income", "Mixture Expenditure"),
       col=c("red", "blue", "green", "purple", "orange"),
       lty=1,
       cex=0.7)

###### Test 1: mix income and expenditure ######
n = 50
# permutation test
nrep = 100
nperm = 1000
## storage
perm_pvalue_IQR = rep(NA, nrep)
perm_lower_IQR = rep(NA, n)
perm_pvalue_median = rep(NA, nrep)
perm_lower_median = rep(NA, n)
diff_IQR = IQR(mix_income)-IQR(expenditure)
diff_median = median(mix_income)-median(expenditure)
diff_IQR_per = rep(NA, nrep)
diff_median_per = rep(NA, nrep)
data_pull = c(mix_income, expenditure)
for (i in 2:n) {
  for (j in 1:nrep) {
    ## permutation
    for (k in 1:nperm) {
      ## test for power
      w_per = sample(2000, 2*i, replace=FALSE)
      data_per = data_pull[w_per]
      income_new = data_per[1:i]
      expend_new = data_per[(i+1):(2*i)]
      diff_new_IQR = IQR(income_new)-IQR(expend_new)
      diff_new_median = median(income_new)-median(expend_new)
      diff_IQR_per[k] = diff_new_IQR
      diff_median_per[k] = diff_new_median
    }
    perm_pvalue_IQR[j] = (length(which(diff_IQR_per>=diff_IQR))+1)/(nperm+1)
    perm_pvalue_median[j] = (length(which(diff_median_per>=diff_median))+1)/(nperm+1)
  }
  est_perm_power_IQR =  length(perm_pvalue_IQR[perm_pvalue_IQR<=0.05])/nrep
  est_perm_se_IQR = sqrt(est_perm_power_IQR*(1-est_perm_power_IQR)/nrep)
  est_perm_power_median =  length(perm_pvalue_median[perm_pvalue_median<=0.05])/nrep
  est_perm_se_median = sqrt(est_perm_power_median*(1-est_perm_power_median)/nrep)
  perm_lower_IQR[i] = est_perm_power_IQR-qnorm(0.975)*est_perm_se_IQR
  perm_lower_median[i] = est_perm_power_median-qnorm(0.975)*est_perm_se_median
}

# KS test
nrep = 10000
## storage
ks_pvalue = rep(NA, nrep)
ks_lower = rep(NA, n)
for (i in 2:n) {
  ## Monte Carlo
  for (j in 1:nrep) {
    ## Generate data
    t_UD = rt(i, k1)
    income_UD = sort(abs(s1*t_UD+m1))
    t_DS = rt(i, k2)
    income_DS = sort(abs(s2*t_DS+m2))
    t_CH = rt(i, k3)
    income_CH = sort(abs(s3*t_CH+m3))
    nd = rmultinom(1, i, pp)[, 1]
    mix1 = sample(income_UD, nd[1])
    mix2 = sample(income_DS, nd[2])
    mix3 = sample(income_CH, nd[3])
    mix_income = sort(c(mix1, mix2, mix3))
    expenditure = abs(fun2(mix_income))
    ## test for power
    ks_test = ks.test(mix_income, expenditure)
    ks_pvalue[j] = ks_test$p.value
  }
  est_ks_power =  length(ks_pvalue[ks_pvalue<=0.05])/nrep
  est_ks_se = sqrt(est_ks_power*(1-est_ks_power)/nrep)
  ks_lower[i] = est_ks_power-qnorm(0.975)*est_ks_se
}

## plot
plot(1:n, ks_lower, 
     main="n VS lower 95% CI of power",
     ylab="power",
     xlab="n",
     col="blue",
     type="l",
     lwd=3)
ks_n = 2
while (ks_lower[ks_n] < 0.8) {
  ks_n = ks_n+1
}
abline(v=ks_n, col="blue", lty=2)

lines(1:n, perm_lower_IQR, 
      col="green", lwd=3)
perm_n_IQR = 2
while (perm_lower_IQR[perm_n_IQR] < 0.8) {
  perm_n_IQR = perm_n_IQR+1
}
abline(v=perm_n_IQR, col="green", lty=2)

lines(1:n, perm_lower_median, 
      col="orange", lwd=3)
perm_n_median = 2
while (perm_lower_median[perm_n_median] < 0.8) {
  perm_n_median = perm_n_median+1
}
abline(v=perm_n_median, col="orange", lty=2)
abline(h=0.8, col="red", lwd=2)
legend("bottomright",
       c("KS test", "Permutation on IQR", "Permutation on Median"),
       col=c("blue", "green", "orange"),
       pch=19,
       cex=0.4)

c(ks_n, perm_n_IQR, perm_n_median)

###### Test 2: DS income and CH income ######
n = 50
# permutation test
nrep = 100
nperm = 1000
## storage
perm_pvalue_IQR = rep(NA, nrep)
perm_lower_IQR = rep(NA, n)
perm_pvalue_median = rep(NA, nrep)
perm_lower_median = rep(NA, n)
diff_IQR = IQR(income_CH)-IQR(income_DS)
diff_median = median(income_CH)-median(income_DS)
diff_IQR_per = rep(NA, nrep)
diff_median_per = rep(NA, nrep)
data_pull = c(income_CH, income_DS)
for (i in 2:n) {
  for (j in 1:nrep) {
    ## permutation
    for (k in 1:nperm) {
      ## test for power
      w_per = sample(2000, 2*i, replace=FALSE)
      data_per = data_pull[w_per]
      CH_new = data_per[1:i]
      DS_new = data_per[(i+1):(2*i)]
      diff_new_IQR = IQR(CH_new)-IQR(DS_new)
      diff_new_median = median(CH_new)-median(DS_new)
      diff_IQR_per[k] = diff_new_IQR
      diff_median_per[k] = diff_new_median
    }
    perm_pvalue_IQR[j] = (length(which(diff_IQR_per>=diff_IQR))+1)/(nperm+1)
    perm_pvalue_median[j] = (length(which(diff_median_per>=diff_median))+1)/(nperm+1)
  }
  est_perm_power_IQR =  length(perm_pvalue_IQR[perm_pvalue_IQR<=0.05])/nrep
  est_perm_se_IQR = sqrt(est_perm_power_IQR*(1-est_perm_power_IQR)/nrep)
  est_perm_power_median =  length(perm_pvalue_median[perm_pvalue_median<=0.05])/nrep
  est_perm_se_median = sqrt(est_perm_power_median*(1-est_perm_power_median)/nrep)
  perm_lower_IQR[i] = est_perm_power_IQR-qnorm(0.975)*est_perm_se_IQR
  perm_lower_median[i] = est_perm_power_median-qnorm(0.975)*est_perm_se_median
}

# KS test
nrep = 10000
## storage
ks_pvalue = rep(NA, nrep)
ks_lower = rep(NA, n)
for (i in 2:n) {
  ## Monte Carlo
  for (j in 1:nrep) {
    ## Generate data
    t_DS = rt(i, k2)
    income_DS = sort(abs(s2*t_DS+m2))
    t_CH = rt(i, k3)
    income_CH = sort(abs(s3*t_CH+m3))
    ## test for power
    ks_test = ks.test(income_CH, income_DS)
    ks_pvalue[j] = ks_test$p.value
  }
  est_ks_power =  length(ks_pvalue[ks_pvalue<=0.05])/nrep
  est_ks_se = sqrt(est_ks_power*(1-est_ks_power)/nrep)
  ks_lower[i] = est_ks_power-qnorm(0.975)*est_ks_se
}

## plot
plot(1:n, ks_lower, 
     main="n VS lower 95% CI of power",
     ylim=c(0,1),
     ylab="power",
     xlab="n",
     col="blue",
     type="l",
     lwd=3)
ks_n = 2
while (ks_lower[ks_n] < 0.8) {
  ks_n = ks_n+1
}
abline(v=ks_n, col="blue", lty=2)

lines(1:n, perm_lower_IQR, 
      col="green", lwd=3)
perm_n_IQR = 2
while (perm_lower_IQR[perm_n_IQR] < 0.8) {
  perm_n_IQR = perm_n_IQR+1
}
abline(v=perm_n_IQR, col="green", lty=2)

lines(1:n, perm_lower_median, 
      col="orange", lwd=3)
perm_n_median = 2
while (perm_lower_median[perm_n_median] < 0.8) {
  perm_n_median = perm_n_median+1
}
abline(v=perm_n_median, col="orange", lty=2)
abline(h=0.8, col="red", lwd=2)

legend("bottomright",
       c("KS test", "Permutation on IQR", "Permutation on Median"),
       col=c("blue", "green", "orange"),
       pch=19,
       cex=0.4)

c(ks_n, perm_n_IQR, perm_n_median)

###### Test 3: UD income and CH income ######
n = 50
# permutation test
nrep = 100
nperm = 1000
## storage
perm_pvalue_IQR = rep(NA, nrep)
perm_lower_IQR = rep(NA, n)
perm_pvalue_median = rep(NA, nrep)
perm_lower_median = rep(NA, n)
diff_IQR = IQR(income_CH)-IQR(income_UD)
diff_median = median(income_CH)-median(income_UD)
diff_IQR_per = rep(NA, nrep)
diff_median_per = rep(NA, nrep)
data_pull = c(income_CH, income_UD)
for (i in 2:n) {
  for (j in 1:nrep) {
    ## permutation
    for (k in 1:nperm) {
      ## test for power
      w_per = sample(2000, 2*i, replace=FALSE)
      data_per = data_pull[w_per]
      CH_new = data_per[1:i]
      UD_new = data_per[(i+1):(2*i)]
      diff_new_IQR = IQR(CH_new)-IQR(UD_new)
      diff_new_median = median(CH_new)-median(UD_new)
      diff_IQR_per[k] = diff_new_IQR
      diff_median_per[k] = diff_new_median
    }
    perm_pvalue_IQR[j] = (length(which(diff_IQR_per>=diff_IQR))+1)/(nperm+1)
    perm_pvalue_median[j] = (length(which(diff_median_per>=diff_median))+1)/(nperm+1)
  }
  est_perm_power_IQR =  length(perm_pvalue_IQR[perm_pvalue_IQR<=0.05])/nrep
  est_perm_se_IQR = sqrt(est_perm_power_IQR*(1-est_perm_power_IQR)/nrep)
  est_perm_power_median =  length(perm_pvalue_median[perm_pvalue_median<=0.05])/nrep
  est_perm_se_median = sqrt(est_perm_power_median*(1-est_perm_power_median)/nrep)
  perm_lower_IQR[i] = est_perm_power_IQR-qnorm(0.975)*est_perm_se_IQR
  perm_lower_median[i] = est_perm_power_median-qnorm(0.975)*est_perm_se_median
}

# KS test
nrep = 10000
## storage
ks_pvalue = rep(NA, nrep)
ks_lower = rep(NA, n)
for (i in 2:n) {
  ## Monte Carlo
  for (j in 1:nrep) {
    ## Generate data
    t_UD = rt(1000, k1)
    income_UD = sort(abs(s1*t_UD+m1))
    t_CH = rt(i, k3)
    income_CH = sort(abs(s3*t_CH+m3))
    ## test for power
    ks_test = ks.test(income_CH, income_UD)
    ks_pvalue[j] = ks_test$p.value
  }
  est_ks_power =  length(ks_pvalue[ks_pvalue<=0.05])/nrep
  est_ks_se = sqrt(est_ks_power*(1-est_ks_power)/nrep)
  ks_lower[i] = est_ks_power-qnorm(0.975)*est_ks_se
}

## plot
plot(1:n, ks_lower, 
     main="n VS lower 95% CI of power",
     ylim=c(0,1),
     ylab="power",
     xlab="n",
     col="blue",
     type="l",
     lwd=3)
ks_n = 2
while (ks_lower[ks_n] < 0.8) {
  ks_n = ks_n+1
}
abline(v=ks_n, col="blue", lty=2)

lines(1:n, perm_lower_IQR, 
      col="green", lwd=3)
perm_n_IQR = 2
while (perm_lower_IQR[perm_n_IQR] < 0.8) {
  perm_n_IQR = perm_n_IQR+1
}
abline(v=perm_n_IQR, col="green", lty=2)

lines(1:n, perm_lower_median, 
      col="orange", lwd=3)
perm_n_median = 2
while (perm_lower_median[perm_n_median] < 0.8) {
  perm_n_median = perm_n_median+1
}
abline(v=perm_n_median, col="orange", lty=2)
abline(h=0.8, col="red", lwd=2)
legend("bottomright",
       c("KS test", "Permutation on IQR", "Permutation on Median"),
       col=c("blue", "green", "orange"),
       pch=19,
       cex=0.4)

c(ks_n, perm_n_IQR, perm_n_median)

###### Test 4: UD income and DS income ######
n = 1000
nvec = seq(2, n, 50)

# permutation test
nrep = 100
nperm = 1000
## storage
perm_pvalue_IQR = rep(NA, nrep)
perm_lower_IQR = c()
perm_pvalue_median = rep(NA, nrep)
perm_lower_median = c()
diff_IQR = IQR(income_DS)-IQR(income_UD)
diff_median = median(income_UD)-median(income_DS)
diff_IQR_per = rep(NA, nrep)
diff_median_per = rep(NA, nrep)
data_pull = c(income_DS, income_UD)
for (i in nvec) {
  for (j in 1:nrep) {
    ## permutation
    for (k in 1:nperm) {
      ## test for power
      w_per = sample(2000, 2*i, replace=FALSE)
      data_per = data_pull[w_per]
      UD_new = data_per[1:i]
      DS_new = data_per[(i+1):(2*i)]
      diff_new_IQR = IQR(UD_new)-IQR(DS_new)
      diff_new_median = median(DS_new)-median(UD_new)
      diff_IQR_per[k] = diff_new_IQR
      diff_median_per[k] = diff_new_median
    }
    perm_pvalue_IQR[j] = (length(which(diff_IQR_per>=diff_IQR))+1)/(nperm+1)
    perm_pvalue_median[j] = (length(which(diff_median_per>=diff_median))+1)/(nperm+1)
  }
  est_perm_power_IQR =  length(perm_pvalue_IQR[perm_pvalue_IQR<=0.05])/nrep
  est_perm_se_IQR = sqrt(est_perm_power_IQR*(1-est_perm_power_IQR)/nrep)
  est_perm_power_median =  length(perm_pvalue_median[perm_pvalue_median<=0.05])/nrep
  est_perm_se_median = sqrt(est_perm_power_median*(1-est_perm_power_median)/nrep)
  perm_lower_IQR = append(perm_lower_IQR, est_perm_power_IQR-qnorm(0.975)*est_perm_se_IQR)
  perm_lower_median = append(perm_lower_median, 
                             est_perm_power_median-qnorm(0.975)*est_perm_se_median)
}

# KS test
nrep = 10000
## storage
ks_pvalue = rep(NA, nrep)
ks_lower = c()
for (i in nvec) {
  ## Monte Carlo
  for (j in 1:nrep) {
    ## Generate data
    t_UD = rt(i, k1)
    income_UD = sort(abs(s1*t_UD+m1))
    t_DS = rt(i, k2)
    income_DS = sort(abs(s2*t_DS+m2))
    ## test for power
    ks_test = ks.test(income_UD, income_DS)
    ks_pvalue[j] = ks_test$p.value
  }
  est_ks_power =  length(ks_pvalue[ks_pvalue<=0.05])/nrep
  est_ks_se = sqrt(est_ks_power*(1-est_ks_power)/nrep)
  ks_lower = append(ks_lower, est_ks_power-qnorm(0.975)*est_ks_se)
}
## plot
plot(nvec, ks_lower, 
     main="n VS lower 95% CI of power",
     ylab="power",
     xlab="n",
     col="blue",
     type="l",
     lwd=3)
lines(nvec, perm_lower_IQR, 
      col="green", lwd=3)
lines(nvec, perm_lower_median, 
      col="orange", lwd=3)
index = 1
ks_n = 2
while (ks_lower[index] < 0.8) {
  ks_n = ks_n+50
  index = index+1
}
abline(v=ks_n, col="blue", lty=2)
index = 1
perm_n_IQR = 2
while (perm_lower_IQR[index] < 0.8) {
  perm_n_IQR = perm_n_IQR+50
  index = index+1
}
abline(v=perm_n_IQR, col="green", lty=2)
index = 1
perm_n_median = 2
while (perm_lower_median[index] < 0.8) {
  perm_n_median = perm_n_median+50
  index = index+1
}
abline(v=perm_n_median, col="orange", lty=2)
abline(h=0.8, col="red", lwd=2)
legend("topleft",
       c("KS test", "Permutation on IQR", "Permutation on Median"),
       col=c("blue", "green", "orange"),
       pch=19,
       cex=0.4)

c(ks_n, perm_n_IQR, perm_n_median)

###### Compare Variance ######
# permutation
nperm = 10000
## mix income and expenditure
## storage
diff_IQR = rep(NA, nperm)
diff_median = rep(NA, nperm)
data_pull = c(mix_income, expenditure)
for (i in 1:nperm) {
  ## test for power
  w_per = sample(2000, 2000, replace=FALSE)
  data_per = data_pull[w_per]
  income_new = data_per[1:1000]
  expend_new = data_per[1001:2000]
  diff_IQR[i] = IQR(income_new)-IQR(expend_new)
  diff_median[i] = median(income_new)-median(expend_new)
}
hist(diff_IQR, col="red",
     main="Histogram of Difference",
     xlab="difference")
hist(diff_median, add=TRUE, col="blue")
legend("topright", c("Diff in IQR", "Diff in Median"),
       col=c("red", "blue"), lty=1, cex=0.5)
var1_IQR = var(diff_IQR)
var1_median = var(diff_median)

## Downtown Seattle and Capitol Hill
## storage
diff_IQR = rep(NA, nperm)
diff_median = rep(NA, nperm)
data_pull = c(income_DS, income_CH)
for (i in 1:nperm) {
  ## test for power
  w_per = sample(2000, 2000, replace=FALSE)
  data_per = data_pull[w_per]
  DS_new = data_per[1:1000]
  CH_new = data_per[1001:2000]
  diff_IQR[i] = IQR(CH_new)-IQR(DS_new)
  diff_median[i] = median(CH_new)-median(DS_new)
}
hist(diff_IQR, col="red",
     main="Histogram of Difference",
     xlab="difference")
hist(diff_median, add=TRUE, col="blue")
legend("topright", c("Diff in IQR", "Diff in Median"),
       col=c("red", "blue"), lty=1, cex=0.5)
var2_IQR = var(diff_IQR)
var2_median = var(diff_median)

## University District and Capitol Hill
## storage
diff_IQR = rep(NA, nperm)
diff_median = rep(NA, nperm)
data_pull = c(income_UD, income_CH)
for (i in 1:nperm) {
  ## test for power
  w_per = sample(2000, 2000, replace=FALSE)
  data_per = data_pull[w_per]
  UD_new = data_per[1:1000]
  CH_new = data_per[1001:2000]
  diff_IQR[i] = IQR(CH_new)-IQR(UD_new)
  diff_median[i] = median(CH_new)-median(UD_new)
}
hist(diff_IQR, col="red",
     main="Histogram of Difference",
     xlab="difference")
hist(diff_median, add=TRUE, col="blue")
legend("topright", c("Diff in IQR", "Diff in Median"),
       col=c("red", "blue"), lty=1, cex=0.5)
var3_IQR = var(diff_IQR)
var3_median = var(diff_median)

## Downtown Seattle and University District
## storage
diff_IQR = rep(NA, nperm)
diff_median = rep(NA, nperm)
data_pull = c(income_DS, income_UD)
for (i in 1:nperm) {
  ## test for power
  w_per = sample(2000, 2000, replace=FALSE)
  data_per = data_pull[w_per]
  DS_new = data_per[1:1000]
  UD_new = data_per[1001:2000]
  diff_IQR[i] = IQR(DS_new)-IQR(UD_new)
  diff_median[i] = median(DS_new)-median(UD_new)
}
hist(diff_IQR, col="red",
     ylim=c(0,2000),
     main="Histogram of Difference",
     xlab="difference")
hist(diff_median, add=TRUE, col="blue")
legend("topright", c("Diff in IQR", "Diff in Median"),
       col=c("red", "blue"), lty=1, cex=0.5)
var4_IQR = var(diff_IQR)
var4_median = var(diff_median)