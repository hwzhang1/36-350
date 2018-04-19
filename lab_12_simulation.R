# 2a.
generate_data = function(n,p) {
  covariates = matrix(NA, nrow=n, ncol=p)
  for (i in 1:n) {
    draw = rnorm(p, 0, 1)
    covariates[i,] = draw
  }
  responses = rnorm(n, 0, 1)
  return(list(covariates = covariates, responses = responses))
}

# 2b.
model_select = function(covariates, responses, cutoff) {
  temp.lm = lm(responses ~ covariates)
  temp.coeffs = summary(temp.lm)$coefficients
  red.ind = which(temp.coeffs[2:nrow(temp.coeffs),4] < cutoff)
  if (length(red.ind) == 0 ){
    return(c())
  }
  red.lm = lm(responses ~ covariates[,red.ind])
  red.coeffs = as.vector(summary(red.lm)$coefficients[,4])
  return(red.coeffs)
}

# 2c.
run_simulation = function(n_trials=100, n, p, cutoff=0.05) {
  pvals = c() 
  for (i in 1:n_trials) {
    trial = generate_data(n, p)
    pval = model_select(trial$covariates, trial$response, cutoff)
    pvals = c(pvals, pval)
    }
  hist(pvals, xlim=c(0,1), cex.main = 0.7,
       main = paste("Histogram of the p-values(n=",n,", p=",p,")", sep=""))
  return(pvals)
}
