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