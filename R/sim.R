#' Simulation function
#'
#' @param size The sample size
#' @param nvariate The number of variable
#' @param prob The probability vector of distributions (probability of hidden variable)
#' @param mu The vector of means of normal distributions
#' @param sigma The vector of variances of normal distributions
#' @param betay Variable effects
#'
#' @return
#' \item{state}{The hidden variable generated from a multinomial distribution}
#' \item{contt}{Simulated values generated from a mixture of normal distributions}
#' @export
#'
sim=function(size, nvariate, prob, mu, sigma, betay){
  dd = data.frame(matrix(nrow = size, ncol = nvariate))
  state = data.frame(matrix(nrow = size, ncol = nvariate))
  contt = data.frame(matrix(nrow = size, ncol = nvariate))
  for (j in 1:nvariate) {
    sta = sample(x = c(1,2,3,4), size = size, replace = T, prob = prob)
    cont = sta
    for (i in 1:size) {
      cont[i]=ifelse(sta[i]==1, rnorm(1, mean = mu[1], sd = sigma[1]),
                     ifelse(sta[i]==2, rnorm(1, mean = mu[2], sd = sigma[2]),
                            ifelse(sta[i]==3, rnorm(1, mean = mu[3], sd = sigma[3]),
                                   rnorm(1, mean = mu[4], sd = sigma[4]))))
    }

    contt[,j] = cont
    state[,j] = sta
  }

  y = (betay[1]*state[,1] + betay[2]*state[,2])
  y = rnorm(size) + y

  state = cbind(y,state)
  contt = cbind(y,contt)
  return(data = list(state, contt))
}
