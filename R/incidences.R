#' Incidence coefficient
#' 
#' @param inc \code{\link{incidence}} object
#' @return coefficient for the incidence
#' 
#' @export
coef.incidence = function(inc){
  inc$inc
}

#' Incidence coefficient
#' 
#' @param inc \code{\link{incidence}} object
#' @return variance estimator for the incidence
#' 
#' @export
vcov.incidence = function(inc){
  if(inc$inc == 0){
    matrix(0)
  }
  matrix( ( (inc$hi - inc$inc) / qnorm(1 - inc$alpha/2) )^2 )
}


#' Desnity incidence coefficient
#' 
#' @param event vector with the indicator variable indicating the event
#' @param time vector with time to event for each person
#' @param alpha significance for the given interval (default 0.05)
#' @param period years for denominator (default 1)
#' @param pers numerator for the density (default 1000)
#' @return return an incidence object
#' 
#' @export
incidence = function(event, time, alpha = 0.05, period = 1, pers = 1000){
  n_temps = 365.242199 * period * pers
  tab = table(event)
  if (length(tab)<2){
    res = c(0, 0)
    names(res) = c('0', '1')
    res[names(tab)] = tab
    tab = res
  }
  #total = tapply(time, event, length)
  #   time.count = tapply(time, event, sum)
  time.count = sum(time)
  incidence = tab/time.count * n_temps
  inc.lo = incidence-qnorm(1 - alpha/2)*sqrt(tab)/time.count * n_temps
  inc.hi = incidence+qnorm(1 - alpha/2)*sqrt(tab)/time.count * n_temps
  res = list(ev = tab, time = time.count, inc = incidence['1'], lo = inc.lo['1'], hi = inc.hi['1'], 
             alpha = alpha, period=period, pers=pers)
  class(res) = 'incidence'
  res
}

#' Incidence summary
#' 
#' @param inc \code{\link{incidence}} object
#' @return print an incidence object
#' 
#' @export
print.incidence = function(x){
  cat(sprintf("population: %d events: %d", sum(x$ev), x$ev['1']),
      sprintf("incidence density and %2.f%% ci: %.4f  (%.4f, %.4f)  [%dp / %dy]", 100*(1-x$alpha),
              x$inc, x$lo, x$hi, x$pers, x$period), sep='\n')
}