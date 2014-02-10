
#Incidència
incidence = function(event, time, alpha = 0.05, period = 1, pers = 1000){
  tab = table(event)
  #total = tapply(time, event, length)
  time.count = tapply(time, event, sum)
  incidence = tab/sum(time.count) * 365 * period * pers
  inc.lo = incidence-qnorm(0.95 + alpha/2)*sqrt(tab)/sum(time.count) * 365 * period * pers
  inc.hi = incidence+qnorm(0.95 + alpha/2)*sqrt(tab)/sum(time.count) * 365 * period * pers
  res = list(ev = tab, time = time.count, inc = incidence['1'], lo = inc.lo['1'], hi = inc.hi['1'], 
             alpha = alpha, period=period, pers=pers)
  class(res) = 'incidence'
  res
}
quantify.bias = function(x, p){
  rr_u = (x$ev['1'] / x$ev['0']) * ((x$time['0']/x$time['1'] + p) / (1-p))
  rr_b = (x$ev['1'] / x$ev['0']) * x$time['0']/x$time['1']
  rr_b / rr_u
}

print.incidence = function(x){
  cat(sprintf("population: %d events: %d", sum(x$ev), x$ev['1']),
      sprintf("incidence density and %2.f%% ci: %.4f  (%.4f, %.4f)  [%dp / %dy]", 100*(1-x$alpha),
              x$inc, x$lo, x$hi, x$pers, x$period), sep='\n')
}

idiap_describe.factor = function(x){
  tab = table(x, useNA='ifany')
  ptab = prop.table(tab)
  sprintf("%s: %5.2f (%d)", names(tab), 100*ptab, tab)
}
idiap_prevalence = function(x){
  tab = table(x, useNA='ifany')
  ptab = prop.table(tab)
  if('1' %in% names(tab)){
    sprintf("%5.2f%% (%d)", 100*ptab['1'], tab['1'])
  }else{
    sprintf("%5.2f%% (%d)", 0, 0)
  }
}