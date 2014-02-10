
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

###
###
### FUNCIONS DESCRIPTIVES
summary_numeric = function(x){
  list('type'= 'numeric', 'n' = length(x), 'na' = sum(is.na(x)), 'mean' = mean(x,  na.rm=TRUE), 'sd' = sd(x,  na.rm=TRUE))
}
sum_imp_numeric = function(x){
  res = list()
  res[['type']] = 'numeric'
  res[['n']] = mean(laply(x, function(y){y[['n']]}))
  res[['na']] = max(laply(x, function(y){y[['na']]}))
  res[['mean']] = mean(laply(x, function(y){y[['mean']]}))
  res[['sd']] = sqrt(sum(laply(x, function(y){y[['sd']]^2}))/length(x))
  res
}
summary_factor = function(x, base = NULL){
  res = list('type'= 'factor', 'n' = length(x), 'na' = sum(is.na(x)), 'tab' = table(x))
  if( ('0' %in% names(res[['tab']]) | '1' %in% names(res[['tab']])) & length(res[['tab']]) == 1){
    x = factor(x, levels=c('0','1'))
    res = list('type'= 'factor', 'n' = length(x), 'na' = sum(is.na(x)), 'tab' = table(x))
  }
  if(is.null(base)){
    if('1' %in% names(res[['tab']])){
      res[['base']] = '1'
    }else{
      res[['base']] = names(res[['tab']])[1]
    }
  }else{
    res[['base']] = base
  }
  res
}
sum_imp_factor = function(x){
  res = list()
  res[['type']] = 'factor'
  res[['n']] = mean(laply(x, function(y){y[['n']]}))
  res[['na']] = max(laply(x, function(y){y[['na']]}))
  levels = Reduce(union, llply(x, function(y){names(y[['tab']])}))
  tab.imp = llply(x, function(y){
    tab = sapply(levels, function(l){
      if(l %in% names(y[['tab']])){
        y[['tab']][l]
      }else{
        0
      }
    })
    names(tab) = levels
    tab
  })
  res[['tab']] = apply(ldply(tab.imp),2, mean)
  res[['base']] = x[[1]][['base']]
  res
}
summaries = function(d, v_numeric, v_factor, v_factor_specific){
  vars = sort(c(v_numeric, v_factor, names(v_factor_specific)))
  sms = list()
  for(v in vars){
    if(v %in% v_numeric){
      sms[[v]] = summary_numeric(d[,v])
    }
    if(v %in% v_factor){
      sms[[v]] = summary_factor(d[,v])
    }
    if(v %in% names(v_factor_specific)){
      sms[[v]] = summary_factor(d[,v], v_factor_specific[[v]][['base']])
    }
  }
  sms
}
merge_summaries = function(dl){
  final = list()
  for(v in names(dl[[1]])){
    x_imp = llply(dl, function(d){d[[v]]})
    if(dl[[1]][[v]][['type']] == 'numeric'){
      final[[v]] = sum_imp_numeric(x_imp)
    }
    if(dl[[1]][[v]][['type']] == 'factor'){
      final[[v]] = sum_imp_factor(x_imp)
    }
  }
  final
}

print_summaries = function(sms){
  col = sprintf("%s", sms[[1]]$n)
  col.nms = 'n'
  for(v in names(sms)){
    col = c(col, '')
    col.nms = c(col.nms, v)
    i = length(col)
    if(sms[[v]]$type == 'numeric'){
      if( sms[[v]]$na != 0 ){
        col[i] = sprintf(" na:%5.2f%% %5.2f (%6.2f)", sms[[v]]$na / sms[[v]]$n*100, sms[[v]]$mean, sms[[v]]$sd )
      }else{
        col[i] = sprintf("             %5.2f (%6.2f)", sms[[v]]$mean, sms[[v]]$sd )
      }
    }
    if(sms[[v]]$type == 'factor'){
      if(length(sms[[v]]$tab) <= 2){
        if(sms[[v]]$base != '1' | length(sms[[v]]$tab) == 1){
          col.nms[i] = sprintf("%s[%s]", v, sms[[v]]$base)
        }
        m = sms[[v]]$tab[sms[[v]]$base]
        if( sms[[v]]$na != 0 ){
          col[i] = sprintf( "na:%5.2f%%  %d (%5.2f%%)", sms[[v]]$na/ sms[[v]]$n*100, as.integer(m), 100*m / (sms[[v]]$n - sms[[v]]$na) )
        }else{
          col[i] = sprintf( "            %d (%5.2f%%)", as.integer(m), 100*m / sms[[v]]$n )
        }
      }
      if(length(sms[[v]]$tab) > 2){
        col = col[1:(i-1)]
        col.nms = col.nms[1:(i-1)]
        for(l in names(sms[[v]]$tab)){
          col = c(col, sprintf( "            %d (%5.2f%%)", as.integer(sms[[v]]$tab[l]), 100*sms[[v]]$tab[l] / sms[[v]]$n ))
          col.nms = c(col.nms, sprintf("%s[%s]", v, l))
        }
      }
    }
  }
  names(col) = col.nms
  col
}

standardized_differences = function(A, B){
  ## http://support.sas.com/resources/papers/proceedings12/335-2012.pdf
  d = rep(0, length(A))
  names(d) = names(A)
  for(v in names(A)){
    if(A[[v]]$type == 'numeric'){
      d[v] = (A[[v]]$mean - B[[v]]$mean) / sqrt( (A[[v]]$sd^2 + B[[v]]$sd^2) / 2 )
    }
    if(A[[v]]$type == 'factor'){
      pa = A[[v]]$tab[A[[v]]$base] / (A[[v]]$n - A[[v]]$na)
      pb = B[[v]]$tab[B[[v]]$base] / (B[[v]]$n - B[[v]]$na)
      d[v] = (pa-pb) / sqrt( (pa * ( 1-pa) + pb * ( 1-pb))/2 )
    }
  }
  d
}

print_prediction = function(imp){
  cat("\\begin{description}\n")
  for(v in names(imp$visitSequence) ){
    form = gsub("¬", "\\", stringr::str_replace_all(paste(names(imp$predictorMatrix[v,  imp$predictorMatrix[v, ] == 1]), collapse=' + '), "_", "¬_"), fixed=TRUE)
    if(imp$method[v] %in% c('norm', 'logreg')){
      deriv = sprintf("\\texttt{glm(%s \\~{} %s, \\textbf{family='%s'})}", v, form, imp$method[v]) 
    }else{
      deriv = gsub("¬", "\\",  stringr::str_replace_all(imp$method[v], "\\^", "¬^{}"), fixed=TRUE)
      deriv = gsub("¬", "\\",  stringr::str_replace_all(deriv, "~", "¬~{}"), fixed=TRUE)
    }
    cat(sprintf("\\item[%s]  %s \n", v, deriv))
  }
  cat("\\end{description}\n")
}