#Prevalença crua i ajustada (per sexe, edat o ambdos)
prev.adj <- function(d=data, vec.ep=endpoint, 
                     sex=T, pesos.sex=10841904*rep(1/2,2), 
                     age=NULL, pesos.age=NULL, stratum=NULL, num.dec=4){
  if(!is.null(stratum) & !is.factor(data[,stratum])){
    warning("La variable stratum no és un factor")
  }
  if (sex & is.null(age)){
    d$var.adj <- with(d, get("sex"))
    mat.prev <- prev.1fac(d, vec.ep, pesos.sex, levels(d$sex), stratum, num.dec)
  } else if (!sex & !is.null(age)){
    d$var.adj <- d[,age]
    mat.prev <- prev.1fac(d, vec.ep, pesos.age, levels(d$var.adj),stratum, num.dec)
  } else{
    mat.prev <- prev.2fac(d, vec.ep, age, 1/2*rep(pesos.age,each=2),
                          paste( levels(d$sex), rep( levels(d[,age]), each=2 ) ),
                          stratum, num.dec)
  }
  mat.prev
}
  
prev.1fac <- function(d,vec.ep,pesos, nam, stratum, num.dec){
  mat.prev<-NULL
  for (ep in vec.ep){
    d$event <- with(d, get(paste0("i.",ep)))
    if(is.null(stratum)){
      obs <- matrix(with(d, table(event,var.adj)[2,]), nrow = 1, byrow = TRUE, 
                    dimnames = list(ep, nam))
      pop <- matrix(table(d$var.adj), nrow = 1, byrow = TRUE, 
                    dimnames = list(ep, nam))
    } else {
      d$strat <- d[,stratum]
      nam.strat <- levels(d$strat)
      obs <- matrix(with(d, table(strat,var.adj,event)[,,2]), nrow = length(nam.strat), 
                    dimnames = list(nam.strat, nam))
      pop <- matrix(with(d, table(strat,var.adj)), nrow = length(nam.strat), 
                    dimnames = list(nam.strat, nam))
    }
    std <- matrix(pesos, nrow= 1, byrow = TRUE, dimnames = list("", nam))
    prev <- epi.directadj(obs, pop, std, units = 100, conf.level = 0.95)    
    cru <- with(prev$crude.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-", 
                                         round(upper,num.dec),")"))
    adj <- with(prev$adj.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-", 
                                       round(upper,num.dec),")"))
    if(is.null(stratum)){
      mat.prev <- rbind(mat.prev, c(sum(obs),sum(pop),cru, adj))
    } else{
      mat.prev <- rbind(mat.prev, cbind(apply(obs,1,sum), apply(pop,1,sum), cru, adj))
    }
  }
  if(is.null(stratum)){
    dimnames(mat.prev) <- list(vec.ep, c("Events", "Population","Crude Prevalence", 
                                         "Adjusted Prevalence"))
  } else {
    dimnames(mat.prev) <- list(paste(rep(vec.ep, each=length(nam.strat)), rep(nam.strat, length(vec.ep))), 
                              c("Obs", "Population", "Crude Prevalence", "Adjusted Prevalence"))}
  mat.prev
}

prev.2fac <- function(d,vec.ep, age, pesos, nam, stratum, num.dec){
  mat.prev <- NULL
  for (ep in vec.ep){
    d$event <- with(d, get(paste0("i.",ep)))
    d$age.adj <- d[, age]
    if(is.null(stratum)){
      obs <- matrix(with(d, table(sex,age.adj,event)[,,2]), nrow = 1, byrow = FALSE,
                    dimnames = list(ep, nam))
      pop <- matrix(with(d, table(sex,age.adj)), nrow = 1, byrow = FALSE, 
                    dimnames = list(ep, nam))
    } else{
      d$strat <- d[,stratum]
      nam.strat <- levels(d$strat)
      aux.obs <- with(d, table(strat,sex,age.adj,event))
      aux.pop <- with(d, table(strat,sex,age.adj))
      obs <- c(aux.obs[1,,,2])
      pop <- c(aux.pop[1,,])
      for(.i in 2:dim(aux.obs)[1]){
        obs <- rbind(obs, c(aux.obs[.i,,,2]))
        pop <- rbind(pop, c(aux.pop[.i,,]))
      }
      dimnames(obs) <- dimnames(pop) <- list(nam.strat, nam)
    }
    std <- matrix(pesos, nrow= 1, byrow = TRUE, dimnames = list(ep, nam))
    prev <- epi.directadj(obs, pop, std, units = 100, conf.level = 0.95)
    cru <- with(prev$crude.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-", 
                                         round(upper,num.dec),")"))
    adj <- with(prev$adj.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-",
                                       round(upper,num.dec),")"))
    if(is.null(stratum)){
      mat.prev <- rbind(mat.prev, c(sum(obs),sum(pop),cru, adj))
    } else{      
      mat.prev <- rbind(mat.prev, cbind(apply(obs,1,sum), apply(pop,1,sum), cru,adj))
    }
  }
#   print(mat.prev)
  if(is.null(stratum)){
    dimnames(mat.prev) <- list(vec.ep, c("Event", "Population", "Crude Prevalence", 
                                         "Age-Sex-adjusted Prevalence"))
  } else{
    dimnames(mat.prev) <- list(paste(rep(vec.ep, each=length(nam.strat)), nam.strat), 
                                          c("Obs", "Population", "Crude Prevalence", 
                                            "Age-Sex-adjusted Prevalence"))
  }
  mat.prev
}
coef.incidence = function(inc){
  inc$inc
}
vcov.incidence = function(inc){
  if(inc$inc == 0){
    matrix(0)
  }
  matrix( ( (inc$hi - inc$inc) / qnorm(1 - inc$alpha/2) )^2 )
}
#Incidència
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

print.incidence = function(x){
  cat(sprintf("population: %d events: %d", sum(x$ev), x$ev['1']),
      sprintf("incidence density and %2.f%% ci: %.4f  (%.4f, %.4f)  [%dp / %dy]", 100*(1-x$alpha),
              x$inc, x$lo, x$hi, x$pers, x$period), sep='\n')
}

#Incidència crua i ajustada (per sexe, edat o ambdos)
inc.adj <- function(d=data, vec.ep=endpoint, 
                    sex=T, pesos.sex=10841904*rep(1/2,2), 
                    age=NULL, pesos.age=NULL, stratum=NULL, 
                    num.dec=4, n_any=365.242199){
  if(!is.null(stratum) & !is.factor(data[,stratum])){
    warning("La variable stratum no és un factor")
  }
  if (sex & is.null(age)){
    d$var.adj <- with(d, get("sex"))
    mat.inc <- inc.1fac(d, vec.ep, pesos.sex, levels(d$sex), stratum, num.dec, n_any)
  } else if (!sex & !is.null(age)){
    d$var.adj <- d[,age]
    mat.inc <- inc.1fac(d, vec.ep, pesos.age, levels(d$var.adj), stratum, num.dec, n_any)
  } else{
    mat.inc <- inc.2fac(d, vec.ep, age, 1/2*rep(pesos.age,each=2),
                        paste( levels(d$sex), rep( levels(d[,age]), each=2 ) ),
                        stratum, num.dec, n_any)
  }
  mat.inc
}

inc.1fac <- function(d,vec.ep,pesos, nam, stratum, num.dec, n_any){
  mat.inc<-NULL
  for (ep in vec.ep){
    d$event <- d[, paste0("i.",ep)]
    d$time <- d[, paste0("t.",ep)]
    if(is.null(stratum)){
      a.obs <- NULL
      a.pop <- NULL
      for (.nlev in 1:length(nam)){
        aux <- with(subset(d,var.adj == nam[.nlev]), incidence(event,time))
        a.obs <- c(a.obs,aux$ev[2])
        a.pop <- c(a.pop,aux$time)
      }
      obs <- matrix(a.obs, nrow = 1, byrow = TRUE, dimnames = list(ep, nam))
      pop <- matrix(a.pop/n_any, nrow = 1, byrow = TRUE, dimnames = list(ep, nam))
    } else {
      d$strat <- d[,stratum]
      nam.strat <- levels(d$strat)
      a.obs <- NULL
      a.pop <- NULL
      for (.nstrat in 1:length(nam.strat)){
        aa.obs <- NULL
        aa.pop <- NULL
        for (.nlev in 1:length(nam)){
          aux <- with(subset(d, strat == nam.strat[.nstrat] & var.adj == nam[.nlev]), incidence(event,time))
          aa.obs <- c(aa.obs,aux$ev[2])
          aa.pop <- c(aa.pop,aux$time)
        }
        a.obs <- rbind(a.obs, aa.obs)
        a.pop <- rbind(a.pop, aa.pop)
      }
      obs <- matrix(a.obs, nrow = length(nam.strat), dimnames = list(nam.strat, nam))
      pop <- matrix(a.pop/n_any, nrow = length(nam.strat), dimnames = list(nam.strat, nam))
    }
    std <- matrix(pesos, nrow= 1, byrow = TRUE, dimnames = list("", nam))
    inc <- epi.directadj(obs, pop, std, units = 100, conf.level = 0.95)    
    cru <- with(inc$crude.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-", 
                                         round(upper,num.dec),")"))
    adj <- with(inc$adj.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-", 
                                       round(upper,num.dec),")"))
    if(is.null(stratum)){
      mat.inc <- rbind(mat.inc, c(sum(obs),sum(pop),cru, adj))
    } else{
      mat.inc <- rbind(mat.inc, cbind(apply(obs,1,sum), apply(pop,1,sum), cru, adj))
    }
  }
  if(is.null(stratum)){
    dimnames(mat.inc) <- list(vec.ep, c("Events", "Population","Crude Incidence", 
                                         "Adjusted Incidence"))
  } else {
    dimnames(mat.inc) <- list(paste(rep(vec.ep, each=length(nam.strat)), rep(nam.strat, length(vec.ep))), 
                               c("Obs", "Population", "Crude Incidence", "Adjusted Incidence"))}
  mat.inc
}

inc.2fac <- function(d,vec.ep, age, pesos, nam, stratum, num.dec, n_any){
  mat.inc <- NULL
  for (ep in vec.ep){
    d$event <- d[, paste0("i.",ep)]
    d$time  <- d[,paste0("t.",ep)]
    d$age.adj <- d[, age]
    if(is.null(stratum)){
      a.obs <- NULL
      a.pop <- NULL
      for (.nage in levels(d$age.adj)){
        for (.nsex in levels(d$sex)){
          aux <- with(subset(d,sex == .nsex & age.adj == .nage), incidence(event,time))
          a.obs <- c(a.obs,aux$ev[2])
          a.pop <- c(a.pop,aux$time)
        }
      }
      obs <- matrix(a.obs, nrow = 1, byrow = TRUE, dimnames = list(ep, nam))
      pop <- matrix(a.pop/n_any, nrow = 1, byrow = TRUE, dimnames = list(ep, nam))
    } else{
      d$strat <- d[,stratum]
      nam.strat <- levels(d$strat)
      a.obs <- NULL
      a.pop <- NULL
      for (.nstrat in nam.strat){
        aa.obs <- NULL
        aa.pop <- NULL
        for (.nage in levels(d$age.adj)){
          for (.nsex in levels(d$sex)){
            aux <- with(subset(d,strat == .nstrat & sex == .nsex & age.adj == .nage), incidence(event,time))
            aa.obs <- c(aa.obs,aux$ev[2])
            aa.pop <- c(aa.pop,aux$time)
          }
        }
        a.obs <- rbind(a.obs, aa.obs)
        a.pop <- rbind(a.pop, aa.pop)
      } 
      obs <- matrix(a.obs, nrow = 2, dimnames = list(nam.strat, nam))
      pop <- matrix(a.pop/n_any, nrow = 2, dimnames = list(nam.strat, nam))
      dimnames(obs) <- dimnames(pop) <- list(nam.strat, nam)
    }
    std <- matrix(pesos, nrow= 1, byrow = TRUE, dimnames = list(ep, nam))
    inc <- epi.directadj(obs, pop, std, units = 100, conf.level = 0.95)
    cru <- with(inc$crude.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-", 
                                         round(upper,num.dec),")"))
    adj <- with(inc$adj.strata,paste0(round(est,num.dec)," (", round(lower,num.dec), "-",
                                       round(upper,num.dec),")"))
    if(is.null(stratum)){
      mat.inc <- rbind(mat.inc, c(sum(obs),sum(pop),cru, adj))
    } else{      
      mat.inc <- rbind(mat.inc, cbind(apply(obs,1,sum), apply(pop,1,sum), cru,adj))
    }
  }
  #   print(mat.inc)
  if(is.null(stratum)){
    dimnames(mat.inc) <- list(vec.ep, c("Event", "Population", "Crude Incidence", 
                                         "Age-Sex-adjusted Incidence"))
  } else{
    dimnames(mat.inc) <- list(paste(rep(vec.ep, each=length(nam.strat)), nam.strat), 
                               c("Obs", "Population", "Crude Incidence", 
                                 "Age-Sex-adjusted Incidence"))
  }
  mat.inc
}


quantify.bias = function(x, p){
  rr_u = (x$ev['1'] / x$ev['0']) * ((x$time['0']/x$time['1'] + p) / (1-p))
  rr_b = (x$ev['1'] / x$ev['0']) * x$time['0']/x$time['1']
  rr_b / rr_u
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
  list('type'= 'numeric', 'n' = length(x), 'na' = sum(is.na(x)), 'mean' = mean(x,  na.rm=TRUE),
       'sd' = sd(x,  na.rm=TRUE), 'min' = min(x, na.rm=TRUE), 'max' = max(x, na.rm=TRUE),
       'quantile' = quantile(x, probs=c(0.05,0.25,0.5,0.75,0.95), na.rm=TRUE))
}
sum_imp_numeric = function(x){
  res = list()
  res[['type']] = 'numeric'
  res[['n']] = mean(laply(x, function(y){y[['n']]}))
  res[['na']] = max(laply(x, function(y){y[['na']]}))
  res[['mean']] = mean(laply(x, function(y){y[['mean']]}))
  res[['sd']] = sqrt(sum(laply(x, function(y){y[['sd']]^2}))/length(x))
  res[['min']] = mean(laply(x, function(y){y[['min']]}))
  res[['max']] = mean(laply(x, function(y){y[['max']]}))
  res[['quantile']] = c(mean(laply(x, function(y){y[['quantile']][1]})),
                        mean(laply(x, function(y){y[['quantile']][2]})),
                        mean(laply(x, function(y){y[['quantile']][3]})),
                        mean(laply(x, function(y){y[['quantile']][4]})),
                        mean(laply(x, function(y){y[['quantile']][5]})))
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
summaries = function(d, v_numeric = c(), v_factor = c(), v_factor_specific = c()){
  vars = c(v_numeric, v_factor, names(v_factor_specific))
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
        col[i] = sprintf("na:%5.2f%% %5.2f (%6.2f)", sms[[v]]$na / sms[[v]]$n*100,
                         sms[[v]]$mean, sms[[v]]$sd )
      }else{
        col[i] = sprintf("           %5.2f (%6.2f)", sms[[v]]$mean, sms[[v]]$sd )
      }
    }
    if(sms[[v]]$type == 'factor'){
      if(length(sms[[v]]$tab) <= 2 & ('0' %in% names(sms[[v]]$tab) & '1' %in% names(sms[[v]]$tab)) ){
        if(sms[[v]]$base != '1' | length(sms[[v]]$tab) == 1){
          col.nms[i] = sprintf("%s[%s]", v, sms[[v]]$base)
        }
        m = sms[[v]]$tab[sms[[v]]$base]
        if( sms[[v]]$na != 0 ){
          col[i] = sprintf( "na:%5.2f%%  %d (%5.2f%%)", sms[[v]]$na/ sms[[v]]$n*100, as.integer(m),
                            100*m / (sms[[v]]$n - sms[[v]]$na) )
        }else{
          col[i] = sprintf( "            %d (%5.2f%%)", as.integer(m), 100*m / sms[[v]]$n )
        }
      }
      if(length(sms[[v]]$tab) > 2 |  !('0' %in% names(sms[[v]]$tab) & '1' %in% names(sms[[v]]$tab))){
        if( sms[[v]]$na != 0 ){
          
        }
        col = col[1:(i-1)]
        col.nms = col.nms[1:(i-1)]
        for(l in names(sms[[v]]$tab)){
          if(l == names(sms[[v]]$tab)[1] & sms[[v]]$na != 0){
            col = c(col, sprintf( "na:%5.2f%%  %5d (%5.2f%%)", sms[[v]]$na/ sms[[v]]$n*100,
                                  as.integer(sms[[v]]$tab[l]), 100*sms[[v]]$tab[l] / sms[[v]]$n ))
          }else{
            col = c(col, sprintf( "            %5d (%5.2f%%)", as.integer(sms[[v]]$tab[l]),
                                  100*sms[[v]]$tab[l] / sms[[v]]$n ))
          }
          col.nms = c(col.nms, sprintf("%s[%s]", v, l))
        }
      }
    }
  }
  names(col) = col.nms
  col
}

print_summaries_numeric = function(sms){
  col = c(rep("",8),sprintf("%s", sms[[1]]$n))
  col.nms = 'n'
  for(v in names(sms)){
    col = rbind(col, rep('',9))
    col.nms = c(col.nms, v)
    i = dim(col)[1]
    if(sms[[v]]$type == 'numeric'){
      if( sms[[v]]$na != 0 ){
        col[i,1] = sprintf("%5.2f%%", sms[[v]]$na / sms[[v]]$n*100)
        col[i,2] = sprintf("%5.2f", sms[[v]]$min)
        col[i,3] = sprintf("%5.2f", sms[[v]]$quantile[1])
        col[i,4] = sprintf("%5.2f", sms[[v]]$quantile[2])
        col[i,5] = sprintf("%5.2f", sms[[v]]$quantile[3])
        col[i,6] = sprintf("%5.2f", sms[[v]]$quantile[4])
        col[i,7] = sprintf("%5.2f", sms[[v]]$quantile[5])
        col[i,8] = sprintf("%5.2f", sms[[v]]$max)
        col[i,9] = sprintf("%5.2f (%6.2f)", sms[[v]]$mean, sms[[v]]$sd )
      }else{
        col[i,2] = sprintf("%5.2f", sms[[v]]$min)
        col[i,3] = sprintf("%5.2f", sms[[v]]$quantile[1])
        col[i,4] = sprintf("%5.2f", sms[[v]]$quantile[2])
        col[i,5] = sprintf("%5.2f", sms[[v]]$quantile[3])
        col[i,6] = sprintf("%5.2f", sms[[v]]$quantile[4])
        col[i,7] = sprintf("%5.2f", sms[[v]]$quantile[5])
        col[i,8] = sprintf("%5.2f", sms[[v]]$max)
        col[i,9] = sprintf("%5.2f (%6.2f)", sms[[v]]$mean, sms[[v]]$sd )
      }
    }
  }
  colnames(col) <- c("na", "min", "5%", "1st", "media", "3rd", "95%", "max", "mean (sd)")
  row.names(col) = col.nms
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
