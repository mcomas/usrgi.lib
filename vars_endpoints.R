# END POINTS

# Explicació:
#   Creem la varialbe "ep_death" (amb format Date) a partir de les variables exitus i dexitus.
# Input: 
#   - exitus: var car amb tres nivells: D (= death), E (= end. Final d'estudi) i T (= trasllat).
#   - dexitus: data on succeix exitus.
# Output:
#   - data on es mor o NA si no es mor
create.ep_death = function(exitus, dexitus){
  res = as.Date( rep(NA, length(exitus)))
  select = exitus == 'D'
  res[select] = dexitus[select]
  res
}

# Explicació:
#   Creació d'un nou end-point com a combinació de diferents end-points.
# Input:
#   - deps: matriu de dates on cada columna correspon a un end-point diferent.
# Output:
#   - data on succeix el primer end-point o NA si no n'hi ha cap
create.ep_combinate = function(deps){ 
  res = deps[,1]
  for(col in 2:ncol(deps)){
    na.res = is.na(res)
    nona.deps = ! is.na(deps[,col])
    sel = ( na.res & nona.deps ) | ( ! na.res &  nona.deps & deps[,col] < res)
    res[sel] = deps[sel, col]
  }
  res
}

# Explicació:
#   Funció que converteix els end-points en una variable indicadora i en el temps fins a l'end-point.
# Input:
#   - dataset: matriu de dades
#   - time_to_event: si és TRUE, calcula el temps fins a l'end-point
#                    Per defecte, FALSE
#   - check_exitus: si és TRUE, es comprova si els end-points succeixen abans de l'exitus.
#                   Per defecte, FALSE
#   - eps: vector amb els noms dels end-points
# Output:
#   - dataset: dataset original afeguint les variables indicadores i de temps de l'end-point.
format.endpoints = function(dataset, time_to_event = FALSE, check_exitus=FALSE, 
                            eps = names(dataset)[substring(names(dataset), 1, 3) == "ep_"],
                            silently = FALSE){
  if(check_exitus){
    for(v in eps){
      sel = !is.na(dataset[,v]) & dataset$dexitus < dataset[,v] 
      dataset[sel,v] = NA
    }
  }
  for(v in eps){
    if(!silently){
      cat("i.", v, ': created\n', sep='')
    }
    dataset[,paste("i.", v, sep="")] = as.numeric(!is.na(dataset[,v]))
  }
  if(time_to_event){
    dataset = time.to.event(dataset, eps = eps, silently = silently)
  }
  dataset
}

# Explicació:
#   Funció auxliar de la funció format.endpoints.
#   Per cada end-point, crea el temps  fins a l'end-point o final d'estudi, el que succeix més aviat.
# Input:
#   - dataset: matriu de dades
#   - eps: noms dels diferents end-points que volem transformar
# Output:
#   - t.ep: temps fins a l'end-point o final d'estudi.
time.to.event = function(dataset, eps, silently = FALSE){
  for(v in eps){
    if(!silently){
      cat("t.", v, ': created\n', sep="")
    }
    dataset[,paste("t.", v, sep="")] = as.numeric(dataset[,'dexitus'] - dataset[,'dintro'])
    sel = !is.na(dataset[,v])
    dataset[sel,paste("t.", v, sep="")] = as.numeric(dataset[sel,v] - dataset[sel,'dintro'])
  }
  dataset
}

# Explicació:
#   Funció que censura tot els end-points posteriors al primer d'un conjunt seleccionat.
# Input: 
#   - dataset: matriu de dades.
#   - end-points: conjunt d'end-points en que buscarem el mínim a partir del qual censurarem la resta.
#                 Per defecte, c('ami', 'angor', 'stroke_i', 'stroke_e', 'pad', 'tia')
# Output:
#   - dataset: amb els end-points modificats.
follow_up.censured = function(dataset, endpoints = c('ami', 'angor', 'stroke_i', 'stroke_e', 'pad', 'tia'), modify_date = FALSE){
  
  tmin = apply(dataset[,paste0('t.ep_', endpoints)], 1 , min)
  s_ep = apply(dataset[,paste0('i.ep_', endpoints)], 1 , sum) > 0
  
  dataset[,'exitus'] = factor(dataset[,'exitus'], levels=c('D', 'E', 'T', '*'))
  dataset[s_ep, 'exitus'] = '*'
  for(ep in names(dataset)[substr(names(dataset), 1, 3)  == 'ep_']){
    difftime = dataset[,paste0('t.', ep)] - tmin
    modif = s_ep & ( difftime > 0 | difftime <= 0 & paste0('i.', ep) == 0 )
    dataset[modif, paste0('i.', ep)] = 0
    dataset[modif, paste0('t.', ep)] = tmin[modif]
  }
  # S'actualitza la variable dexitus amb la informació de ep_death
  dataset[,'dexitus'] = dataset[,'dintro'] + tmin
  if( modify_date ){
    for(ep in names(dataset)[substr(names(dataset), 1, 3)  == 'ep_']){
      difftime = dataset[,paste0('t.', ep)] - tmin
      modif = s_ep & ( difftime > 0 | difftime <= 0 & paste0('i.', ep) == 0 )
      dataset[modif, ep] = dataset[modif,'dexitus']
      dataset[dataset[,paste0('t.', ep)] == 0, ep] = NA
    }
  }
  dataset
}

# Explicació:
#   Funció que censura un endpoint fins a un periode concret
# Input: 
#   - dataset: matriu de dades.
#   - end-points: conjunt d'end-points en que buscarem es censurarà arribat a un temps period)
#   - period: seguiment de l'endpoint
# Output:
#   - dataset: amb els end-points modificats.
adverse.effect_until = function(data, eps_until, period = 365){
  for(e in eps_until){
    i.e = paste('i.', e, sep='')
    t.e = paste('t.', e, sep='')
    e1 = paste(e, '_1y', sep='')
    i.e1 = paste('i.', e1, sep='')
    t.e1 = paste('t.', e1, sep='')
    data[, e1] = data[,e]
    data[, i.e1] = data[,i.e]
    data[, t.e1] = data[,t.e]
    sel = data[, t.e1] > period
    data[sel, i.e1] = 0
    data[sel, t.e1] = period
  }
  data
}
# Explicació:
#   Funció que censura un endpoint abans d'un periode concret
# Input: 
#   - dataset: matriu de dades.
#   - end-points: conjunt d'end-points en que buscarem es censurarà arribat a un temps period)
#   - period: seguiment de l'endpoint
# Output:
#   - dataset: amb els end-points modificats.
adverse.effect_from =  function(data, eps_from, period = 365){
  for(e in eps_from){
    i.e = paste('i.', e, sep='')
    t.e = paste('t.', e, sep='')
    e1 = paste(e, '_1y', sep='')
    i.e1 = paste('i.', e1, sep='')
    t.e1 = paste('t.', e1, sep='')
    data[, e1] = data[,e]
    data[, i.e1] = data[,i.e]
    data[, t.e1] = data[,t.e]
    sel = data[, t.e1] <= period
    data[sel, i.e1] = 0
    data[data[,i.e1] == 0, e1] = NA
    data[, t.e1] = data[, t.e1] - period
    sel = data[, t.e1] < 0
    data[sel, t.e1] = 0
  }
  data
}
