### NA dependance
# na.dependance = function(d, vmiss, omit){
#   RR = llply(vmiss, function(vmiss1){
#     vnames = setdiff(names(d), c(vmiss1, omit))
#     formula = sprintf("glm(is.na(%s)~%s, family='binomial', data=d)", vmiss1, 
#                       paste( vnames, collapse = '+'))
#     mod = eval(parse(text=formula))
#     s = summary(mod)
#     tau = cbind(exp(cbind( s$coefficients[-1,1], confint.default(mod)[-1,])),
#                 s$coefficients[-1,3])
#     row.names(tau) = vnames
#     res = matrix(NA, nrow=length(names(d)), ncol=4)
#     rownames(res) = names(d)
#     colnames(res) = colnames(tau)
#     res[vnames, ] = tau
#     res })
#   names(RR) = vmiss
#   RR
# }
# New version
na.dependance = function(d, vmiss, omit){
  RR = llply(vmiss, function(vmiss1){
    vnames = setdiff(names(d), c(vmiss1, omit))
    formula = sprintf("glm(is.na(%s)~%s, family='binomial', data=d)", vmiss1, 
                      paste( vnames, collapse = '+'))
    mod = eval(parse(text=formula))
    s = summary(mod)
    data.frame('odd.ratio' = exp(s$coefficients[-1, 'Estimate']), 
               'z.value' = s$coefficients[-1,'z value'])
  })
  names(RR) = vmiss
  RR
}
### Logistic regression
# logistic.dependance = function(d, vmiss, omit){
#   RR = llply(vmiss, function(vmiss1){
#     vnames = setdiff(names(d), c(vmiss1, omit))
#     formula = sprintf("glm(%s~%s, family='binomial', data=d)", vmiss1, 
#                       paste( vnames, collapse = '+'))
#     mod = eval(parse(text=formula))
#     s = summary(mod)
#     tau = cbind(exp(cbind( s$coefficients[-1,1], confint.default(mod)[-1,])),
#                 s$coefficients[-1,3])
#     row.names(tau) = vnames
#     res = matrix(NA, nrow=length(names(d)), ncol=4)
#     rownames(res) = names(d)
#     colnames(res) = colnames(tau)
#     res[vnames, ] = tau
#     res })
#   names(RR) = vmiss
#   RR
# }
# New version
logistic.dependance = function(d, vmiss, omit){
  RR = llply(vmiss, function(vmiss1){
    vnames = setdiff(names(d), c(vmiss1, omit))
    formula = sprintf("glm(%s~%s, family='binomial', data=d)", 
                      vmiss1, paste( vnames, collapse = '+'))
    mod = eval(parse(text=formula))
    s = summary(mod)
    data.frame('odd.ratio' = exp(s$coefficients[-1, 'Estimate']), 
               'z.value' = s$coefficients[-1,'z value'])
  })
  names(RR) = vmiss
  RR
}

### Regression
# regression.dependance = function(d, vmiss, omit){
#   RR = llply(vmiss, function(vmiss1){
#     vnames = setdiff(names(d), c(vmiss1, omit))
#     formula = sprintf("lm(%s~%s, data=d)", vmiss1, paste( vnames, collapse = '+'))
#     mod = eval(parse(text=formula))
#     s = summary(mod)
#     tau = cbind(s$coefficients[-1,1], confint.default(mod)[-1,], s$coefficients[-1,3])
#     row.names(tau) = vnames
#     res = matrix(NA, nrow=length(names(d)), ncol=4)
#     rownames(res) = names(d)
#     colnames(res) = colnames(tau)
#     res[vnames, ] = tau
#     res })
#   names(RR) = vmiss
#   RR
# }
# New version
regression.dependance = function(d, vmiss, omit){
  RR = llply(vmiss, function(vmiss1){
    vnames = setdiff(names(d), c(vmiss1, omit))
    formula = sprintf("lm(%s~%s, data=d)", vmiss1, paste( vnames, collapse = '+'))
    mod = eval(parse(text=formula))
    s = summary(mod)
    data.frame('coefficient' = s$coefficients[-1, 'Estimate'], 't.value' = s$coefficients[-1,'t value'])
  })
  names(RR) = vmiss
  RR
}

# sum_dependances <- function(d, ignore_vars, restrictions ){
#   dd = d[, setdiff(names(d), ignore_vars)]
#   na.sum = do.call('c', llply(restrictions, function(l){
#     na.dependance(dd, l$v, omit = c(ignore_vars, l$omit)) }))
#   values.sum = do.call('c', llply(restrictions, function(l){
#     if(l$model == 'reg'){
#       regression.dependance(dd, l$v, omit = c(ignore_vars, l$omit)) 
#     } else{ 
#       logistic.dependance(dd, l$v, omit = c(ignore_vars, l$omit))} }))
#   DF.na = do.call('cbind', llply(names(na.sum), function(v){
#     l = na.sum[[v]]
#     df = data.frame('val'=sprintf("    OR: %5.3f (%5.2f, %5.2f) z=%5.1f", l[,1], 
#                                   ifelse(is.na(l[,2]), 0, l[,2]), ifelse(l[,3]>10000, 
#                                                                          Inf, l[,3]), l[,4]))
#     df$val = as.character(df$val)
#     df[is.na(l[,1]), ] = "---"
#     names(df) = v
#     df }))
#   rownames(DF.na) = rownames(na.sum[[1]])
#   DF.values = do.call('cbind', llply(names(values.sum), function(v){
#     l = values.sum[[v]]
#     df = data.frame('val'=sprintf("    coef: %6.2f (%6.2f, %6.2f) z=%5.1f", l[,1], 
#                                   ifelse(is.na(l[,2]), 0, l[,2]), ifelse(l[,3]>10000, 
#                                                                          Inf, l[,3]), l[,4]))
#     df$val = as.character(df$val)
#     df[is.na(l[,1]), ] = "---"
#     names(df) = v
#     df }))
#   rownames(DF.values) = rownames(values.sum[[1]])
#   vars = llply(names(na.sum), function(v){
#     df = na.sum[[v]]
#     v_omited = rownames(df)[is.na(df[,3])]
#     v_na = rownames(df)[!is.na(df[,3]) & abs(df[,3]) > 1.96]
#     df = values.sum[[v]]
#     v_values = rownames(df)[!is.na(df[,3]) & abs(df[,4]) > 1.96]
#     list('omited' = v_omited, 'na' = v_na, 'values' = v_values) })
#   names(vars) = names(na.sum)
#   list('na' = DF.na, 'values' = DF.values, 'vars' = vars)
# }
#New version
sum_dependances = function(d, ignore_vars, restrictions, alpha=0.05 ){
  dd = d[,setdiff( names(d), ignore_vars)]
  na.sum = do.call('c', llply(restrictions, function(l){
    na.dependance(dd, l$v, omit = c(ignore_vars, l$omit))
  }))
  values.sum = do.call('c', llply(restrictions, function(l){
    if(l$model == 'reg')
      regression.dependance(dd, l$v, omit = c(ignore_vars, l$omit))
    else
      logistic.dependance(dd, l$v, omit = c(ignore_vars, l$omit))
  }))
  DF.na = data.frame('row.names' = Reduce('union', llply(na.sum, row.names)))
  for(v.na in names(na.sum) ){
    df.na = na.sum[[v.na]]
    DF.na[,v.na] = "---"
    DF.na[rownames(df.na),v.na] = sprintf("    z=%5.1f", df.na$z.value)
    #DF.na[rownames(df.na),v.na] = sprintf("    or: %5.2f z=%5.1f", 
    #                                      df.na$odd.ratio,  df.na$z.value)
  }
  DF.values = data.frame('row.names' = Reduce('union', llply(na.sum, row.names)))
  for(v.values in names(values.sum) ){
    df.val = values.sum[[v.values]]
    DF.values[,v.values] = "---"
    if('z.value' %in% names(df.val)){
      DF.values[rownames(df.val),v.values] = sprintf("    z=%5.1f", df.val$z.value)
      #DF.values[rownames(df.val),v.values] = 
      #  sprintf("    or: %5.2f z=%5.1f", df.val$odd.ratio,  df.val$z.value)
    }else{
      #DF.values[rownames(df.val),v.values] = sprintf("    t=%5.1f", df.val$t.value)
      DF.values[rownames(df.val),v.values] = 
        sprintf("    coef: %5.2f t=%5.1f", df.val$coefficient,  df.val$t.value)
    }    
  }
  vars = llply(names(na.sum), function(v){
    df.na = na.sum[[v]]
    df.values = values.sum[[v]]
    #v_na = row.names(df.na)[ abs(df.na$z.value) > 1.96 ]
    v_na = row.names(df.na)[ abs(df.na$z.value) > abs(qnorm(alpha/2)) ]
    #v_values = row.names(df.values)[ abs(df.values[,2]) > 1.96 ]
    v_values = row.names(df.values)[ abs(df.values[,2]) > abs(qnorm(alpha/2)) ]
    list('na' = v_na, 'values' = v_values)
  })
  names(vars) = names(na.sum)
  list('na' = DF.na, 'values' = DF.values, 'vars' = vars)
}

# idiap_mi = function(data.imp, v_missing, mi_pred_matrix, mi_post, mi_method = list(), ...){  
#   print('imputacio buida')
#   ini <- mice(data.imp, meth = "norm", maxit = 0 )
#   visit.sequence <- ini$visitSequence
#   visit.sequence <- visit.sequence[v_missing]  
#   meth <- ini$method
#   for(v in names(mi_method)) meth[v] = mi_method[[v]]
#   pred.matrix <- ini$predictorMatrix
#   pred.matrix[,] = 0
#   for(v in names(mi_pred_matrix)) pred.matrix[v, mi_pred_matrix[[v]]] = 1
#   post <- ini$post
#   for (v in names(mi_post)) post[v] = mi_post[[v]]
#   print('Imputació ')
#   mice(data.imp, method=meth, predictorMatrix=pred.matrix, 
#        visitSequence = visit.sequence, post=post, ...) 
# }
#New version
idiap_mi = function(data.imp, v_missing_order = NULL, mi_method = NULL, 
                    mi_pred_matrix = NULL, ...){
  #ini <- mice(data.imp, meth = "norm", maxit = 0 )
  ini <- mice(data.imp, meth = "fastpmm", maxit = 0 )
  visit.sequence <- ini$visitSequence
  if(!is.null(v_missing_order)){
    visit.sequence <- visit.sequence[v_missing_order]
  }
  meth <- ini$method
  if(!is.null(mi_method)){
    for(v in names(mi_method)) meth[v] = mi_method[[v]]    
  }
  pred.matrix <- ini$predictorMatrix
  pred.matrix[,'ocip'] = 0
  pred.matrix[,'dintro'] = 0
  if(!is.null(mi_pred_matrix)){
    pred.matrix[,] = 0
    for(v in names(mi_pred_matrix)) pred.matrix[v, mi_pred_matrix[[v]]] = 1
  }
  mice(data.imp, method=meth, pre=pred.matrix, visitSequence = visit.sequence, ...)
}

#Noves funcions
mi_merge = function(imp, data){
  llply(1:imp$m, function(i){
    d.t = complete(imp, i)
    ## El dataset imputat estar desordenat, s'ha d'ordenar
    row.names(d.t) = d.t$ocip
    d.miss = data.frame(ifelse(is.na(data[,names(imp$visitSequence)]), 1, 0))
    names(d.miss) = paste0(names(d.miss),".na")
    data = cbind( d.t[ data$ocip, ], data[, setdiff(names(data), names(d.t))], 
                  d.miss)
    data
  })
}

imputation = function(data, restrictions, v_missing, v_explanatory, 
                      v_interact = NULL,  v_model = NULL,
                      v_outcome = NULL, ...){
  all_vars = union( 'ocip', c(v_missing, v_explanatory, v_interact, v_model, 
                              v_outcome) )
  if( length(vrs <- setdiff(all_vars, names(data))) > 0)
    stop( sprintf("Variables not available: %s", paste(vrs, collapse=', ')) )
  if(!is.null(v_interact)){
    interact = do.call('interaction', data.frame(data[, v_interact]))
    D.imp = split(data[, all_vars], interact)
  }else{
    D.imp = list(data[,all_vars])
  }
  IMP = llply(D.imp, function(d.imp){
    ini <- mice( d.imp, meth = "norm", maxit = 0 )
    s.dependance = sum_dependances( d.imp, 
                                    ignore_vars = c('ocip', v_interact), 
                                    restrictions = restrictions)
    pred.matrix <- ini$predictorMatrix
    pred.matrix[,] = 0
    l_ply(names(s.dependance$vars), function(v){
      pred.matrix[v, union(s.dependance$vars[[v]]$na, 
                           s.dependance$vars[[v]]$values) ] <<- 1
      if(!is.null(v_model))
        pred.matrix[v, v_model] <<- 1
      if(!is.null(v_outcome))
        pred.matrix[v, v_outcome] <<- 1
    })
    l_ply(restrictions, function(l){
      if('dependance' %in% l)
        pred.matrix[l$dependance, l$depandance]<<-matrix(1, nrow=length(l$dependance), ncol=length(l$dependance)) - diag(1, length(l$dependance))
      else
        pred.matrix[l$v, l$v]  <<- matrix(1, nrow=length(l$v), 
                                          ncol=length(l$v))-diag(1, length(l$v))
    })
    visitSequence = ini$visitSequence[v_missing]
    mice(d.imp,visitSequence=visitSequence,pre=pred.matrix,meth=ini$method,...)
  })
  imp = Reduce('rbind.mids', IMP)
  llply(1:imp$m, function(i){
    d.t = complete(imp, i)
    ## El dataset imputat estar desordenat, s'ha d'ordenar
    row.names(d.t) = d.t$ocip
    d.miss = data.frame(ifelse(is.na(data[,v_missing]), 1, 0))
    names(d.miss) = paste0(names(d.miss),".na")
    data = cbind( d.t[ data$ocip, ], data[, setdiff(names(data), names(d.t))], 
                  d.miss)
    data
  })
}
