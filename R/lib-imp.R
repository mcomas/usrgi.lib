### NA dependance
na.dependance = function(d, vmiss, omit){
  RR = llply(vmiss, function(vmiss1){
    vnames = setdiff(names(d), c(vmiss1, omit))
    formula = sprintf("glm(is.na(%s)~%s, family='binomial', data=d)", vmiss1, 
                      paste( vnames, collapse = '+'))
    mod = eval(parse(text=formula))
    s = summary(mod)
    tau = cbind(exp(cbind( s$coefficients[-1,1], confint.default(mod)[-1,])),
                s$coefficients[-1,3])
    row.names(tau) = vnames
    res = matrix(NA, nrow=length(names(d)), ncol=4)
    rownames(res) = names(d)
    colnames(res) = colnames(tau)
    res[vnames, ] = tau
    res })
  names(RR) = vmiss
  RR
}

### Logistic regression
logistic.dependance = function(d, vmiss, omit){
  RR = llply(vmiss, function(vmiss1){
    vnames = setdiff(names(d), c(vmiss1, omit))
    formula = sprintf("glm(%s~%s, family='binomial', data=d)", vmiss1, 
                      paste( vnames, collapse = '+'))
    mod = eval(parse(text=formula))
    s = summary(mod)
    tau = cbind(exp(cbind( s$coefficients[-1,1], confint.default(mod)[-1,])),
                s$coefficients[-1,3])
    row.names(tau) = vnames
    res = matrix(NA, nrow=length(names(d)), ncol=4)
    rownames(res) = names(d)
    colnames(res) = colnames(tau)
    res[vnames, ] = tau
    res })
  names(RR) = vmiss
  RR
}

### Regression
regression.dependance = function(d, vmiss, omit){
  RR = llply(vmiss, function(vmiss1){
    vnames = setdiff(names(d), c(vmiss1, omit))
    formula = sprintf("lm(%s~%s, data=d)", vmiss1, paste( vnames, collapse = '+'))
    mod = eval(parse(text=formula))
    s = summary(mod)
    tau = cbind(s$coefficients[-1,1], confint.default(mod)[-1,], s$coefficients[-1,3])
    row.names(tau) = vnames
    res = matrix(NA, nrow=length(names(d)), ncol=4)
    rownames(res) = names(d)
    colnames(res) = colnames(tau)
    res[vnames, ] = tau
    res })
  names(RR) = vmiss
  RR
}

sum_dependances <- function(d, ignore_vars, restrictions ){
  dd = d[, setdiff(names(d), ignore_vars)]
  na.sum = do.call('c', llply(restrictions, function(l){
    na.dependance(dd, l$v, omit = c(ignore_vars, l$omit)) }))
  values.sum = do.call('c', llply(restrictions, function(l){
    if(l$model == 'reg'){
      regression.dependance(dd, l$v, omit = c(ignore_vars, l$omit)) 
    } else{ 
      logistic.dependance(dd, l$v, omit = c(ignore_vars, l$omit))} }))
  DF.na = do.call('cbind', llply(names(na.sum), function(v){
    l = na.sum[[v]]
    df = data.frame('val'=sprintf("    OR: %5.3f (%5.2f, %5.2f) z=%5.1f", l[,1], 
                                  ifelse(is.na(l[,2]), 0, l[,2]), ifelse(l[,3]>10000, 
                                                                         Inf, l[,3]), l[,4]))
    df$val = as.character(df$val)
    df[is.na(l[,1]), ] = "---"
    names(df) = v
    df }))
  rownames(DF.na) = rownames(na.sum[[1]])
  DF.values = do.call('cbind', llply(names(values.sum), function(v){
    l = values.sum[[v]]
    df = data.frame('val'=sprintf("    coef: %6.2f (%6.2f, %6.2f) z=%5.1f", l[,1], 
                                  ifelse(is.na(l[,2]), 0, l[,2]), ifelse(l[,3]>10000, 
                                                                         Inf, l[,3]), l[,4]))
    df$val = as.character(df$val)
    df[is.na(l[,1]), ] = "---"
    names(df) = v
    df }))
  rownames(DF.values) = rownames(values.sum[[1]])
  vars = llply(names(na.sum), function(v){
    df = na.sum[[v]]
    v_omited = rownames(df)[is.na(df[,3])]
    v_na = rownames(df)[!is.na(df[,3]) & abs(df[,3]) > 1.96]
    df = values.sum[[v]]
    v_values = rownames(df)[!is.na(df[,3]) & abs(df[,4]) > 1.96]
    list('omited' = v_omited, 'na' = v_na, 'values' = v_values) })
  names(vars) = names(na.sum)
  list('na' = DF.na, 'values' = DF.values, 'vars' = vars)
}

idiap_mi = function(data.imp, v_missing, mi_pred_matrix, mi_post, mi_method = list(), ...){  
  print('imputacio buida')
  ini <- mice(data.imp, meth = "norm", maxit = 0 )
  visit.sequence <- ini$visitSequence
  visit.sequence <- visit.sequence[v_missing]  
  meth <- ini$method
  for(v in names(mi_method)) meth[v] = mi_method[[v]]
  pred.matrix <- ini$predictorMatrix
  pred.matrix[,] = 0
  for(v in names(mi_pred_matrix)) pred.matrix[v, mi_pred_matrix[[v]]] = 1
  post <- ini$post
  for (v in names(mi_post)) post[v] = mi_post[[v]]
  print('Imputació ')
  mice(data.imp, method=meth, predictorMatrix=pred.matrix, 
       visitSequence = visit.sequence, post=post, ...) 
}