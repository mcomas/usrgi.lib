#' Cardiovascular risk calculation according to REGICOR model
#' 
#' @param age age in years
#' @param men indicator of men
#' @param smoking indicator of smoker
#' @param diabetes indicator of diabetes
#' @param coltot total cholesterol measure in mg/l
#' @param colhdl high density cholesterol measure in mg/l
#' @param sbp systolic blood pressure
#' @param dbp diastolic blood pressure
#' @param years year to where the risk is calculated (default:10)
#' @return REGICOR risk
#' 
#' @export
regicor = function(age, men, smoking, diabetes, coltot, colhdl, sbp, dbp, years = 10){
  smoking = as.numeric(as.character(smoking))
  diabetes = as.numeric(as.character(diabetes))
  bp_opti = ifelse( sbp <  120 & dbp < 80 , 1, 0 )
  bp_high = ifelse( (130 <= sbp & sbp < 140) | (85 <= dbp & dbp < 90), 1, 0 )
  bp_i = ifelse( (140 <= sbp & sbp < 160) | (90 <= dbp & dbp < 100), 1, 0 )
  bp_ii = ifelse( 160 <= sbp | 100 <= dbp, 1, 0 )
  
  i_bp_ii = (bp_ii == 1)
  bp_opti[i_bp_ii] = bp_high[i_bp_ii] = bp_i[i_bp_ii] = 0
  
  i_bp_i = (bp_i == 1)
  bp_opti[i_bp_i] = bp_high[i_bp_i] =  0
  
  i_bp_high = (bp_high == 1)
  bp_opti[i_bp_high]  =  0
  
  c_160 = ifelse( coltot < 160, 1, 0 )
  c200_239 = ifelse( 200 <= coltot & coltot < 240, 1, 0 )
  c240_279 = ifelse( 240 <= coltot & coltot < 280, 1, 0 )
  c280_ = ifelse( 280 <= coltot, 1, 0 )
  h_35 = ifelse( colhdl < 35, 1, 0 )
  h35_44 = ifelse( 35 <= colhdl & colhdl < 45, 1, 0 )
  h45_49 = ifelse( 45 <= colhdl & colhdl < 50, 1, 0 )
  h50_59 = ifelse( 50 <= colhdl & colhdl < 60, 1, 0 )
  h60_ = ifelse( 60 <= colhdl, 1, 0 )
  
  n = length(age)
  
  l_chol = rep(0, n)
  g_chol = rep(0, n)
  l_chol[men] = ( (0.04826*age)- (0.65945* c_160) + 
                    (0.17692* c200_239) +(0.50539* c240_279) + (0.65713* c280_) + 
                    (0.49744* h_35) + (0.24310* h35_44) - (0.05107* h50_59) - 
                    (0.48660*h60_) - (0.00226 * bp_opti) + (0.28320 *  bp_high) + 
                    (0.52168 * bp_i) + (0.61859 * bp_ii) + (0.42839 * diabetes) + (0.52337* smoking) )[men]
  g_chol[men] =  3.489
  
  women = 1 - men
  l_chol[women] = ( (0.33766*age)  - (0.00268 * age^2)- (0.26138* c_160) + 
                      (0.20771* c200_239) + (0.24385 * c240_279) + (0.53513* c280_) + 
                      (0.84312* h_35) + (0.377096* h35_44) + (0.19785* h45_49) - 
                      (0.42951*h60_)- (0.53363 * bp_opti) - (0.06773 *  bp_high) + 
                      (0.26288 * bp_i) + (0.46573 * bp_ii) + (0.59626 * diabetes) + (0.29246* smoking))[women]
  g_chol[women] = 10.279
  
  b_chol = exp(l_chol - g_chol)
  
  result = rep(0,n)
  result[men] = (1- (1-(1-0.951)/(10/years))^b_chol[men]) * 100 
  result[women] = (1-(1-(1-0.978)/(10/years))^b_chol[women]) * 100
  result
}

#' Blood presure categorization according to REGICOR model
#' 
#' @param sbp systolic blood pressure
#' @param dbp diastolic blood pressure
#' @return BP levels
#' 
#' @export
bp_cat = function(dbp, sbp){
  bp = rep('normal', length(dbp))
  bp[sbp <  120 & dbp < 80] = 'opti'
  bp[(130 <= sbp & sbp < 140) | (85 <= dbp & dbp < 90)] = 'high'
  bp[(140 <= sbp & sbp < 160) | (90 <= dbp & dbp < 100)] = 'i'
  bp[160 <= sbp | 100 <= dbp] = 'ii'
  bp[bp == 'normal' & (is.na(dbp) | is.na(sbp))] = NA
  factor(bp, levels = c('normal', 'opti', 'high', 'i', 'ii'))
}

#' Total cholesterol categorization according to REGICOR model
#' 
#' @param coltot total cholesterol
#' @return coltot levels
#' 
#' @export
coltot_cat = function(coltot){
  x = cut(coltot, breaks=c(0, 160, 200, 240, 280, Inf), right=F)
  factor(x, levels=c("[160,200)", "[0,160)", "[200,240)", "[240,280)", "[280,Inf)"))
}

#' High density cholesterol categorization according to REGICOR model
#' 
#' @param colhdl High density cholesterol
#' @return colhdl levels
#' 
#' @export
colhdl_cat = function(colhdl){
  x = cut(colhdl, breaks=c(0, 35, 45, 50, 60, Inf), right=F)
  factor(x, levels=c("[45,50)", "[0,35)", "[35,45)", "[50,60)", "[60,Inf)"))
}


#' Calculate an date event variable for death
#' 
#' @param exitus exitus reason
#' @param date of exitus
#' @param death level indicating death
#' @return creates a date variable with the date of death for dose dying
#' 
#' @export
create.ep_death = function(exitus, dexitus, death = 'D'){
  res = as.Date( rep(NA, length(exitus)))
  select = exitus == death
  res[select] = dexitus[select]
  res
}

time.to.event = function(dataset, eps, silently = FALSE){
  for(v in eps){
    if(!silently){
      cat("t.", v, ': created\n', sep="")
    }
    dataset[,paste("t.", v, sep="")] = as.numeric(dataset[['dexitus']] - dataset[['dintro']])
    sel = !is.na(dataset[,v])
    dataset[sel,paste("t.", v, sep="")] = as.numeric(dataset[[v]][sel] - dataset[['dintro']][sel])
  }
  dataset
}

#' This function converts a given outcome in date format with time and event variables
#' 
#' @param dataset original dataset
#' @param time_to_event calculate time to event
#' @param check_exitus if TRUE all eps variables have to appear after dexitus date
#' @param dexitus (default: 'dexitus')
#' @param eps endpoints to calculate. (default: all variables starting with 'ep_')
#' @param silently print messages (default = FALSE)
#' @return original dataset with time and event variables included
#' 
#' @export
create.endpoints = function(dataset, time_to_event = TRUE, check_exitus=FALSE, dexitus='dexitus',
                            eps = names(dataset)[substring(names(dataset), 1, 3) == "ep_"],
                            silently = TRUE){
  if(check_exitus){
    for(v in eps){
      sel = !is.na(dataset[,v]) & dataset[,dexitus] < dataset[,v] 
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

#' Create a combined end-point from different endpoints
#' 
#' @param deps enpoints
#' @return returns the combined endpoint
#' 
#' @export
create.ep_combinate = function(deps){ 
  res = deps[[1]]
  for(col in 2:ncol(deps)){
    na.res = is.na(res)
    nona.deps = ! is.na(deps[[col]])
    sel = ( na.res & nona.deps ) | ( ! na.res &  nona.deps & deps[[col]] < res)
    res[sel] = deps[[col]][sel]
  }
  res
}


#' Fix the level of statine treatment, using PETREA definition or binary
#' 
#' @param dose statin dose
#' @param atc statin type
#' @param definition character 'PETREA' (default), '2-levels'
#' @return returns the statin level
#' 
#' @export
create.statine_level = function(dose, atc, definition = 'PETREA'){
  if(definition == 'PETREA'){
    low = ( (dose == 20 | dose == 40) & atc == 'C10AA04' ) |
      ( (dose == 10 | dose == 20) & atc == 'C10AA02' ) |
      ( (dose == 10 ) & atc == 'C10AA03' ) |
      ( (dose == 5 ) & atc == 'C10AA01' )    
    moderate = ( (dose == 10 ) & atc == 'C10AA05' ) |
      ( (dose == 80 ) & atc == 'C10AA04' ) |
      ( (dose == 40 ) & atc == 'C10AA02' ) |
      ( (dose == 20 | dose == 40 | dose == 80) & atc == 'C10AA03' ) |
      ( (dose == 10 | dose == 20 ) & atc == 'C10AA01' )    
    high = ( (dose == 20 | dose == 40) & atc == 'C10AA05' ) |
      ( (dose == 40 | dose == 80 ) & atc == 'C10AA01' ) |
      ( (dose == 5 | dose == 10 ) & atc == 'C10AA07' )    
    very_high = ( (dose == 80 ) & atc == 'C10AA05' ) | 
      ( (dose == 20 | dose == 40 ) & atc == 'C10AA07' )
    level = NA
    level[low] = 1
    level[moderate] = 2
    level[high] = 3
    level[very_high] = 4
  }else if(definition == '2-levels'){
    low = ( atc == 'C10AA01' & (dose ==  5 | dose == 10 | dose == 20) ) |
      ( atc == 'C10AA02' & (dose == 10 | dose == 20 | dose == 40) ) |
      ( atc == 'C10AA03' & (dose == 10 | dose == 20 | dose == 40) ) |
      ( atc == 'C10AA04' & (dose == 10 | dose == 20 | dose == 40) ) |
      ( atc == 'C10AA05' & (dose == 10) )
    high = ( atc == 'C10AA01' & (dose ==  40 | dose == 80) ) |
      ( atc == 'C10AA02' & (dose == 80) ) |
      ( atc == 'C10AA03' & (dose == 80) ) |
      ( atc == 'C10AA04' & (dose == 80) ) |
      ( atc == 'C10AA05' & (dose == 20 | dose == 40 | dose == 80) ) |
      ( atc == 'C10AA07' )
    level = NA
    level[low] = 1
    level[high] = 2
  }
  level
}

#' Format variables
#' 
#' @param dataset
#' @param vnumeric
#' @param vdate
#' @param vfactor
#' @param silently 
#' @return returns dataset with variables converted
#' 
#' @export
format_variables = function(dataset, vnumeric = c(), vdate = c(), vfactor = c(), silently = FALSE){
  idiap_numeric = c(.vars.numeric, vnumeric)
  idiap_date = c(.vars.dates, names(dataset)[substring(names(dataset), 1, 3) == "ep_"], vdate)
  idiap_factor = c(.vars.factors, vfactor)
  for(v in names(dataset)){
    if(v %in% idiap_numeric & !is.numeric(dataset[,v]) ){
      dataset[,v] = as.numeric(dataset[,v])
      message(paste(v, "converted to numeric"))
    }else if(v %in% idiap_date & class(dataset[,v]) != 'Date' ){
      dataset[,v] = as.Date(dataset[,v])
      message(paste(v, "converted to Date"))
    }else if(v %in% idiap_factor & !is.factor(dataset[,v])){
      dataset[,v] = as.factor(dataset[,v])
      message(paste(v, "converted to factor"))
    }else{
      if(!silently){
        cat(paste(v, ":", class(dataset[,v]), "\n"))
      }
    }
  }
  dataset
}