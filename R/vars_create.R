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