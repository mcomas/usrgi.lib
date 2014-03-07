# Explicació:
#   Construïm la potencia de la estatina, segons la definició PETREA o binària
# Input:
#   - dose: variable amb la dosi d'e 'estatina.
#   - atc: variable amb el tipus d'estatina.
#   - definition: "PETREA" o "2-levels". Per defecte, "PETREA".
# Output:
#   - level: variable amb la potencia de l'estatina.
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

# Explicació:
#   Càlcul del Glomerular filtration rate. 
# Input:
#   - cre: creatinina
#   - age: edat
#   - sex: sexe
#   - race: raça. 0 = blanc, 1 = negre
# Output:
#   - Glomerular filtration rate (GFR).
ckd.epi = function(cre, age, sex, race = 0){
  # 0 WHITE
  # 1 BLACK
  k = ifelse(sex == 'H', 0.9, 0.7)
  a = ifelse(sex == 'H', -0.411, -0.329)
  eGFR =  141 * pmin(cre / k, 1)^a
  eGFR = eGFR * pmax(cre / k, 1)^(-1.209) 
  eGFR = eGFR * 0.993^age 
  p1 = ifelse(sex == 'D', 1.018, 1)
  p2 = ifelse(race == 1, 1.159, 1)
  return(p1 * p2 * eGFR)
}

# Explicació:
#   Càlcul del Estimated GFR (eGFR) using Modification of Diet in Renal Disease (MDRD) formula. 
# Input:
#   - cre: creatinina
#   - age: edat
#   - sex: sexe
#   - race: raça. 0 = blanc, 1 = negre
# Output:
#   - Estimated GFR (eGFR) using Modification of Diet in Renal Disease (MDRD) formula.
mdrd = function(cre, age, sex, race = 0){
  # 0 WHITE
  # 1 BLACK
  eGFR = 186 * cre^(-1.154) * age^(-0.203) 
  p1 = ifelse(sex == 'D', 0.742, 1)
  p2 = ifelse(race == 1, 1.212, 1)
  return(p1 * p2 * eGFR)
}

# Explicació:
#   Càlcul de l'estadi segons Estimated GFR (eGFR) using ckd epi. 
# Input:
#   - x: ckd.epi
# Output:
#   - Estadi de malaltia renal crònica.
estadis.mrc = function(x){
  res = rep(NA, length(x))
  res[!is.na(x) & x >= 90] = 1
  res[!is.na(x) & 90 > x & x >= 60] = 2
  res[!is.na(x) & 60 > x & x >= 45] = 3
  res[!is.na(x) & 45 > x & x >= 30] = 3.5 
  res[!is.na(x) & 30 > x & x >= 15] = 4
  res[!is.na(x) & 15 > x] = 5
  return(res)
}

# Explicació:
#   Albuminuria positiva
# Input:
#   - x: alb
# Output:
#   - Albuminuria positiva si o no
with.alb = function(x, alb.limit = 20){
  res = rep(NA, length(x))
  res[!is.na(x) & x > alb.limit] = 1
  res[!is.na(x) & x <= alb.limit] = 0
  return(res)
}
# Explicació:
#   Càlcul de la diferència estandaritzada.
# Input:
#   - x: variable que volem fer la diferència estandaritzada
#   - g: variable categòrica amb almenys dues categories
#   - type: tipus de diferència. "default": fa la diferència que toca. "numeric": dif estan numèrica. 
#           "proportion": dif estan per proporció.
# Output:
#   - vector de caràcters amb 5 camps:
#     + v1: nom ("mean (sd)" o "N (%)")
#     + v2: càlcul pel primer grup
#     + v3: càlcul pel segon grup
#     + v4: p-valor de la diferència
#     + v5: diferència estandaritzada
stand.diff = function(x, g, type='default'){
  tab = table(g)
  if(length(tab) != 2) stop("2 groups needed")
  X = split(x,g)
  if(type == 'default'){
    if(length(table(x)) > 2){
      type = 'numeric'
    }else{
      type = 'proportion'
    }
  }
  if(type=='numeric'){
    m.u = mean(X[["1"]], na.rm=TRUE)
    m.c = mean(X[["0"]], na.rm=TRUE)
    s.u = sd(X[["1"]], na.rm=TRUE)
    s.c = sd(X[["0"]], na.rm=TRUE)
    v1 = sprintf("%.20s", 'mean (SD)')
    v2 = sprintf('%7.2f (%6.3f)', m.u, s.u)
    v3 = sprintf('%7.2f (%6.3f)', m.c, s.c)
    mse = (s.u^2+s.c^2)/2
    N = length(na.omit(X[["1"]])) + length(na.omit(X[["1"]]))
    v4 = sprintf('%9.4f', 2- 2*pt( abs( (m.u-m.c)/(2*sqrt(mse/N)) ), N-1))
    v5 = sprintf('%9.4f', (m.u - m.c) / sqrt(0.5*s.u*s.u + 0.5*s.c*s.c))
  }
  if(type=='proportion'){
    v.u = table(X[["1"]])  
    v.c = table(X[["0"]])
    other = sort(names(v.u))[1]
    base = sort(names(v.u))[2]
    v1 = sprintf("%.20s, No (%%)", base)
    
    val = v.u[base]
    per = prop.table(v.u)[base]
    v2 = sprintf('%6.d (%6.2f%%)', val, 100*per)
    pu = per / 100
    
    val = v.c[base]
    per = prop.table(v.c)[base]
    v3 = sprintf('%6.d (%6.2f%%)', val, 100*per)
    pc = per / 100
    
    n.u = length(X[["1"]])
    n.c = length(X[["0"]])
    p = (pu*n.u + pc*n.c) / (n.u+n.c)
    se = sqrt(p * (1-p) * (1/n.u + 1/n.c))
    v4 = sprintf('%9.4f', 2 - 2*pnorm( abs((pu-pc)/se) ))
    
    v5 = sprintf('%9.4f', (pu - pc) / sqrt(0.5*pu*(1-pu) + 0.5*pc*(1-pc)))
  }
  return( c(v1,v2,v3,v4,v5))
}

# Explicació:
#   Funció per categoritzar una variable en K-quantiles
# Input:
#   - X: variable a categoritzar
#   - K: nombre de categoríes
# Output:
#   - variable en K categories
xquantile = function(X, K){
  qt = unique(quantile(X, probs=seq(0,1,length.out=K+1), na.rm=TRUE))
  return( cut(X, qt, include.lowest=TRUE) )
}

# Explicació:
#   Creació de la funció de risc REGICOR.
# Input:
#   - age: variable edat.
#   - sex: variable sexe.
#   - smoker: variable fumador.
#   - diabetes: variable diabetes.
#   - coltot: variable colesterol total.
#   - colhdl: variable colesterol HDL.
#   - colldl: variable colesterol LDL.
#   - tas: variable tensió arterial sistòlica.
#   - tad: variable tensió arterial diastòlica.
#   - divide: número a dividar. Per defecte, 1.
# Output:
#   - variable REGICOR.
regicor = function(age, sex, smoker, diabetes, coltot, colhdl, tas, tad, divide = 1){
  smoker = as.numeric(as.character(smoker))
  diabetes = as.numeric(as.character(diabetes))
  bp_opti = ifelse( tas <  120 & tad < 80 , 1, 0 )
  bp_high = ifelse( (130 <= tas & tas < 140) | (85 <= tad & tad < 90), 1, 0 )
  bp_i = ifelse( (140 <= tas & tas < 160) | (90 <= tad & tad < 100), 1, 0 )
  bp_ii = ifelse( 160 <= tas | 100 <= tad, 1, 0 )
  
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
  men = (sex == 'H')
  l_chol[men] = ( (0.04826*age)- (0.65945* c_160) + 
                    (0.17692* c200_239) +(0.50539* c240_279) + (0.65713* c280_) + 
                    (0.49744* h_35) + (0.24310* h35_44) - (0.05107* h50_59) - 
                    (0.48660*h60_) - (0.00226 * bp_opti) + (0.28320 *  bp_high) + 
                    (0.52168 * bp_i) + (0.61859 * bp_ii) + (0.42839 * diabetes) + (0.52337* smoker) )[men]
  g_chol[men] =  3.489
  
  women = (sex == 'D')
  l_chol[women] = ( (0.33766*age)  - (0.00268 * age^2)- (0.26138* c_160) + 
                      (0.20771* c200_239) + (0.24385 * c240_279) + (0.53513* c280_) + 
                      (0.84312* h_35) + (0.377096* h35_44) + (0.19785* h45_49) - 
                      (0.42951*h60_)- (0.53363 * bp_opti) - (0.06773 *  bp_high) + 
                      (0.26288 * bp_i) + (0.46573 * bp_ii) + (0.59626 * diabetes) + (0.29246* smoker))[women]
  g_chol[women] = 10.279
  
  b_chol = exp(l_chol - g_chol)
  
  result = rep(0,n)
  result[men] = (1- (1-(1-0.951)/divide)^b_chol[men]) * 100 
  result[women] = (1-(1-(1-0.978)/divide)^b_chol[women]) * 100
  result
}

# Explicació:
#   Funció que categoritza la variable REGICOR
# Input:
#   - regicor: varaible REGICOR
#   - breaks: punts de tall
# Output:
#   - variable REGICOR categortizada
groups_risc = function(regicor, breaks = c(0,5,7.5,10,100)){
  cut(regicor, breaks=breaks, include.lowest=T)
}
# Explicació:
#   Funció que categoritza la variable REGICOR segons els talls: [0,5), [5,7.5), [7.5,10) i >10
# Input:
#   - regicor: varaible REGICOR
# Output:
#   - variable REGICOR categortizada
groups_risc2 = function(regicor){
  x = rep(NA, length(regicor))
  x[!is.na(regicor) & regicor < 5]  = '[0, 5)'     
  x[!is.na(regicor) & regicor >= 5] = '[5, 7.5)'
  x[!is.na(regicor) & regicor >= 7.5] = '[7.5, 10)'
  x[!is.na(regicor) & regicor >= 10] = '[10+)'
  factor(x, levels=c('[0, 5)', '[5, 7.5)', '[7.5, 10)', '[10+)'))
}
bp_cat = function(tad, tas){
  bp = rep('normal', length(tad))
  bp[tas <  120 & tad < 80] = 'opti'
  bp[(130 <= tas & tas < 140) | (85 <= tad & tad < 90)] = 'high'
  bp[(140 <= tas & tas < 160) | (90 <= tad & tad < 100)] = 'i'
  bp[160 <= tas | 100 <= tad] = 'ii'
  factor(bp, levels = c('normal', 'opti', 'high', 'i', 'ii'))
}

# Explicació:
#   Funció que categoritza la variable COLTOT segons els talls: 
#   [160,200)", "[0,160)", "[200,240)", "[240,280)", "[280,Inf)
# Input:
#   - coltot: colesterol TOTAL
# Output:
#   - variable COLTOT categortizada segons funció REGICOR
coltot_cat = function(coltot){
  x = cut(coltot, breaks=c(0, 160, 200, 240, 280, Inf), right=F)
  factor(x, levels=c("[160,200)", "[0,160)", "[200,240)", "[240,280)", "[280,Inf)"))
}

# Explicació:
#   Funció que categoritza la variable HDL segons els talls: 
#   "[45,50)", "[0,35)", "[35,45)", "[50,60)", "[60,Inf)"
# Input:
#   - colhdl: High colesterol
# Output:
#   - variable HDL categortizada segons funció REGICOR
colhdl_cat = function(colhdl){
  x = cut(colhdl, breaks=c(0, 35, 45, 50, 60, Inf), right=F)
  factor(x, levels=c("[45,50)", "[0,35)", "[35,45)", "[50,60)", "[60,Inf)"))
}

# Explicació:
#   Funció que categoritza la variable COLHDL segons els talls: [0,5), [5,7.5), [7.5,10) i >10
# Input:
#   - regicor: varaible REGICOR
# Output:
#   - variable REGICOR categortizada