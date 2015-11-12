### FILTRES

#' Filter a variable eliminating measurements below and above a constant inf and sup respectively
#' 
#' @param value vector with measurements to be filtered
#' @param inf lower bound
#' @param sup upper bound
#' @return the measurement vector filtered with NA in measurements outside the rank [ind, sup]
#' 
#' @export
filter.variable = function(value, inf, sup){
  valid = (inf <= value & value <= sup)
  value[!valid] = NA
  value
}


#' Filter height variable eliminating measurements below and above a constant inf and sup respectively
#' 
#' @param value vector with height measurements to be filtered
#' @param inf lower bound (default 140)
#' @param sup upper bound (default 220)
#' @return the measurement vector filtered with NA in measurements outside the rank [ind, sup]
#' 
#' @export
filter.height = function(height, inf = 140, sup = 220){
  filter.variable(height, inf, sup)
}

#' Filter weight variable eliminating measurements below and above a constant inf and sup respectively
#' 
#' @param value vector with weight measurements to be filtered
#' @param inf lower bound (default 140)
#' @param sup upper bound (default 220)
#' @return the measurement vector filtered with NA in measurements outside the rank [ind, sup]
#' 
#' @export
filter.weight = function(weight, inf = 40, sup=Inf){
  filter.variable(weight, inf, sup)
}

#' Filter bmi variable eliminating measurements below and above a constant inf and sup respectively
#' 
#' @param value vector with bmi measurements to be filtered
#' @param obesity indicative variable of obesity
#' @param inf lower bound (default 140)
#' @param sup upper bound (default 220)
#' @return the measurement vector filtered with NA in measurements outside the rank [ind, sup]
#' 
#' @export
filter.bmi = function(bmi, obesity = NULL, inf = 14, sup = 40){
  valid = (inf <= bmi & bmi <= sup) | ifelse(is.null(obesity), FALSE, (bmi > sup & obesity == 1))
  bmi[!valid] = NA
  bmi
}


#' Filter bp variable eliminating measurements below and above a constant inf and sup respectively and with coherent measurement
#' between sbp and dbp
#' 
#' @param sbp vector with bmi measurements to be filtered
#' @param dbp indicative variable of obesity
#' @param sbp.inf lower bound (default 60)
#' @param dbp.inf lower bound (default 30)
#' @param sbp.sup upper bound (default 250)
#' @param dbp.sup upper bound (default 170)
#' @return the measurement vector filtered with NA in measurements outside the rank [ind, sup]
#' 
#' @export
filter.bp = function(sbp, dbp, inf.sbp = 60, inf.dbp = 30, sup.sbp = 250, sup.dbp = 170){
  valid = sup.sbp >= sbp & sbp >= inf.sbp
  sbp[!valid] = NA
  valid = sup.dbp >= dbp & dbp >= inf.dbp
  dbp[!valid] = NA
  no.valid = !is.na(sbp) & !is.na(dbp) & sbp <= dbp
  dbp[no.valid] = NA
  cbind(sbp, dbp)
}


#' Filter liidic variables (coltot, colhdl, colldl, tg)
#' 
#' @param coltot coltot variable
#' @param colhdl colhdl variable
#' @param colldl colldl variable
#' @param tg tg variable
#' @param inf.tot coltot inferior limit
#' @param sup.tot coltot superior limit
#' @param inf.hdl colhdl inferior limit
#' @param sup.hdl colhdl superior limit
#' @param inf.ldl colldl inferior limit
#' @param sup.ldl colldl superior limit
#' @param inf.tg tg inferior limit
#' @param sup.tg tg superior limit
#' @return coltot,colhdl,colldl and tg filtered
#' 
#' @export
filter.cholesterol = function(coltot, colhdl, colldl, tg, 
                             inf.tot = 90, sup.tot = 450, inf.hdl = 25, sup.hdl = 150, 
                             inf.ldl = 50, sup.ldl = 295, inf.tg = 25, sup.tg = 2000){
  non.valid = !is.na(coltot) & ( coltot < inf.tot | coltot > sup.tot)
  coltot[non.valid] = NA
  non.valid = !is.na(colhdl) & ( colhdl < inf.hdl | colhdl > sup.hdl)
  colhdl[non.valid] = NA
  non.valid = !is.na(colldl) & ( colldl < inf.ldl | colldl > sup.ldl)
  colldl[non.valid] = NA
  non.valid = !is.na(tg) & ( tg < inf.tg | tg > sup.tg)
  tg[non.valid] = NA
  data.frame(coltot = coltot, colhdl = colhdl, colldl = colldl, tg = tg)
}

### RECOVERS

#' Recover bmi variable from height and weight
#' 
#' @param bmi vector with bmi measurements to be filtered
#' @param height height in cm
#' @param weight weight in kg
#' @return bmi with recovered measurements
#' 
#' @export
recover.bmi = function(bmi, height, weight){
  select = is.na(bmi) & !is.na(height) & !is.na(weight) & height != 0
  if(length(select) != 0)
    bmi[select] = weight[select] / (height[select]/100)^2
  bmi
}

#' Recover obesity from BMI calculation
#' 
#' @param obesity original indicator variable for obesity condition
#' @param bmi vector with bmi measurements
#' @return recovered indicator variable for obesity condition
#' 
#' @export
recover.obesity = function(obesity, imc){
  as.numeric(obesity == 1 | (!is.na(imc) & imc > 30))
}


#' Recover smoking
#' 
#' @param smoking original indicator variable for smoking condition
#' @param smoke smoking condition (0: never, 1: smoker, 2: previous smoker)
#' @param t_smoke days since  smoking condition
#' @param non_smoker_time days since a person is considered non-smoker (default: 365)
#' @param na.value value to be assigned to people with missing in smoke variable
#' @return recovered indicator variable for smoking condition
#' 
#' @export
recover.smoking = function(smoking, smoke, t_smoke, non_smoker_time = 365, na.value = 0){
  new_smoking = smoking
  sel1 = ( !is.na(smoke) & (smoke == 1) )
  sel2_smoker = ( !is.na(smoke) & (smoke == 2) & (t_smoke < non_smoker_time) )
  sel2_non_smoker = ( !is.na(smoke) & (smoke == 2) & (t_smoke >= non_smoker_time) )
  new_smoking[sel1] = 1
  new_smoking[sel2_smoker] = 1
  new_smoking[sel2_non_smoker] = 0
  new_smoking[new_smoking == 0 & is.na(smoke)] = na.value
  new_smoking
}

# Explicació:
#   Recuperem la informació de les variables de colesterol.
#   Quan només tenim missing en una de les 4 varaibles, la recuperem.
# Input:
#   - coltot: colesterol total.
#   - colhdl: colesterol HDL.
#   - colldl: colesterol LDL.
#   - tg: triglicèrids..
#   - filter: després de recuperar tornem a fer el filtre. Per defecte, TRUE.
# Output:
#   - matriu amb els colesterols.

#' Recover cholesterols
#' 
#' @param coltot
#' @param colhdl
#' @param colldl 
#' @param tg 
#' @param filter apply the filter after recovering (default TRUE)
#' @return recovered cholesterols
#' 
#' @export
recover.cholesterol = function(coltot, colhdl, colldl, tg, filter = TRUE){  
  tot = as.numeric(is.na(coltot))
  hdl = as.numeric(is.na(colhdl))
  ldl = as.numeric(is.na(colldl))
  tgb = as.numeric(is.na(tg))
  
  all = tot + hdl + ldl + tgb
  recov = all == 1
  
  tot = is.na(coltot) & recov
  hdl = is.na(colhdl) & recov
  ldl = is.na(colldl) & recov
  tgb = is.na(tg) & recov
  
  coltot[tot] = colldl[tot] + tg[tot]/5 + colhdl[tot]
  colhdl[hdl] = coltot[hdl] - tg[hdl]/5 - colldl[hdl]
  colldl[ldl] = coltot[ldl] - tg[ldl]/5 - colhdl[ldl]
  tg[tgb] = 5*(coltot[tgb] - colldl[tgb] - colhdl[tgb])
  if(filter){
    message('Applying filters to cholesterols')
    filter.cholesterol(coltot, colhdl, colldl, tg)
  }else{
    data.frame(coltot = coltot, colhdl = colhdl, colldl = colldl, tg = tg)
  }
}


#' Recover alcoholism
#' 
#' @param alcoholism original indicator variable for smoking condition
#' @param alcohol alcohol condition (0: no drinker, 1: low risk drinker, 2: high risk drinker)
#' @param na.value value to be assigned to people with missing in alcoholism variable
#' @return recovered indicator variable for alcoholism condition from high risk drinker
#' 
#' @export
recover.alcoholism = function(alcoholism, alcohol, na.value = 0){
  alcoholism_new = alcoholism
  alcoholism_new[!is.na(alcohol) & alcohol == 2] = 1
  alcoholism_new[alcoholism_new == 0 & is.na(alcohol)] = na.value
  alcoholism_new
}

# Explicació:
#   Recuperem inforamció segons si pren un tipus de medicació.
# Input:
#   - problem: problema que volem recuperar.
#   - meds: medicaments relacionats amb el problema.
# Output:
#   - variable recuperada.

#' Recover alcoholism
#' 
#' @param problem indicator variable for a given problem
#' @param meds matrix with indicator facturation associated to the problem
#' @return recovered indicator variable for a given problem
#' 
#' @export
recover.using_medication = function(problem, meds){
  as.numeric(problem == 1 | apply( as.data.frame(meds), 1, sum) > 0 )
}