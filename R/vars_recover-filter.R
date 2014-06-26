### FILTRES

# Explicació:
#   Filtra la variable tall, per a que tots els valors estiguin entre dos límits.
# Input:
#   - talla: variable on tenim la talla.
#   - inf: límit inferior.
#   - sup: límit superior.
# Output:
#   - talla: variable talla filtrada.
filter.talla = function(talla, inf = 140, sup = 220){
  valid = (inf <= talla & talla <= sup)
  talla[!valid] = NA
  talla
}

# Explicació:
#   Filtra la variable pes, per a que tots els valors estiguin per sobre d'un límit inferior.
# Input:
#   - pes: variable on tenim la pes.
#   - inf: límit inferior.
# Output:
#   - pes: variable pes filtrada.
filter.pes = function(pes, inf = 40){
  valid = (inf <= pes)
  pes[!valid] = NA
  pes
}

# Explicació:
#   Filtra la variable IMC, per a que tots els valors estiguin entre dos límits, exceptuant les persones que 
#   siguin obeses que no tenen límit superior.
# Input:
#   - imc: variable on tenim la IMC.
#   - obesity: variable indicadora on tenim recollida la informació sobre la obesitat.
#              Per defecte, NULL.
#   - inf: límit inferior.
#   - sup: límit superior.
# Output:
#   - imc: variable imc filtrada.
filter.imc = function(imc, obesity = NULL, inf = 14, sup = 40){
  valid = (inf <= imc & imc <= sup) | ifelse(is.null(obesity), FALSE, (imc > sup & obesity == 1))
  imc[!valid] = NA
  imc
}

# Explicació:
#   Filtra les dues variables sobre la tensió arterial (tas i tad), 
#   per a que tots els valors estiguin entre dos límits.
# Input:
#   - tas: variable on tenim la tas.
#   - tad: variable on tenim la tas.
#   - inf.tas: límit inferior de tas.
#   - sup.tas: límit superior de tas.
#   - inf.tad: límit inferior de tad.
#   - sup.tad: límit superior de tad.
# Output:
#   - tas: variable tas filtrada.
#   - tad: variable tad filtrada.
filter.bp = function(tas, tad, inf.tas = 60, inf.tad = 30, sup.tas = 250, sup.tad = 170){
  valid = sup.tas >= tas & tas >= inf.tas
  tas[!valid] = NA
  valid = sup.tad >= tad & tad >= inf.tad
  tad[!valid] = NA
  no.valid = !is.na(tas) & !is.na(tad) & tas <= tad
  tad[no.valid] = NA
  cbind(tas, tad)
}

# Explicació:
#   Filtra les variables sobre el colesterol (coltot, colhdl, colldl i tg), 
#   per a que tots els valors estiguin entre dos límits.
# Input:
#   - coltot: variable on tenim la coltot.
#   - colhdl: variable on tenim la colhdl.
#   - colldl: variable on tenim la colldl.
#   - tg: variable on tenim la tg.
#   - inf.tot: límit inferior de coltot.
#   - sup.tot: límit superior de coltot.
#   - inf.hdl: límit inferior de colhdl.
#   - sup.hdl: límit superior de colhdl.
#   - inf.ldl: límit inferior de colldl.
#   - sup.ldl: límit superior de colldl.
#   - inf.tg: límit inferior de tg.
#   - sup.tg: límit superior de tg.
# Output:
#   - coltot: variable coltot filtrada.
#   - colhdl: variable colhdl filtrada.
#   - colldl: variable colldl filtrada.
#   - tg: variable tg filtrada.
filter.colesterol = function(coltot, colhdl, colldl, tg, 
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

# Explicació:
#   Recuperem IMC a partir de la informació de la talla i el pes.
# Input:
#   - imc: variable IMC original.
#   - talla: variable talla.
#   - pes: variable pes.
# Output:
#   - imc: IMC recuperat.
recover.imc = function(imc, talla, pes){
  select = is.na(imc) & !is.na(talla) & !is.na(pes) & talla != 0
  if(length(select) != 0)
    imc[select] = pes[select] / (talla[select]/100)^2
  imc
}
recover.obesity = function(obesity, imc){
  as.numeric(obesity == 1 | (!is.na(imc) & imc > 30))
}

# Explicació:
#   Recuperem la informació de les variables de tabac a partir d'informació sobre tabac.
# Input:
#   - smoking: tabac per icd (problemes ecap o cmbd).
#   - tabac : variables ecap.
#   - t_tabac : temp fins a mesura variable.
#   - non_smoker_time : temps que fa que no fuma.
# Output:
#   - new_smoking: variable smoking resum.
recover.smoking = function(smoking, tabac, t_tabac, non_smoker_time = 365, na.value = 0){
  new_smoking = smoking
  sel1 = ( !is.na(tabac) & (tabac == 1) )
  sel2_smoker = ( !is.na(tabac) & (tabac == 2) & (t_tabac < non_smoker_time) )
  sel2_non_smoker = ( !is.na(tabac) & (tabac == 2) & (t_tabac >= non_smoker_time) )
  new_smoking[sel1] = 1
  new_smoking[sel2_smoker] = 1
  new_smoking[sel2_non_smoker] = 0
  new_smoking[new_smoking == 0 & is.na(tabac)] = na.value
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
recover.colesterol = function(coltot, colhdl, colldl, tg, filter = TRUE){  
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
    filter.colesterol(coltot, colhdl, colldl, tg)
  }else{
    data.frame(coltot = coltot, colhdl = colhdl, colldl = colldl, tg = tg)
  }
}

# Explicació:
#   Recuperem la informació de les variables de alcohol
# Input:
#   - alcoholism (problema ecap o cmbd).
#   - alcohol 0: no beu, 1: beu, 2: bevedor de risc.
#   - deafult: valor que posem per defecte, nivells de la variable alcohol. Per defecte, 2.
# Output:
#   - alcoholism_new: variable alcohol resum.
recover.alcoholism = function(alcoholism, alcohol, default = 2, na.value = 0){
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
recover.using_medication = function(problem, meds){
  as.numeric(problem == 1 | apply( as.data.frame(meds), 1, sum) > 0 )
}

# Explicació:
#   Recuperem informació agregant segons la variable agg i aplicant la funció funct
# Input:
#   - x: variable que volem recuperar.
#   - agg: grup que s'utilitzarà per recuperar x
#   - funct: funció que s'aplica a cada grup. El resultat s'utilitzarà per recuperar x
# Output:
#   - variable recuperada.
impute_by_aggregation = function(x, agg, funct, ...){
  X = aggregate(as.matrix(x)~agg, FUN=funct, ...)
  x.agg = X[,2]
  names(x.agg) = X[,1]  
  x.na = is.na(x)
  x[x.na] = x.agg[agg[x.na]]
  x[is.na(x)] = mean(x, na.rm=TRUE)
  return(x)
}