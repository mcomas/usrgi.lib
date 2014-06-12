#El fitxer l'hem dividit en diferents grups:

#   Les funcions relacionades amb els end-points.
source(.root('vars_endpoints.R'))
#   Les funcions per a recuperar variables i filtres.
source(.root('vars_recover-filter.R'))
#   Les funcions que creen variables
source(.root('vars_create.R'))
       
### Funcions generals
# Explicació:
#   Funció auxiliar de format.variables que llegeix un vector des d'un fitxer.
# Input:
#   - file: nom del fitxer a llegir
# Output:
#   - vector de caràcters
read.vector = function(file){
  v <- read.table(file)
  v <- as.character(v[,1])
  v
}
# Funcions auxiliars per a carregar els diferents conjunts de nom segons el tipus de variable que és: 
# numèric, factor o Data.
idiap.numeric = function(){
  read.vector('/home/idiap/lib/R/vars.numeric')
}
idiap.factor = function(){
  read.vector('/home/idiap/lib/R/vars.factor')
}
idiap.Date = function(){
  read.vector('/home/idiap/lib/R/vars.dates')
}

# Explicació:
#   Posem el format de les variables.
# Input: 
#   - dataset: matriu de dades original
# Output:
#   - matriu de dades amb format
format.variables = function(dataset, vnumeric = c(), vdate = c(), vfactor = c(), silently=FALSE){
  idiap_numeric = c(idiap.numeric(), vnumeric)
  idiap_date = c(idiap.Date(), names(dataset)[substring(names(dataset), 1, 3) == "ep_"], vdate)
  idiap_factor = c(idiap.factor(), vfactor)
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