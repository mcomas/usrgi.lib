#   Adreça on tenim la llibreria
.root = function(text) paste0('/home/idiap/lib/R/usrgi_lib/', text)

#   Fitxer amb les funcions per manipular variables
source(.root('vars.R'))

#   Fitxer amb les funcions per descriure
source(.root('R/desc.R'))

#   Fitxer amb les funcions per als models
source(.root('R/mods.R'))

source(.root('R/lib-imp.R'))