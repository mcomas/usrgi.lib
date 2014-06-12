#   Adreça on tenim la llibreria
.root = function(text) paste0('/home/idiap/lib/R/usrgi_lib/', text)

#   Fitxer amb les funcions per manipular variables
source(.root('vars.R'))

#   Fitxer amb les funcions per descriure
source(.root('desc.R'))

#   Fitxer amb les funcions per als models
source(.root('mods.R'))

source(.root('lib-imp.R'))