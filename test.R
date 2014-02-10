data = read.csv("/home/idiap/projects.conan.previous/petrea/data/petrea.csv", sep=";", stringsAsFactors = F)

source('/home/idiap/lib/R/usrgi_lib/main.R')
data$ep_death = with(data, create.ep_death(exitus, dexitus))
with(data, table(is.na(ep_death), exitus), useNA='ifany')

with(data, table(create.statine_level(dose, atc), 
                 create.statine_level(dose, atc, definition='2-levels'),
                 useNA='ifany'))

EPS = data[,c('ep_ami', 'ep_card_proc', 'ep_coronari_i_death')]
table('comb' = !is.na(create.ep_combinate(EPS)),
      'union' = apply(EPS, 1, function(x){
        sum(!is.na(x)) > 0
      }))

imc = filter.imc(data$imc)

smoking = with(data, recover.smoking(smoking, tabac, as.numeric(as.Date(dintro)-as.Date(dtabac)) ))
