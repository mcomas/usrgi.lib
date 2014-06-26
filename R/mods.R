time.setting = function(d, ep='ep_ami', minDate = as.Date("2006-07-01"), add.age = FALSE){
  if(add.age){
    d$T0 = as.integer(d$age * 365.25)
    d$T1 = d[,paste0('t.', ep)] + d$T0
    d$timevar = d$user
    d$event = d[, paste0('i.', ep)]  
    d
  }else{
    d$T0 = as.numeric(d$dintro - minDate)
    d$T1 = d[,paste0('t.', ep)] + d$T0
    d$timevar = d$user
    d$event = d[, paste0('i.', ep)]  
    d  
  }
}

idiap.time_depending = function(t.event, event, t.exp, exp, t.intro = 0, data = NULL){
  d = data.frame(id_ = 1:length(t.event), t.intro_ = t.intro, t.event_ = t.event, orig.event_ = event, t.exp_ = t.exp, exp_ = exp)
  
  if(!is.null(data)){
    d = cbind(data, d)
  }
  d$T0 = with(d, ifelse(exp_ == 1, t.exp_ - t.intro_, t.intro_))
  d$T1 = d$t.event_
  d$timevar = d$exp_
  d$event = d$orig.event_
  
  d.bias = d[d$exp_ == 1, ]
  
  d.bias$T0 = d.bias$t.intro_
  d.bias$T1 = with(d.bias, t.exp_ - t.intro_)
  d.bias$timevar = 0
  d.bias$event = 0
  
  d.all = rbind(d, d.bias)
  ordre <- order(d.all$id_, d.all$T0)
  d.all[ordre,]  
}

idiap.time_depending2 = function(d, ep='ep_ami', i.exp = 'user', d.exp = 'dintro', 
                                 intro = 'dintro', origin = as.Date("2006-07-01")){
  d$origin = origin
  d$d0 = d[,intro]
  usr = d[,i.exp] == 1
  d[usr, 'd0'] = d[usr, d.exp]
  
  d$T0 = as.numeric(d$d0 - d$origin)
  d$T1 = as.numeric( (d[,paste0('t.', ep)] + d[,intro]) - d$origin)
  
  d$timevar = d$user
  d$event = d[, paste0('i.', ep)]  
  
  
  d.bias = d[usr, ]
  
  
  d.bias$T0 = as.numeric(d.bias[,intro] - d.bias$origin)
  d.bias$T1 = as.numeric(d.bias[,d.exp] - d.bias$origin)
  d.bias$timevar = 0
  d.bias$event = 0
  
  d.all = rbind(d, d.bias)
  ordre <- order(d.all$ocip, d.all$T0)
  d.all[ordre,]   
}

time.depending = function(d, ep='ep_ami', minDate = as.Date("2006-07-01") , control.time = NULL, add.age = FALSE){
  if(add.age){
    d$minIntro = minDate
    d$T0 = as.integer(d$age * 365.25)
    d$T1 = d[,paste0('t.', ep)] + d$T0
    
    d$timevar = d$user
    d$event = d[, paste0('i.', ep)]  
    
    
    d.bias = d[d$user == 1, ]
    
    intro = d.bias$T0
    
    d.bias$T0 = as.integer(d.bias$age * 365.25) - as.numeric(d.bias$dintro - d.bias$minDate)
    d.bias$T1 = intro
    d.bias$timevar = 0
    d.bias$event = 0
    
    d.all = rbind(d, d.bias)
    ordre <- order(d.all$ocip, d.all$T0, d.all$T1)
    d.all[ordre,]
  }else{
    d$T0 = as.numeric(d$dintro - minDate)
    d$T1 = d[,paste0('t.', ep)] + d$T0
    if(!is.null(control.time)){
      d$T0[d$user == 0] = control.time
    }
    d$timevar = d$user
    d$event = d[, paste0('i.', ep)]  
    
    
    d.bias = d[d$user == 1, ]
    
    intro = d.bias$T0
    
    d.bias$T0 = 0
    d.bias$T1 = intro
    d.bias$timevar = 0
    d.bias$event = 0
    
    d.all = rbind(d, d.bias)
    ordre <- order(d.all$ocip, d.all$T0, d.all$T1)
    d.all[ordre,]     
  }
  
}

time.depending2 = function(data.match1, ep='ep_ami', minDate = as.Date("2006-07-01") ){
  data.match1$entry <- data.match1$dintro
  data.match1$entry[data.match1$user == 1] <- minDate
  data.match1$ttoexposed <- as.double(difftime(data.match1$dintro, data.match1$entry,units="days"))/365.25
  data.match1$ttoexposed[(data.match1$user == 0) | (data.match1$ttoexposed < 0)] <- NA
  data.match1$toa <- ifelse(data.match1[,paste0('i.', ep)] == 1,
                            as.double(difftime(data.match1[,ep], data.match1$entry,
                                               units="days"))/365.25,
                            as.double(difftime(data.match1$dexitus, data.match1$entry,
                                               units="days"))/365.25) 
  #BBDD Counting process
  #Definim en la bbdd completa T0 i T1:
  #No usuari: T0 = 0 i T1 = toa
  #Usuari: T0 = 0 i T1 = ttoexposed
  data.match1$T0 <- 0
  data.match1$T1 <- with(data.match1,ifelse(user == 0,toa,ttoexposed))
  data.match1$timevar <- 0
  data.match1$event <- ifelse(data.match1$user == 0, data.match1[,paste0('i.', ep)] == 1,0)
  
  #Pel counting process, dupliquem els usuaris i redefinim el T0 i T1
  #T0 = ttoexposed i T1 = toa
  data.match1.sub <- subset(data.match1,data.match1$user == 1)
  data.match1.sub$T0 <- data.match1.sub$T1
  data.match1.sub$T1 <- data.match1.sub$toa
  data.match1.sub$timevar <- 1
  data.match1.sub$event <- data.match1.sub[,paste0('i.', ep)]
  
  #Ho ajuntem
  data.surv <- rbind(data.match1,data.match1.sub)
  ordre <- order(data.surv$ocip,data.surv$T0)
  
  data.surv[ordre,] 
}