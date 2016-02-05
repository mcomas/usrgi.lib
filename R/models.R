#' ifelse when used with Dates
#' 
#' @param mi_data multiple imputation data with each imputation in stacked by rows
#' @param str_coxph string containing the cox models to be evaluated (default="coxph(Surv(time = time, event = event)~1")
#' @param analysis type of analysis to perform 'intention.to.treat', 'per.protocol' and 'as.treated' (default: 'intention.to.treat')
#' @param str_survfit string containing the survival curve to be evaluated
#' @return result of fitting the model
#' 
#' @export
fit_cox.mi = function(mi_data, str_coxph = "coxph(Surv(time = time, event = event)~1", analysis = 'intention.to.treat',
                      str_survfit = gsub('user', 'strata(user)', str_coxph)){
  if(analysis == 'as.treated'){
    mi_data = as_treated_df(mi_data)
  }
  if(analysis == 'per.protocol'){
    mi_data = per_protocol_df(mi_data)
  }
  nimp = length(unique(mi_data$imp))
  l_df = split(mi_data, mi_data$imp)
  df_events = mi_data %>% dplyr::group_by(exposure) %>% dplyr::summarise(n = sum(event))
  if( df_events %>% select(n) %>% min > 0 ){
    suppressWarnings(
      mods.cox <- lapply(l_df, function(.d){
        with(.d,  eval(parse(text = str_coxph)))
      })
    )
    suppressWarnings(
      mod <- summary(pool(as.mira(mods.cox)))
    )
    suppressWarnings(
      mods.strata <- lapply(l_df, function(.d){
        with(.d,  eval(parse(text = str_survfit)))
      })
    )
    df.risk.cox = lapply(mods.cox, function(MOD){
      R_t = basehaz(MOD, centered=TRUE)
      R_y = with(R_t, approx(time, hazard, xout = 365))$y
      S_y = (1-R_y)
      data_frame('user' = 1-S_y^exp( (1-MOD$means[['user']]) * MOD$coefficients[['user']]),
                 'control' = 1-S_y^exp( (0-MOD$means[['user']]) * MOD$coefficients[['user']]))
    }) %>% bind_rows %>% summarise(
      'user' = mean(user),
      'control' = mean(control) )
    df.risk = lapply(mods.strata, function(MOD){
      pred = summary(survfit(MOD), 365)
      data_frame(
        strata = pred$strata,
        risk = 1-pred$surv,
        risk.lo = 1-pred$lower,
        risk.hi = 1-pred$upper)
    }) %>% bind_rows %>% dplyr::group_by(strata) %>%
      dplyr::summarise(
        risk = mean(risk),
        risk.lo = mean(risk.lo),
        risk.hi = mean(risk.hi) )
    nnt.cox = 1/(df.risk.cox$control - df.risk.cox$user)
    nnt = 1/(df.risk %>% subset(strata == 'user=0') %>% unlist - 
               df.risk %>% subset(strata == 'user=1') %>% unlist)[-1]
    suppressWarnings(
      inc_user <- summary(pool(as.mira(lapply(l_df, with, incidence(event = event[user==1], time = time[user==1])))))
    )
    suppressWarnings(
      inc_cntr <- summary(pool(as.mira(lapply(l_df, with, incidence(event = event[user==0], time = time[user==0])))))
    )
  }else{
    mod = matrix(rep(NA, 3), nrow=1, dimnames = list('user', c('est', 'lo 95', 'hi 95')))
    nnt.cox = NA
    nnt = c('risk'=NA, 'risk.lo'=NA, 'risk.hi'=NA)
    if( df_events %>% filter(exposure == 'user') %>% select(n) %>% min > 0 ){
      suppressWarnings(
        inc_user <- summary(pool(as.mira(lapply(l_df, with, incidence(event = event[user==1], time = time[user==0])))))
      )
    }else{
      inc_user = matrix(rep(0,3), ncol=3, dimnames = list('user', c('est', 'lo 95', 'hi 95')))
    }
    if( df_events %>% filter(exposure == 'control') %>% select(n) %>% min > 0 ){
      suppressWarnings(
        inc_cntr <- summary(pool(as.mira(lapply(l_df, with, incidence(event = event[user==0], time = time[user==0])))))
      )
    }else{
      inc_cntr = matrix(rep(0,3), ncol=3, dimnames = list('control', c('est', 'lo 95', 'hi 95')))
    }
  }
  with(mi_data,
       data_frame('ctl.n' = sum(user==0)/nimp,
                  'c.ev' = sum(event[user==0])/nimp,
                  'c.inc' = inc_cntr[1, 'est'],
                  'c.lo' = inc_cntr[1, 'lo 95'],
                  'c.hi' = inc_cntr[1, 'hi 95'],
                  'usr.n' = sum(user==1)/nimp,
                  'u.ev' = sum(event[user==1])/nimp,
                  'u.inc' = inc_user[1, 'est'],
                  'u.lo' = inc_user[1, 'lo 95'],
                  'u.hi' = inc_user[1, 'hi 95'],
                  'haz' = exp(mod['user', 'est']), 
                  'h.lo' = exp(mod['user', 'lo 95']),  
                  'h.hi' = exp(mod['user', 'hi 95']),
                  'nnt.cox' = nnt.cox,
                  'nnt' = nnt['risk']) )
}

as_treated_df = function(.data0){
  .data = bind_rows(
    # 'user' exposed period
    .data0 %>%
      subset(exposure == 'user') %>%
      mutate(
        time.stat.end = as.numeric(stat.end - dintro) + end_stat * 30,
        censored = !is.na(time.stat.end) & time.stat.end < time,
        event = ifelse(censored, 0, event),
        time = ifelse(censored, time.stat.end, time) ),
    # 'user' unexposed period
    .data0 %>%
      subset(exposure == 'user') %>%
      mutate(
        time.stat.end = as.numeric(stat.end - dintro) + end_stat * 30,
        censored = !is.na(time.stat.end) & time.stat.end < time,
        event = event,
        time = time - time.stat.end ) %>%
      subset(censored),
    # 'control' unexposed period
    .data0 %>% 
      subset(exposure == 'control') %>%
      mutate(
        time.beg = as.numeric(stat.beg.all - dintro) + beg_stat * 30,
        censored = !is.na(time.beg) & time.beg < time,
        event = ifelse(censored, 0, event),
        time = ifelse(censored, time.beg, time) ),
    # 'control' exposed period
    .data0 %>% 
      subset(exposure == 'control') %>%
      mutate(
        time.beg = as.numeric(stat.beg.all - dintro) + beg_stat * 30,
        censored = !is.na(time.beg) & time.beg < time,
        event = event,
        time = time - time.beg ) %>%
      subset(censored) )
}

per_protocol_df = function(.data0){
  .data = bind_rows(
    .data0 %>% 
      subset(exposure == 'user') %>%
      mutate(
        time.stat.end = as.numeric(stat.end - dintro) + end_stat * 30,
        censored = !is.na(time.stat.end) & time.stat.end < time,
        event = ifelse(censored, 0, event),
        time = ifelse(censored, time.stat.end, time) ),
    .data0 %>% 
      subset(exposure == 'control') %>%
      mutate(
        time.beg = as.numeric(stat.beg.all - dintro) + beg_stat * 30,
        censored = !is.na(time.beg) & time.beg < time,
        event = ifelse(censored, 0, event),
        time = ifelse(censored, time.beg, time) ) )
}