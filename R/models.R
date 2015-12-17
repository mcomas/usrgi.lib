#' ifelse when used with Dates
#' 
#' @param mi_data multiple imputation data with each imputation in stacked by rows
#' @param str_coxph string containing the cox models to be evaluated (default="coxph(Surv(time = time, event = event)~1")
#' @return result of fitting the model
#' 
#' @export
fit_cox.mi = function(mi_data, str_coxph = "coxph(Surv(time = time, event = event)~1"){ # = "coxph(Surv(time = time, event = event)~user+age+age^2+sex+htn+smoking+coltot_cat+colhdl_cat)"
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
        with(.d,  eval(parse(text = gsub('user', 'strata(user)', str_coxph))))
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
      inc_user = matrix(0, dimnames = list('user', 'est'))
    }
    if( df_events %>% filter(exposure == 'control') %>% select(n) %>% min > 0 ){
      suppressWarnings(
        inc_cntr <- summary(pool(as.mira(lapply(l_df, with, incidence(event = event[user==0], time = time[user==0])))))
      )
    }else{
      inc_cntr = matrix(0, dimnames = list('control', 'est'))
    }
  }
  with(mi_data,
       data_frame('ctrl.n' = sum(user==0)/nimp,
                  'ctrl.ev' = sum(event[user==0]/nimp),
                  'ctrl.inc' = inc_cntr[1, 'est'],
                  'user.n' = sum(user==1)/nimp,
                  'user.ev' = sum(event[user==1])/nimp,
                  'user.inc' = inc_user[1, 'est'],
                  'haz' = exp(mod['user', 'est']), 
                  'haz.lo' = exp(mod['user', 'lo 95']), 
                  'haz.hi' = exp(mod['user', 'hi 95']),
                  'nnt.cox' = nnt.cox,
                  'nnt' = nnt['risk']) )
}