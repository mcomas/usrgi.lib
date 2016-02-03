#' Build a dataframe with the Nelson-Aalen survival estimates
#' 
#' @param mi_data multiple imputation data with each imputation in stacked by rows
#' @param str_coxph string containing the cox models to be evaluated (default="Surv(time = time, event = event)~user")
#' @param min_obs minimum number of imputations with event in specific time to evaluated the average
#' @return result table with survival time
#' 
#' @export
cuminc.mi = function(mi_data, str_coxph = "Surv(time = time, event = event)~user", min_obs = 5){
  nimp = length(unique(mi_data$imp))
  l_df = split(mi_data, mi_data$imp)
  df_events = mi_data %>% dplyr::group_by(exposure) %>% dplyr::summarise(n = sum(event))
  if( df_events %>% select(n) %>% min > 0 ){
    suppressWarnings(
      df.survfit <- lapply(l_df, function(.d){
        sf = with(.d,  survfit(coxph(eval(parse(text = gsub('user', 'strata(user)', str_coxph))), ties='breslow'), type='aalen'))
        data_frame(
          'surv' = sf$surv,
          'lower' = sf$lower,
          'upper' = sf$upper,
          'time' = sf$time,
          'strata' = rep(c('control', 'user'), sf$strata),
          'imp' = unique(.d$imp)
        )
      }) %>% bind_rows %>%
        group_by(strata, time) %>%
        summarise(
          n = n(),
          surv = mean(surv),
          lower = mean(lower),
          upper = mean(upper)) %>% ungroup %>%
        filter(n >= min_obs)
    )
  }else{
    data_frame(
      'surv' = numeric(0),
      'lower' = numeric(0),
      'upper' = numeric(0),
      'time' = numeric(0),
      'strata' = character(0),
      'imp' = numeric(0)
    )
  }
}