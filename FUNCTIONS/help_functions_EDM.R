SurrogateData.AR <- function(ts,num_surr=100,AR_coeff=NULL){
  
  # Fit AR model
  if(is.null(AR_coeff)){
    
    ar.ts <- ar(ts,order.max = 1,na.action=na.omit)
    AR_coeff <- ar.ts$ar
    
  }
  
  df_surr <- map_dfc(1:num_surr, function(idx_surr){
    ts.surr <- arima.sim(list(ar = AR_coeff),n=length(ts))
    ts.surr <- data.frame(V=as.double(ts.surr))
  })
  
  return(data.matrix(df_surr))
  
}


do_univariate_1_var <- function(df,target_col){
  
  E_list <- 1:15
  theta_list <- c(0,10^seq(-2,1,by=.075))
  
  lib <- paste(1,NROW(df))
  pred <- paste(1,NROW(df))
  
  ## Simplex
  stats_simplex <- map_df(E_list,function(E_i){
    out_simplex_i <- Simplex(dataFrame=df,
                             target=target_col,
                             columns=target_col,
                             lib=lib,pred=pred,
                             E=E_i,
                             parameterList=TRUE)
    
    stats_i <- compute_stats(out_simplex_i$predictions$Predictions,
                             out_simplex_i$predictions$Observations)
    
    stats_i <- bind_cols(out_simplex_i$parameters[L_save_params],
                         stats_i
    )
    
    return(stats_i)
    
  })
  
  stats_simplex <- suppressMessages(type_convert(stats_simplex))
  
  E_star <- as.integer(stats_simplex$E[which.max(stats_simplex$rho)])
  
  ## S-map
  stats_smap <- map_df(theta_list,function(theta_i){
    
    out_smap_i <- SMap(dataFrame=df,
                       target=target_col,
                       columns=target_col,
                       lib=lib,pred=pred,
                       E=E_star,
                       theta=theta_i,
                       parameterList = TRUE)
    
    stats_i <- compute_stats(out_smap_i$predictions$Predictions,out_smap_i$predictions$Observations)
    
    stats_i <- bind_cols(
      out_smap_i$parameters[L_save_params],
      stats_i
    )
    
    return(stats_i)
    
  })
  
  stats_smap <- suppressMessages(type_convert(stats_smap))
  
  return(list(simplex=stats_simplex,smap=stats_smap))
  
}