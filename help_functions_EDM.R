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