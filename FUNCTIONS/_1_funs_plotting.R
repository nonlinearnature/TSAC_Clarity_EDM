## Visualizing Null Results

# Collecting:

collect_null_results <- function(outputs,labels){
  
  outputs_collected <- map2_dfr(outputs,labels,function(x,y) x %>% mutate(method=y))
  outputs_collected <- outputs_collected %>%
    mutate(method=as_factor(method))
  return(outputs_collected)
  
}

# Labeling: 
dict_Fig4_labelling <- list(
  NO3_sechi = "nitrate-Secchi",
  NO3_deep_euphotic = "nitrate-deep euphotic",
  Chla_sechi = "Chla-Secchi",
  Chla_deep_euphotic = "Chla-deep euphotic",
  Secchi_Ave = "average Secchi"
)

# Jitter plots:
  

plot_null_jitters <- function(null_result,edm_stat,facet_scales="fixed"){
  
  ##
  g <- null_result %>% 
    ggplot(aes_string(x="method",y=edm_stat)) + 
    geom_jitter() + 
    facet_wrap(~target,ncol=1,scales=facet_scales)
  ## Add empirical result
  xz
  
  return(g)
}


# Flipped violins:
  
plot_null_violins <- function(null_result,edm_stat,facet_scales="fixed"){
  g <- null_result %>% 
    ggplot(aes_string(x="method",y=edm_stat)) + 
    geom_violin() + coord_flip() + 
    facet_wrap(~target,ncol=1,scales=facet_scales)
  
}