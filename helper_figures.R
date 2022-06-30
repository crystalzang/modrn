#Example: 
#plot_individual(dd,"param_1", 0.95)

plot_individual <- function(data, var, cl){
  variable <- as.character(pull(data, Parameter))
  order <- pull(data,order)[!is.na(pull(data, order))]
  variable_output <- variable[1:length(order)] # these are the  that we compare their estimates across states
  variable_order <- as.data.frame(cbind(variable_output, order))
  
  # index = variable_order%>%
  #   filter(variable_output == var)%>%
  #   select(order)%>%
  #   pull()%>%
  #   as.numeric()
  out <- generate_var_lists(data, cl)
 # metahksj <- out$metahksj_ls[[index]]
  
  metahksj <- out$metahksj_ls[[i]]
  #dat <- out$dat_ls[[index]]
  forest(metahksj,
         level =cl) # TODO: CL or PCL
}


plot_global <- function(data, cl){
  
}
