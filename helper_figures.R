#Example: 
#plot_individual(dd,"param_1", 0.95)

plot_individual <- function(data, var, cl){
  variable <- as.character(pull(data, Parameter))
  order <- unique(pull(data,order)[!is.na(pull(data, order))])
  variable_output <- variable[1:length(order)] # these are the  that we compare their estimates across states
  variable_order <- as.data.frame(cbind(variable_output, order))
  
  index = variable_order%>%
    filter(variable_output == var)%>%
    select(order)%>%
    pull()%>%
    as.numeric()
  out <- generate_var_lists(data, cl)
  metahksj <- out$metahksj_ls[[index]]
  forest(metahksj,
         level = cl) # TODO: CL or PCL
}

#odds = "RR" if unit is log odds ratio
#odds = "OR" if unit is odds ratio
data <- dd
cl = 0.95
pcl = 0.90
plot_global <- function(data, cl, pcl){
  odds = "RR"
  data <- generate_global_estimates(data, cl, pcl)
  data <- add_desc(data)
  
  ggplot_data_long <- data%>%
    select(order, Parameter, estimate, OR_estimate, ci_lb, OR_ci_lb, ci_ub, OR_ci_ub,
           pci_lb, OR_pci_lb, pci_ub, OR_pci_ub, description, description_OR)%>%
    gather(est, value, - c(order, Parameter, description_OR, description ))%>%
    mutate(odds.1 = if_else(est %in% 
                              c("OR_estimate","OR_ci_lb","OR_ci_ub", "OR_pci_lb","OR_pci_ub"),
                            "OR", "RR"))%>%
    gather(type, description , - c(order, Parameter, est, value,odds.1))%>%
    mutate(odds.2 = if_else(type == "description_OR","OR","RR"))%>%
    filter(odds.1 == odds.2)%>%
    select(-c(odds.2, type))%>%
    rename( odds =odds.1)
  
  ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, fixed("OR"), "")
  ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, "[[:punct:]]", "")
  
  if (odds == "RR"){   #log odds ratio 
    xintercept = 0
  }else if (odds == "OR"){   #odds ratio
    xintercept = 1
  }else{
    return("Error: No odds unit selection.")
  }
  
  ### ggplot the log OR
  p <- ggplot_data_long%>%
    filter(odds == "RR")%>% 
    select(-c(order,odds))%>%
    spread(est, value)%>%
    ggplot(aes(x = (estimate), y = Parameter, group = 1)) +
    geom_errorbarh(height = 0.0, size = 1.8, aes(xmin = pcilb, xmax = pciub), colour="grey88", alpha = 1) + #grey88
    geom_errorbarh(height = 0.0, size = 0.8, aes(xmin = cilb, xmax = ciub), colour="grey22", alpha = 0.5) + #grey22
    geom_point(colour = "black", size = 1.8, alpha = 1) +
    labs(y = NULL, title = NULL ) + 
    #geom_vline(xintercept = 0, linetype = "dashed", color = "blue", alpha = 0.5) +
    geom_vline(xintercept = xintercept, linetype = "dashed", color = "blue", alpha = 0.5)+
    theme_bw() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1, size = rel(1.2)),
          axis.title.y = element_text(size = rel(1.2)),
          strip.text.y = element_text(size = rel(0.6)),
          axis.text.x = element_text(size = rel(1.1)),
          axis.title.x = element_text(size = rel(1.2), hjust = 0.5),
          plot.title = element_text(size = rel(1.2)),
          strip.background = element_rect(fill="gray95"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) 
  if(odds == "RR"){
    p <- p +
      scale_x_continuous(
        breaks = seq(-2, 2, by = 0.5)
      )
  }else if (odds == "OR"){
    
    p <- p + coord_trans(x = "log2") +
      scale_x_continuous(limits = c(0.5, 8), # make the x range to be wider on the left side
                         breaks = seq(0.5, 8, by = 1)
      )
    
  }
  
  # ply <- ggplotly(p, tooltip =  c("description"))%>%
  #   layout(legend = list(
  #     orientation = "h"
  #   ))
  return(p)
  
}

# plot_global <- function(data, cl, pcl){
#   odds = "RR"
#   data <- generate_global_estimates(data, cl, pcl)
#   data <- add_desc(data)
#   
#   ggplot_data_long <- data%>%
#     select(order, Parameter, estimate, OR_estimate, ci_lb, OR_ci_lb, ci_ub, OR_ci_ub,
#            pci_lb, OR_pci_lb, pci_ub, OR_pci_ub, description, description_OR)%>%
#     gather(est, value, - c(order, Parameter, description_OR, description ))%>%
#     mutate(odds.1 = if_else(est %in% 
#                               c("OR_estimate","OR_ci_lb","OR_ci_ub", "OR_pci_lb","OR_pci_ub"),
#                             "OR", "RR"))%>%
#     gather(type, description , - c(order, Parameter, est, value,odds.1))%>%
#     mutate(odds.2 = if_else(type == "description_OR","OR","RR"))%>%
#     filter(odds.1 == odds.2)%>%
#     select(-c(odds.2, type))%>%
#     rename( odds =odds.1)
#   
#   ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, fixed("OR"), "")
#   ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, "[[:punct:]]", "")
#   
#   if (odds == "RR"){   #log odds ratio 
#     xintercept = 0
#   }else if (odds == "OR"){   #odds ratio
#     xintercept = 1
#   }else{
#     return("Error: No odds unit selection.")
#   }
#   
#   ### ggplot the log OR
#   p <- ggplot_data_long%>%
#     filter(odds == "RR")%>% 
#     select(-c(order,odds))%>%
#     spread(est, value)%>%
#     ggplot(aes(x = (estimate), y = Parameter, group = 1, description = description)) +
#     geom_errorbarh(height = 0.0, size = 1.8, aes(xmin = pcilb, xmax = pciub), colour="grey88", alpha = 1) + #grey88
#     geom_errorbarh(height = 0.0, size = 0.8, aes(xmin = cilb, xmax = ciub), colour="grey22", alpha = 0.5) + #grey22
#     geom_point(colour = "black", size = 1.8, alpha = 1) +
#     labs(y = NULL, title = NULL ) + 
#     #geom_vline(xintercept = 0, linetype = "dashed", color = "blue", alpha = 0.5) +
#     geom_vline(xintercept = xintercept, linetype = "dashed", color = "blue", alpha = 0.5)+
#     theme_bw() +
#     theme(axis.text.y = element_text(angle = 0, hjust = 1, size = rel(1.2)),
#           axis.title.y = element_text(size = rel(1.2)),
#           strip.text.y = element_text(size = rel(0.6)),
#           axis.text.x = element_text(size = rel(1.1)),
#           axis.title.x = element_text(size = rel(1.2), hjust = 0.5),
#           plot.title = element_text(size = rel(1.2)),
#           strip.background = element_rect(fill="gray95"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
#     ) 
#   if(odds == "RR"){
#     p <- p +
#       scale_x_continuous(
#         breaks = seq(-2, 2, by = 0.5)
#       )
#   }else if (odds == "OR"){
#     
#     p <- p + coord_trans(x = "log2") +
#       scale_x_continuous(limits = c(0.5, 8), # make the x range to be wider on the left side
#                          breaks = seq(0.5, 8, by = 1)
#       )
#     
#   }
#   
#   ply <- ggplotly(p, tooltip =  c("description"))%>%
#     layout(legend = list(
#       orientation = "h"
#     ))
#   return(ply)
#   
# }


