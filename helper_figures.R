#Example: 
#plot_individual(dd,"param_1", 0.95)
# data <- dd
# var = "param_1"
# cl = pcl = 0.95
plot_individual <- function(data, var, cl, pcl){
  variable <- as.character(pull(data, Parameter))

  order <- unique(pull(data, order)[!is.na(pull(data, order))])
  variable_output <- variable[1:length(order)] # these are the  that we compare their estimates across states
  variable_order <- as.data.frame(cbind(variable_output, order))
  
  site <- unique(as.character(pull(data, site)))
  
  index = variable_order%>%
    filter(variable_output == var)%>%
    select(order)%>%
    pull()%>%
    as.numeric()
  out <- generate_var_lists(data, cl)
  metahksj <- out$metahksj_ls[[index]]
  
  forest(metahksj, 
         addfit = FALSE, # set this to false to suppress global, will manually add later
         addcred = FALSE, # set this to false to suppress global, will manually add later
         slab = site, # study label
         ylim = c(0, metahksj$k+3),
         rows = c((metahksj$k+1):2), # can be adjusted, height location of display [leave room for global at bottom]
         mlab = "Summary:", 
        # xlab = dd_i[, "Parameter"][1], # x-axis label
         xlab = var,
         psize = 0.8, # dot size
         level = cl, # CI level
         refline = 0, # vertical reference line
         pch = 19, # dot shape/type
         # transf = exp, # whether transformation of scale should be done
         showweights = FALSE, 
         header = c("Site", "Log OR [95% CI]"), # CHECK LABEL TO BE Log OR
         top = 2) # Plots 95% CI and 95% PI
  addpoly(metahksj, row = 0.5, cex = 0.65, mlab = "Global", addcred = TRUE, 
          # transf = exp, # whether transformation of scale should be done
          level = pcl, annotate = TRUE) # in this way, the CI will be 95%, the PI will be 90% [this is a work around]
  abline(h = 1)
}

#odds = "RR" if unit is log odds ratio
#odds = "OR" if unit is odds ratio
#data = dd
#scale = "OR"
#plot_global(dd, 0.95,0.95, "logOR")
plot_global <- function(data, cl, pcl, scale){
  data <- generate_global_estimates(data, cl, pcl)
  data <- add_desc(data)
  
  ggplot_data_long <- data%>%
    select(order, Parameter, estimate, OR_estimate, ci_lb, OR_ci_lb, ci_ub, OR_ci_ub,
           pci_lb, OR_pci_lb, pci_ub, OR_pci_ub, description, description_OR)%>%
    gather(est, value, - c(order, Parameter, description_OR, description ))%>%
    mutate(odds.1 = if_else(est %in% 
                              c("OR_estimate","OR_ci_lb","OR_ci_ub", "OR_pci_lb","OR_pci_ub"),
                            "OR", "logOR"))%>%
    gather(type, description , - c(order, Parameter, est, value,odds.1))%>%
    mutate(odds.2 = if_else(type == "description_OR","OR","logOR"))%>%
    filter(odds.1 == odds.2)%>%
    select(-c(odds.2, type))%>%
    rename( odds =odds.1)
  
  ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, fixed("OR"), "")
  ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, "[[:punct:]]", "")
 

  order <- get_variable_names(dd)
  f <- factor(ggplot_data_long$Parameter, level = order)
  ggplot_data_long$Parameter <- fct_rev(f)
  #levels(ggplot_data_long$Parameter)
  
  if (scale == "OR"){   #log odds ratio 
    xintercept = 1
  }else if (scale == "logOR"){   #odds ratio
    xintercept = 0
  }else{
    return("Error: No odds unit selection.")
  }
  
  ### ggplot the log OR
  p <- ggplot_data_long%>%
    filter(odds ==  scale)%>% 
    select(-c(order, odds))%>%
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
  if(scale == "logOR"){
    p <- p +
      scale_x_continuous(
        breaks = seq(-2, 2, by = 0.5)
      )+
      labs(x = "Estimate(log odds ratio)")
  }else if (scale == "OR"){
    p <- p + 
      #coord_trans(x = "log2")+
      scale_x_continuous(
          limits = c(0, 10), # make the x range to be wider on the left side
                         breaks = seq(0, 8, by = 1))+
      labs(x = "Estimate(odds ratio)")
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


