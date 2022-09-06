#Example: 
#plot_individual(data,"param_1", 0.95, 0.95,0.95)
# cl_ind: individual confidence interval
# cl_global: global confidence interval

plot_individual <- function(data, var, cl_ind, cl_global){
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
  out <- generate_var_lists(data, cl_ind)
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
         level = cl_ind, # individual CI level
         refline = 0, # vertical reference line
         pch = 19, # dot shape/type
         # transf = exp, # whether transformation of scale should be done
         showweights = FALSE, 
         header = c("Site", "Log OR [95% CI]"), # CHECK LABEL TO BE Log OR
         top = 2) # Plots 95% CI and 95% PI
  addpoly(metahksj, row = 0.5, cex = 0.65, mlab = "Global", addcred = TRUE, 
          # transf = exp, # whether transformation of scale should be done
          level = cl_global, annotate = TRUE) # global confidence level
  abline(h = 1)
}

#plot_individual_export(data, 0.95, 0.95,"plot_test.pdf")
#TODO: export user input file collapsed, possibly the pdf wasn't saved into www/ folder. 
plot_individual_export <- function(data, cl_ind, cl_global, filename){
  pdf(file = paste0("www/", filename,sep=""), # START saving plots in a pdf file
      width = 8.5, # width of the plot in inches
      height = 11) # height of the plot in inches
  par(mfrow=c(6, 3), # plotting options: each page has 6 rows and 3 columns, 18 sub-figures on a page; 
      mar = c(4, 3, 1, 2)) # mar sets the margin sizes in the following order: bottom, left, top, and right
  
  variable <- get_variable_names(data)
  
  for(var in variable){
    plot_individual(data, var, cl_ind, cl_global)
  }
  dev.off() # END saving plots in a pdf file
  par(mfrow=c(1,1)) # reset plotting options
}

#odds = "RR" if unit is log odds ratio
#odds = "OR" if unit is odds ratio
#scale = "OR"
#plot_global(data, 0.95,0.95, "logOR")
plot_global <- function(data, cl, pcl, scale){
  data_estimates <- generate_global_estimates(data, cl, pcl)
  data_estimates <- add_desc(data_estimates)
  
  order <- get_variable_names(data_estimates)
  f <- factor(data_estimates$Parameter, level = order)
  data_estimates$Parameter <- fct_rev(f)

  if(scale == "logOR"){
    ### ggplot the log OR (original scale)
    p <- data_estimates%>%
     # ggplot(aes(x = estimate, y = Parameter, group = 1, "CI lower bound"=ci_lb, ci_ub=ci_ub,  "PCI"= paste("(", pci_lb, ",",pci_ub,")",sep="") )) +
      ggplot(aes(y = Parameter, x = estimate, group = 1, ci_lb=ci_lb, ci_ub=ci_ub,  pci_lb=pci_lb, pci_ub=pci_ub , pvalue= pvalue)) +
      geom_errorbarh(height = 0.0, size = 1.8, aes(xmin = pci_lb, xmax = pci_ub), colour="grey88", alpha = 1) + #grey88
      geom_errorbarh(height = 0.0, size = 0.8, aes(xmin = ci_lb, xmax = ci_ub), colour="grey22", alpha = 0.5) + #grey22
      geom_point(colour = "black", size = 1.8, alpha = 1) +
      labs(y = NULL, title = NULL ) + 
      geom_vline(xintercept = 0, linetype = "dashed", color = "blue", alpha = 0.5)+
      scale_x_continuous( limits = c(-1, 2), 
                          breaks = c(-1, -0.5 , 0, 0.5, 1, 1.5, 2),
                          label =c("-1", "-0.5", "0", "0.5", "1", "1.5", "2"))+
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
      )+
      labs(x = "Estimate(original scale)")
  }else{ # exponentiated
    
    p <- data_estimates%>%
      ggplot(aes(y = Parameter,x = exp(estimate),  group=1, ci_lb=exp(ci_lb), ci_ub=exp(ci_ub),  pci_lb=exp(pci_lb), pci_ub=exp(pci_ub) , pvalue= pvalue)) +
      geom_errorbarh(height = 0.0, size = 1.8, aes(xmin = exp(pci_lb), xmax = exp(pci_ub)), colour="grey88", alpha = 1) + #grey88
      geom_errorbarh(height = 0.0, size = 0.8, aes(xmin = exp(ci_lb), xmax = exp(ci_ub)), colour="grey22", alpha = 0.5) + #grey22
      geom_point(colour = "black", size = 1.8, alpha = 1) +
      labs(x = "Estimate(exponentiated)")+
    #  coord_trans(x = "log2") +
     scale_x_continuous( limits = c(0.1, 6), #TODO: change the lower bound to be dependent on the min(pci_lb) and upper bound to be dependent on max(pci_ub)
                         breaks = c(0.2, 0.5 , 1, 2, 4, 6), #TODO: 
                         label =  c("0.2", "0.5", "1", "2", "4", "6"))+
      geom_vline(xintercept = 1, linetype = "dashed", color = "blue", alpha = 0.5)+
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
  }
  
  ply <- ggplotly(p) %>%
    layout(legend = list(
      orientation = "h"
    )) 
    
  return(ply)
  
}

