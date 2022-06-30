outdata <- generate_global_estimates(dd, 0.95, 0.90)


outdata <- add_desc(outdata)


### output data: ggplot_data_long
# integrates shiny feature: select odds or odds ratio
# ggplot_data_long <- ggplotdata%>%
#   select(order, variables, estimate, OR_estimate, ci_lb, OR_ci_lb, ci_ub, OR_ci_ub,
#          pci_lb, OR_pci_lb, pci_ub, OR_pci_ub, variablesgroup, description, description_OR)%>%
#   gather(est, value, - c(order, variables,variablesgroup))%>%
#   mutate(odds = if_else(est %in% 
#                           c("OR_estimate","OR_ci_lb","OR_ci_ub", "OR_pci_lb","OR_pci_ub","description_OR"),
#                         "OR", "RR"))




