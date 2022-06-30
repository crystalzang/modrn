outdata <- generate_global_estimates(dd, 0.95, 0.90)

# descriptions
outdata$description <- paste0("(global RR ", round(outdata$estimate, 2), ", ",
                              "95% CI: ", round(outdata$ci_lb, 2), "-", round(outdata$ci_ub, 2),
                              ", p", ifelse(outdata$pvalue < 0.0001, "<0.0001", paste0("=", signif(outdata$pvalue, 2))), ", ", 
                              "90% PI: ", round(outdata$pci_lb, 2), "-", round(outdata$pci_ub, 2),
                              ")")

outdata$description_OR <- paste0("(global OR ", round(outdata$OR_estimate, 2), ", ",
                              "95% CI: ", round(outdata$OR_ci_lb, 2), "-", round(outdata$OR_ci_ub, 2),
                              ", p", ifelse(outdata$pvalue < 0.0001, "<0.0001", paste0("=", signif(outdata$pvalue, 2))), ", ", 
                              "90% PI: ", round(outdata$OR_pci_lb, 2), "-", round(outdata$OR_pci_ub, 2),
                              ")")








### output data: ggplot_data_long
# integrates shiny feature: select odds or odds ratio
# ggplot_data_long <- ggplotdata%>%
#   select(order, variables, estimate, OR_estimate, ci_lb, OR_ci_lb, ci_ub, OR_ci_ub,
#          pci_lb, OR_pci_lb, pci_ub, OR_pci_ub, variablesgroup, description, description_OR)%>%
#   gather(est, value, - c(order, variables,variablesgroup))%>%
#   mutate(odds = if_else(est %in% 
#                           c("OR_estimate","OR_ci_lb","OR_ci_ub", "OR_pci_lb","OR_pci_ub","description_OR"),
#                         "OR", "RR"))


ggplot_data_long <- outdata%>%
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

