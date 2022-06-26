# Data Processing 
dd <- clean_data(dd, "State")

### prepare the data
dd$Parameter <- paste0(dd$Variable, ifelse(dd$ClassVal0 == "", "", "_"), dd$ClassVal0)

# create a new name for the variables for convenience
uniqueParameter <- unique(dd$Parameter) # all unique variables
uniqueParameter <- c("Intercept", "age_at_index_5yr_inc", "GENDER_M", 
                     "race_ethnic_Hispanic", "race_ethnic_Non-Hispanic black", "race_ethnic_Others",
                     "race_ethnic_Unknown/Missing",
                     "elig_cat_model_Children", "elig_cat_model_Disabled", "rural_Rural", 
                     "ID_1", "MISUD_1", "MEDCOMP_1", 
                     "any_od_preperiod_1", "op_prof_pr_dx_oud_pr_1", "either_UDS_pre_1", 
                     "index_yr_2017", "index_yr_2018", "index_yr_2019") # set the order manually
dd$order <- as.numeric(factor(dd$Parameter, levels = uniqueParameter)) # save the parameter display order in data as a new column

#info_columns would be in the output data
info_columns <- unique(dd[, c("order", "Variable", "ClassVal0", "Parameter", "Exclude_from_forestplot")]) # save the info columns
p <- nrow(info_columns)
dd <- dd[order(dd$site, dd$order), ] # ORDER by state abbreviation, then by the parameter display order


### meta-analysis for each of the adjusted variables at a time, in a for loop
result_est <- c()
result_se <- c()
result_95ci_lb <- c()
result_95ci_ub <- c()
result_90pci_lb <- c()
result_90pci_ub <- c()
result_pval <- c()
result_tau2 <- c()
result_isq <- c()
result_Q <- c()
result_Qp <- c()
result_est_min <- c()
result_est_max <- c()

for(i in 1:p) {
  dd_i <- dd[dd$order == i, ] # data of the corresponding parameter
  estimate <- dd_i[, "Estimate"]
  sderr <- dd_i[, "StdErr"]
  site <- dd_i[, "Site"]
  # issue <- dd[dd$order == i, "possible_issue_variable"] # if any marked as problematic
  keep <- (sderr != 0) & (!is.na(estimate)) # check if stderr = 0 or estimate = NA, exclude
  
  # random effect meta-analysis by the Hartung-Knapp-Sidik-Jonkman method
  # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-25
  dat <- data.frame(yi = estimate[keep], vi = sderr[keep]^2, state = state[keep])
  # meta analysis typically done on the log of the OR, log of the RR, etc
  metahksj <- rma(yi, vi, data = dat, method = "SJ", test="knha", level = 95)
  metahksj_pred90 <- predict(metahksj, level = 90)   ### prediction interval, based on T dist

  result_est <- c(result_est, metahksj$b)
  result_se <- c(result_se, metahksj$se)
  result_pval <- c(result_pval, metahksj$pval)
  result_tau2 <- c(result_tau2, metahksj$tau2)
  result_isq <- c(result_isq, metahksj$I2)
  result_Q <- c(result_Q, metahksj$QE)
  result_Qp <- c(result_Qp, metahksj$QEp)
  result_est_min <- c(result_est_min, min(estimate[keep], na.rm = TRUE))
  result_est_max <- c(result_est_max, max(estimate[keep], na.rm = TRUE))
  result_95ci_lb <- c(result_95ci_lb, metahksj$ci.lb)
  result_95ci_ub <- c(result_95ci_ub, metahksj$ci.ub)
  result_90pci_lb <- c(result_90pci_lb, metahksj_pred90$cr.lb)
  result_90pci_ub <- c(result_90pci_ub, metahksj_pred90$cr.ub)
}



### output dataframe: outdata
outdata <- data.frame(info_columns, 
                      estimate = result_est,
                      se = result_se,
                      ci_lb = result_95ci_lb,
                      ci_ub = result_95ci_ub,
                      OR_estimate = exp(result_est),
                      OR_ci_lb = exp(result_95ci_lb),
                      OR_ci_ub = exp(result_95ci_ub),
                      pvalue = result_pval, 
                      tau = sqrt(result_tau2),
                      pci_lb = result_90pci_lb,
                      pci_ub = result_90pci_ub,
                      OR_pci_lb = exp(result_90pci_lb),
                      OR_pci_ub = exp(result_90pci_ub),
                      Isquare = result_isq, CochranQ = result_Q, 
                      CochranQpvalue = result_Qp, 
                      range_lb = result_est_min,
                      range_ub = result_est_max)

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





### output dataframe: outdata_outcome
# FIGURE per JAMA style
outdata_outcome <- outdata[is.na(outdata$Exclude_from_forestplot), ] # exclude some not to be plot

variables <- c("Age Index", "Male", "Hispanic", "Non Hisp. Black", "Other Race/Ethnicity",
               "Children", "Disabled", "Rural", "ID", "MISUD", "MEDCOMP",
               "Any OD Pre", "OD Prof Pr Dx Oud Pr", "Either UDS Pre",
               "2017", "2018", "2019")
#cbind(outdata_outcome$Parameter, variables)

# because we want to also plot reference levels, we create the data for those
refvariables <- c("2014 [REF]", "Non Disabled Adult [REF]", "Non Hisp. White [REF]")
outdata_outcome <- rbind(outdata_outcome, NA, NA, NA)
rownames(outdata_outcome) <- c(variables, refvariables)
outdata_outcome$estimate[is.na(outdata_outcome$estimate)] <- 0
outdata_outcome$pci_lb[is.na(outdata_outcome$pci_lb)] <- 0
outdata_outcome$pci_ub[is.na(outdata_outcome$pci_ub)] <- 0
outdata_outcome$ci_lb[is.na(outdata_outcome$ci_lb)] <- 0
outdata_outcome$ci_ub[is.na(outdata_outcome$ci_ub)] <- 0


### output dataframe: ggplotdata
ggplotdata <- outdata_outcome
ggplotdata$variables <- as.factor(rownames(ggplotdata))

ggplotdata$variables <- factor(ggplotdata$variables,    
                               levels = c("Age Index", "Male", "Hispanic", "Non Hisp. Black", "Other Race/Ethnicity", "Children", "Disabled", "Rural", "ID", "MISUD", "MEDCOMP", "Any OD Pre",  "OD Prof Pr Dx Oud Pr", "Either UDS Pre", "2019", "2018", "2017", "2014 [REF]", "Non Disabled Adult [REF]", "Non Hisp. White [REF]"))

# create variable groups for clustered display
ggplotdata$variablesgroup <- as.factor(c("Age", "Sex", "Race/Ethnicity", "Race/Ethnicity", "Race/Ethnicity",  "Eligibility Group", "Eligibility Group",
                                         "Region", "ID", "MISUD", "MEDCOMP", "Any OD Pre", "OD Prof Pr Dx Oud Pr", "Either UDS Pre",
                                         "Year", "Year", "Year", 
                                         "Year", "Eligibility Group", "Race/Ethnicity"))

ggplotdata$variablesgroup <- factor(ggplotdata$variablesgroup,
                                    levels = c("Age", "Sex", "Race/Ethnicity", "Eligibility Group", "Region", "Year", "ID", "MISUD", "MEDCOMP", "Any OD Pre", "OD Prof Pr Dx Oud Pr", "Either UDS Pre"))



### output data: ggplot_data_long
# integrates shiny feature: select odds or odds ratio
# ggplot_data_long <- ggplotdata%>%
#   select(order, variables, estimate, OR_estimate, ci_lb, OR_ci_lb, ci_ub, OR_ci_ub,
#          pci_lb, OR_pci_lb, pci_ub, OR_pci_ub, variablesgroup, description, description_OR)%>%
#   gather(est, value, - c(order, variables,variablesgroup))%>%
#   mutate(odds = if_else(est %in% 
#                           c("OR_estimate","OR_ci_lb","OR_ci_ub", "OR_pci_lb","OR_pci_ub","description_OR"),
#                         "OR", "RR"))


ggplot_data_long <- ggplotdata%>%
  select(order, variables, estimate, OR_estimate, ci_lb, OR_ci_lb, ci_ub, OR_ci_ub,
         pci_lb, OR_pci_lb, pci_ub, OR_pci_ub, variablesgroup, description, description_OR)%>%
  gather(est, value, - c(order, variables,variablesgroup, description_OR, description ))%>%
  mutate(odds.1 = if_else(est %in% 
                          c("OR_estimate","OR_ci_lb","OR_ci_ub", "OR_pci_lb","OR_pci_ub"),
                        "OR", "RR"))%>%
  gather(type, description , - c(order, variables,variablesgroup, est, value,odds.1))%>%
  mutate(odds.2 = if_else(type == "description_OR","OR","RR"))%>%
  filter(odds.1 == odds.2)%>%
  select(-c(odds.2, type))%>%
  rename( odds =odds.1)

ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, fixed("OR"), "")
ggplot_data_long$est <- str_replace_all(ggplot_data_long$est, "[[:punct:]]", "")

