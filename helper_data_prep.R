#Example
#get_variable_names(data_toy)
# output a vector of parameter names
get_variable_names <- function(df){
  variable <- as.character(pull(df, Parameter))
  variable <- unique(variable)
  return(variable)
}
# this function is used in helper_figure.R
# generates estimates for each variable for each site
# generate_var_lists(data_toy, i_level = 0.95)
generate_var_lists <- function(df, i_level){
  out_ls <- list()
  dat_ls <- list()
  metahksj_ls <- list()
  order <- pull(df,order)[!is.na(pull(df,order))]
  variable <- as.character(pull(df, Parameter))
  p <- max(order)
  
  for(i in 1:p){
    df_i <- df[df$order == i, ] # data of the corresponding parameter
    estimate <- df_i[, "Estimate"]
    sderr <- df_i[, "StdErr"]
    site <- df_i[, "site"]
    keep <- (sderr != 0) & (!is.na(estimate)) # check if stderr = 0 or estimate = NA, exclude
    # random effect meta-analysis by the Hartung-Knapp-Sidik-Jonkman method
    # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-25
    dat <- data.frame(yi = estimate[keep], vi = sderr[keep]^2, site = site[keep])
    dat_ls[[i]] <- dat
    
    metahksj <- rma(yi, vi, data = dat, method = "SJ", test="knha", level = i_level)
    #metahksj <- rma(yi, vi, data = dat, method = "SJ", test="knha", level = input$cl)
    metahksj_ls[[i]] <- metahksj
  }
  var_order <- c()
  for (i in 1:max(order)){
    if (i %in% order){
      k = sum(is.na(var_order))
      var_order[i] <- variable[i-k]
    }else{
      var_order[i] = NA
    }
  }
  names(dat_ls) <- var_order
  names(metahksj_ls) <- var_order
  
  
  out_ls[[1]] <- dat_ls
  out_ls[[2]] <- metahksj_ls
  names(out_ls) <- c("dat_ls" , "metahksj_ls")
  
  return(out_ls)
}



# Rename site to numbers 
#data <- rename_site(dd, "State")
rename_site <- function(data, site){
  site_index <- grep(site, colnames(data))
  site <- pull(unique(data[site_index])) # the unique states included in this analysis
  k <- length(site) #Number of unique sites
  colnames(data)[site_index] <- "site"
  set.seed(1)
  key = sample(k)
  
  dictionary <- as.data.frame(cbind(site, key))
  
  data <- left_join(data, dictionary, by = "site")
  data <- data%>%
    select(-site)%>%
    rename(site = key)
  
  return(data)
}

#dd_out <- generate_global_estimates(data_toy, 0.95, 0.90)
generate_global_estimates <- function(data, cl, pcl){
  info_columns <- unique(data[, c( "order", "Parameter")]) # save the info columns
  
  
  p = length(unique(data$order)) #number of unique variables
  result_est <- c()
  result_se <- c()
  result_ci_lb <- c()
  result_ci_ub <- c()
  result_pci_lb <- c()
  result_pci_ub <- c()
  result_pval <- c()
  result_tau2 <- c()
  result_isq <- c()
  result_Q <- c()
  result_Qp <- c()
  result_est_min <- c()
  result_est_max <- c()
  
  for(i in 1:p) {
    data_i <- data[data$order == i, ] # data of the corresponding parameter
    estimate <- data_i[, "Estimate"]
    sderr <- data_i[, "StdErr"]
    site <- data_i[, "site"]
    keep <- (sderr != 0) & (!is.na(estimate)) # check if stderr = 0 or estimate = NA, exclude
    
    # random effect meta-analysis by the Hartung-Knapp-Sidik-Jonkman method
    # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-25
    dat <- data.frame(yi = estimate[keep], vi = sderr[keep]^2, site = site[keep])
    # meta analysis typically done on the log of the OR, log of the RR, etc
    metahksj <- rma(yi, vi, data = dat, method = "SJ", test="knha", level = cl)
    metahksj_pred <- predict(metahksj, level = pcl)   ### prediction interval, based on T dist
    
    result_est <- c(result_est, metahksj$b)
    result_se <- c(result_se, metahksj$se)
    result_pval <- c(result_pval, metahksj$pval)
    result_tau2 <- c(result_tau2, metahksj$tau2)
    result_isq <- c(result_isq, metahksj$I2)
    result_Q <- c(result_Q, metahksj$QE)
    result_Qp <- c(result_Qp, metahksj$QEp)
    result_est_min <- c(result_est_min, min(estimate[keep], na.rm = TRUE))
    result_est_max <- c(result_est_max, max(estimate[keep], na.rm = TRUE))
    result_ci_lb <- c(result_ci_lb, metahksj$ci.lb)
    result_ci_ub <- c(result_ci_ub, metahksj$ci.ub)
    result_pci_lb <- c(result_pci_lb, metahksj_pred$cr.lb)
    result_pci_ub <- c(result_pci_ub, metahksj_pred$cr.ub)
  }
  
  A <- function(x) round(x, digits = 4)
  
  ### output dataframe: outdata
  outdata <- data.frame(info_columns, 
                        estimate = result_est,
                        se = result_se,
                        ci_lb = result_ci_lb,
                        ci_ub = result_ci_ub,
                       # OR_estimate = exp(result_est),
                       # OR_ci_lb = exp(result_ci_lb),
                       # OR_ci_ub = exp(result_ci_ub),
                        pvalue = result_pval, 
                        tau = sqrt(result_tau2),
                        pci_lb = result_pci_lb,
                        pci_ub = result_pci_ub,
                       # OR_pci_lb = exp(result_pci_lb),
                        #OR_pci_ub = exp(result_pci_ub),
                        Isquare = result_isq, CochranQ = result_Q, 
                        CochranQpvalue = result_Qp, 
                        range_lb = result_est_min,
                        range_ub = result_est_max)%>%
    mutate(across(c(3:12, 14:15), A))

  rownames(outdata) <- NULL
  return(outdata)
}

add_desc <- function(data){
  # descriptions
  data$description <- paste0("(global estimate - original scale", round(data$estimate, 2), ", ",
                                "95% CI: ", round(data$ci_lb, 2), "-", round(data$ci_ub, 2),
                                ", p", ifelse(data$pvalue < 0.0001, "<0.0001", paste0("=", signif(data$pvalue, 2))), ", ", 
                                "90% PI: ", round(data$pci_lb, 2), "-", round(data$pci_ub, 2),
                                ")")
  
  data$description_OR <- paste0("(global estimate - exponentiated", round(exp(data$estimate), 2), ", ",
                                   "95% CI: ", round(exp(data$ci_lb), 2), "-", round(exp(data$ci_ub), 2),
                                   ", p", ifelse(data$pvalue < 0.0001, "<0.0001", paste0("=", signif(data$pvalue, 2))), ", ", 
                                   "90% PI: ", round(exp(data$pci_lb), 2), "-", round(exp(data$pci_ub), 2),
                                   ")")
  return(data)
}




