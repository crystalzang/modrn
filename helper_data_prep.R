#Example
#get_variable_names(ggplotdata)
get_variable_names <- function(df){
  variable <- as.character(pull(df, variables))
  return(variable)
}

# this function is used in helper_figure.R
# generates estimates for each variable for each site
generate_var_lists <- function(df, i_level){
  out_ls <- list()
  dat_ls <- list()
  metahksj_ls <- list()
  order <- pull(df,order)[!is.na(pull(df,order))]
  variable <- as.character(pull(df, variables))
  p <- max(order)
  
  for(i in 1:p){
    dd_i <- dd[dd$order == i, ] # data of the corresponding parameter
    estimate <- dd_i[, "Estimate"]
    sderr <- dd_i[, "StdErr"]
    state <- dd_i[, "State"]
    keep <- (sderr != 0) & (!is.na(estimate)) # check if stderr = 0 or estimate = NA, exclude
    # random effect meta-analysis by the Hartung-Knapp-Sidik-Jonkman method
    # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-25
    dat <- data.frame(yi = estimate[keep], vi = sderr[keep]^2, state = state[keep])
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
#data <- clean_data(dd, "State")

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

dd_out <- generate_global_estimates(dd, 0.95, 0.90)
data = dd
cl = 0.95
pcl = 0.9
generate_global_estimates <- function(data, cl, pcl){
  info_columns <- unique(data[, c("order",  "Parameter")]) # save the info columns
  
  
  p = length(unique(data$order)) # order is variable
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
  
  ### output dataframe: outdata
  outdata <- data.frame(info_columns, 
                        estimate = result_est,
                        se = result_se,
                        ci_lb = result_ci_lb,
                        ci_ub = result_ci_ub,
                        OR_estimate = exp(result_est),
                        OR_ci_lb = exp(result_ci_lb),
                        OR_ci_ub = exp(result_ci_ub),
                        pvalue = result_pval, 
                        tau = sqrt(result_tau2),
                        pci_lb = result_pci_lb,
                        pci_ub = result_pci_ub,
                        OR_pci_lb = exp(result_pci_lb),
                        OR_pci_ub = exp(result_pci_ub),
                        Isquare = result_isq, CochranQ = result_Q, 
                        CochranQpvalue = result_Qp, 
                        range_lb = result_est_min,
                        range_ub = result_est_max)
  
  return(outdata)
}
