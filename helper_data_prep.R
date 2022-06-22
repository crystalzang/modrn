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
data = dd
site = "State"

clean_data <- function(data, site){
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
  
}

generate_global_estimates <- function(data, cl){
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
    data_i <- data[data$order == i, ] # data of the corresponding parameter
    estimate <- data_i[, "Estimate"]
    sderr <- data_i[, "StdErr"]
    site <- data_i[, "Site"]
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
  
  
}
