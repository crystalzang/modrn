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
