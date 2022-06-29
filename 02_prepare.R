# Prepare original data to host on r package
dd <- read.csv("../meta_analysis_20220203_example_code/input/Aim2_UDS_logistic_model_Either_UDS_v2.csv")

source("helper_data_prep.R")
source("helper_figures.R")

dd <- rename_site(dd, "State")

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
#info_columns <- unique(dd[, c("order", "Variable", "ClassVal0", "Parameter", "Exclude_from_forestplot")]) # save the info columns
info_columns <- unique(dd[,c("order", "Variable", "Parameter")])
p <- nrow(info_columns)
dd <- dd[order(dd$site, dd$order), ] # ORDER by state abbreviation, then by the parameter display order

#site, Parameter, Estimate, StdErr
dd_out <- dd%>%
  select(site, order, Parameter, Estimate, StdErr)
  #mutate(Parameter = paste("param_",Parameter, sep=""), site = paste("site_", site, sep = ""))

dd_out_anon <- dd_out%>%
  mutate(Parameter = paste("param_",order, sep=""))

saveRDS(dd_out_anon, file = "data/data.rds")

