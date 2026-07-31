### function to run denominator and numerator of stabilized censoring weights

## inputs: dataset, wave year, censor type (death or ltfu), cancer type (breast colorectal lung endo)
long_sl_denom <- function(data, wave, censor_type, cancer_type) {

  lrnr_mean <- Lrnr_mean$new()
  lrnr_glm <- Lrnr_glm$new()
  lrnr_earth <- Lrnr_earth$new()
  lrnr_ridge <- Lrnr_glmnet$new(alpha = 0)
  lrnr_lasso <- Lrnr_glmnet$new(alpha = 1)
  lrnr_rf_50 <- Lrnr_randomForest$new(ntree = 50)
  lrnr_ranger_50 <- Lrnr_ranger$new(ntree = 50)
  lrnr_polspline <- Lrnr_polspline$new(cv = 3)

  stack <- Stack$new(
    lrnr_glm, lrnr_mean,
    lrnr_earth,
    lrnr_ridge, lrnr_lasso,
    lrnr_rf_50, lrnr_ranger_50, lrnr_polspline
  )

  sl <- Lrnr_sl$new(learners = stack)

  outcome = paste0("censor_", censor_type)

  covariates_base <- c("ccstat", "ageatindex",
                                         # study specific
                                         #"ctflag",
                                          #'hrtarm', 'dmarm', 'cadarm', 'bmdflag',
                                         #demographics
                                         'racenih_cat', 'ethnicnih_cat', 'educ_impute2',
                                         'income_impute', 'region_cat', 'marital_impute',
                                         #'anyins_impute',
                                         'medicaid_impute',
                                         # #medical history
                                         "mi_preindex", "angina_preindex", "diabtx_preindex", "parkinsons_preindex", "hyst_preindex",
                                         #repro history
                                         'menarche_impute', 'gravid_impute',
                                         'agefbir_impute', 'booph_impute',
                                         'brstfdmo_impute', 'brstdis_impute',
                                         'meno_impute',
                                         #family medical history
                                         "mirel_impute",
                                         'cancmrel_impute',
                                         'cancfrel_impute',
                                         "strkrel_impute",
                                         "bkbonrel_impute",
                                         "diabrel_impute",
                                         #lifestyle
                                         'smoking_impute',
                                         'alcswk_impute',
                                         'socsupp_impute',
                                         'texpwk_impute',
                                         'ahei_final',

                                         # clinical characteristics
                                         'syst_impute', 'dias_impute', 'bmi_impute',

                                         # cancer characteristics
                                         'stage_cat', 'surgery_cat', 'chemo_cat', 'radiation_cat',
                                         'size_bin', 'grading_cat', 'lymph_involvement'
                                         #'stage_cat:surgery_cat', 'stage_cat:chemo_cat', 'stage_cat:radiation_cat'
  )#, 'extension_cat')#, 'poslymph_cat', 'numlymph_cat')

    covs_later <- c(covariates_base, "last_genhel", "last_physfun")

    breast_covs_base <- c(covariates_base, 'endocrine_cat',  'erassay_cat', 'prassay_cat', 'her2neu_cat')
    breast_covs_later <- c(covs_later, 'endocrine_cat', 'erassay_cat', 'prassay_cat', 'her2neu_cat')

    if (cancer_type == "breast" & wave == 0) {
      covs <- breast_covs_base
    } else if(cancer_type == "breast" & wave > 0){
      covs <- breast_covs_later
    } else if(cancer_type != "breast" & wave == 0) {
      covs <- covariates_base
    } else {
      covs <- covs_later
    }

  task <- make_sl3_Task(
    data = data,
    outcome = outcome,
    covariates = covs#,
    #id = "setnumber"
  )

  set.seed(1715)
  start_time <- proc.time() # start time
#
  ncores <- 10
  plan(multisession, workers = ncores) # multisession instead of multicore

  sl_fit <- sl$train(task = task)
  sl_pred <- sl_fit$predict(task = task)

  runtime_sl_fit <- proc.time() - start_time # end time - start time = run time
  print(runtime_sl_fit)

  return_list <- list(data$commonid, data$setnumber, sl_pred, sl_fit)

  #return_dat <- as.data.frame(cbind(data$commonid, data$setnumber, wave, sl_pred))
  names(return_list)[1] = "commonid"
  names(return_list)[2] = "setnumber"
  names(return_list)[3] = paste0("denom_preds_wv", wave)
  names(return_list)[4] =  paste0("sl_fit_denom_wv", wave)

  return(return_list)
}

long_sl_num <- function(data, wave, censor_type, cancer_type) {

  lrnr_interact <- Lrnr_define_interactions$new(
    list( c("stage_cat", "surgery_cat"))
  )

  lrnr_glm <- Lrnr_glm$new()
  lrnr_mean <- Lrnr_mean$new()
  lrnr_earth <- Lrnr_earth$new()
  lrnr_ridge <- Lrnr_glmnet$new(alpha = 0)
  lrnr_lasso <- Lrnr_glmnet$new(alpha = 1)
  lrnr_rf_50 <- Lrnr_randomForest$new(ntree = 50)
  lrnr_ranger_50 <- Lrnr_ranger$new(ntree = 50)
  lrnr_polspline <- Lrnr_polspline$new(cv = 3)

  stack <- Stack$new(
    lrnr_glm,
    lrnr_earth,
    lrnr_ridge, lrnr_lasso,
   lrnr_rf_50, lrnr_ranger_50, lrnr_polspline
  )


  sl <- Lrnr_sl$new(learners = stack)

  outcome = paste0("censor_", censor_type)

  covariates_base = c("ccstat",
                      "ctflag",
                      #demographics
                      "ageatindex", 'racenih_cat', 'ethnicnih_cat', 'educ_impute2',
                      'income_impute', 'income_impute_flag', 'region_cat', 'marital_impute',
                      #'anyins_impute',
                      'medicaid_impute'
                      #'bmi_impute',
                      # cancer characteristics
                      #'stage_cat'
                      )

  covs_later <- c(covariates_base)#, "last_genhel_cat", "last_physfun")


  if (wave == 0) {
    covs <- covariates_base
  } else {
    covs <- covs_later
  }

  task <- make_sl3_Task(
    data = data,
    outcome = outcome,
    covariates = covs
  )
  set.seed(1715)
  start_time <- proc.time() # start time

  ncores <- 10
  plan(multisession, workers = ncores)

  sl_fit <- sl$train(task = task)
  sl_pred <- sl_fit$predict(task = task)

  runtime_sl_fit <- proc.time() - start_time # end time - start time = run time
  print(runtime_sl_fit)

  return_list <- list(data$commonid, data$setnumber, sl_pred, sl_fit)

  #return_dat <- as.data.frame(cbind(data$commonid, data$setnumber, wave, sl_pred))
  names(return_list)[1] = "commonid"
  names(return_list)[2] = "setnumber"
  names(return_list)[3] = paste0("num_preds_wv", wave)
  names(return_list)[4] =  paste0("sl_fit_num_wv", wave)

  return(return_list)
}
