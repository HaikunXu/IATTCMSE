#' Update OM's data file with "true" data
#'
#' @param dir_OM the directory of OM
#' @param dir_OM_Boot the directory of the OM bootstrap in the current step
#' @param Mcycle the number of years within a management cycle
#' @param dat_name name of the SS data file
#' 
#' @author Haikun Xu 
#' @export

Update_OM_SMSY = function(istep, dir_OM_Final, dir_OM_MSY, dat_name, ss_name, ctl_name) {
  
  dir.create(dir_OM_MSY)
  
  files = c(
    paste0(dir_OM_Final, ctl_name),
    paste0(dir_OM_Final, dat_name),
    paste0(dir_OM_Final, ss_name),
    paste0(dir_OM_Final, "go_nohess.bat"),
    paste0(dir_OM_Final, "ss3.par")
  )
  file.copy(from = files, to = dir_OM_MSY, overwrite = TRUE)
  
  # change par file
  ParDir <- paste0(dir_OM_MSY, "ss3.par")
  ParFile <- readLines(ParDir, warn = F)
  
  Rep <- r4ss::SS_output(
    dir = dir_OM_Final,
    covar = F,
    printstats = F,
    verbose = FALSE
  )
  
  Recruit <- Rep$recruit$dev[which(Rep$recruit$era %in% c("Main", "Late"))]
  bias_adjust <- -Rep$recruit$biasadjuster[which(Rep$recruit$era %in% c("Main", "Late"))] * 0.6 ^ 2 / 2
  Recruit_late <- Rep$recruit$dev[which(Rep$recruit$era == "Late")]
  
  Recruit_forecast <- c(Recruit_late, Recruit + bias_adjust, 0)
  
  Line <- match("# Fcast_recruitments:", ParFile)
  # Line_error <- match("# Fcast_impl_error:", ParFile)
  ParFile[Line + 1] <- gsub(",", "", toString(Recruit_forecast))
  # ParFile[Line_error + 1] <- gsub(",", "", toString(Recruit_forecast * 0.0))
  writeLines(ParFile, paste0(dir_OM_MSY, "/ss3.par"))
  
  # change forecast file
  Forecast <- r4ss::SS_readforecast(paste0(dir_OM_Final, "forecast.ss"), verbose = FALSE)
  Forecast$MSY <- 2 # calculate FMSY
  Forecast$Nforecastyrs <- length(Recruit) + 1 # change forecast No. of recruitment
  Forecast$ControlRuleMethod <- 0 # Harvest control rule method
  Forecast$First_forecast_loop_with_stochastic_recruitment <- 1
  r4ss::SS_writeforecast(Forecast, dir_OM_MSY, verbose = FALSE, overwrite = TRUE)
  
  # change starter file
  starter <- r4ss::SS_readstarter(paste0(dir_OM_Final, "starter.ss"), verbose = FALSE)
  starter$F_std_basis <- 122
  r4ss::SS_writestarter(starter, dir_OM_MSY, verbose = FALSE, overwrite = TRUE)
  
  # run the OM
  command <- paste("cd", dir_OM_MSY, "& go_nohess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  om_out = r4ss::SS_output(dir = dir_OM_MSY, covar = F, verbose = FALSE, printstats = FALSE)
  TS <- om_out$timeseries[which(om_out$timeseries$Era == "FORE"), ]
  SMSY <- data.frame("Yr" = TS$Yr, "SMSY" = TS$SpawnBio)
  
  return(SMSY)
}