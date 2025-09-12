#' Bootstrap observed data for the EM
#'
#' @param dir_istep the directory of step i
#' @param istep step number
#' @param dir_OM the directory of OM
#' @param Mcycle the number of years within a management cycle
#' @param EM_comp_fleet The fleets with comp data in the EM
#' @param seed the seed for bootstrap
#' @param endquarter the last quarter of the assessment model
#' @param dat_name name of the SS data file
#' @param ctl_name name of the SS control file
#' @param ss_name name of the SS exe file
#'  
#' @author Haikun Xu 
#' @export

Bootstrap_OM = function(dir_istep, istep, dir_OM, Mcycle, EM_comp_fleet, seed, endquarter, dat_name, ctl_name, ss_name) {
  
  # step 1: create a new folder for the OM bootstrap
  dir_OM_Boot <- paste0(dir_istep, "OM_Boot/")
  dir.create(dir_OM_Boot)
  
  # copy files to the new folder
  files = c(
    paste0(dir_OM, "starter.ss"),
    paste0(dir_OM, ss_name),
    paste0(dir_OM, "go_nohess.bat")
  )
  file.copy(from = files, to = dir_OM_Boot, overwrite = TRUE)
  
  # read the report file from the OM projection
  om_out = r4ss::SS_output(dir = dir_OM, covar = F, verbose = FALSE, printstats = FALSE)
  
  # read projected catch
  Catch_projection <- r4ss::SS_ForeCatch(om_out,
                                         yrs = ((istep - 1) * Mcycle * 4 + endquarter + 1):(istep * Mcycle * 4 + endquarter),
                                         zeros = TRUE)
  
  # read catch file
  dat <- r4ss::SS_readdat_3.30(file = paste0(dir_OM, dat_name), verbose = FALSE)
  Catch <- dat$catch
  
  Catch_projection_new <- data.frame("year" = Catch_projection$`#Year`,
                                     "seas" = Catch_projection$Seas,
                                     "fleet" = Catch_projection$Fleet,
                                     "catch" = Catch_projection$`dead(B)`,
                                     "catch_se" = 0.01)
  
  Catch_new <- dplyr::arrange(rbind(Catch, Catch_projection_new), fleet, year)
  
  dat$catch <- Catch_new
  dat$endyr <- istep * Mcycle * 4 + endquarter
  
  # add dummy CPUE data
  CPUE_new <- dat$CPUE[(nrow(dat$CPUE) - Mcycle * 4 + 1):nrow(dat$CPUE),]
  CPUE_new$year <- CPUE_new$year + Mcycle * 4
  CPUE_new$se_log <- mean(dat$CPUE$se_log[(nrow(dat$CPUE)-3):nrow(dat$CPUE)])
  dat$CPUE <- rbind(dat$CPUE, CPUE_new)
  
  # add dummy LF data
  LF <- dat$sizefreq_data_list[[1]] # [which(dat$sizefreq_data_list$)]
  LF_new <- LF[which((LF$year %in% 169:180) & LF$fleet %in% EM_comp_fleet),] # no LL LF during COVID years so use pre-COVID samples
  LF_new$year <- LF_new$year + istep * Mcycle * 4 + 16 # 4 COVID-years (2020-2023)
  dat$sizefreq_data_list[[1]] <- rbind(LF, LF_new)
  dat$Nobs_per_method <- nrow(dat$sizefreq_data_list[[1]])

  r4ss::SS_writedat_3.30(dat, paste0(dir_OM_Boot, dat_name), verbose = FALSE, overwrite = TRUE)
  
  # change recruitment period in the control file
  ctl <- r4ss::SS_readctl_3.30(
    file = paste0(dir_OM, ctl_name),
    verbose = FALSE,
    datlist = dat,
    use_datlist = TRUE
  )
  ctl$MainRdevYrLast <- ctl$MainRdevYrLast + Mcycle * 4 # increase the main recruitment last year
  
  r4ss::SS_writectl_3.30(
    ctl,
    outfile = paste0(dir_OM_Boot, ctl_name),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  
  # change forecast file
  Forecast <- r4ss::SS_readforecast(paste0(dir_OM, "forecast.ss"), verbose = FALSE)
  Forecast$Nforecastyrs <- 1
  r4ss::SS_writeforecast(Forecast, dir_OM_Boot, verbose = FALSE, overwrite = TRUE)
  

  # change recruitment in the par file
  ParDir <- paste0(dir_OM, "ss3.par")
  ParFile <- readLines(ParDir, warn = F)
  
  Line_main <- match("# recdev2:", ParFile)
  R_main <- read.table(
    file = ParDir,
    nrows = 1,
    skip = Line_main
  )
  
  Line_forecast <- match("# Fcast_recruitments:", ParFile)
  R_forecast <- read.table(
    file = ParDir,
    nrows = 1,
    skip = Line_forecast
  )
  
  R_main_new <- cbind(R_main, R_forecast[1,1:(Mcycle*4)])
  R_forecast_new <- R_forecast[1,(Mcycle*4+1):ncol(R_forecast)]
  
  ParFile[Line_main + 1] <- gsub(",", "", toString(R_main_new))
  ParFile[Line_forecast + 1] <- gsub(",", "", toString(R_forecast_new))
  
  writeLines(ParFile, paste0(dir_OM_Boot, "/ss3.par"))
  
  # set up bootstrap
  starter_boot <- r4ss::SS_readstarter(paste0(dir_OM_Boot, "starter.ss"), verbose = FALSE)
  
  # specify to use the ss3.par as parameters
  starter_boot$init_values_src = 1
  # turn off estimation of parameters 
  starter_boot$last_estimation_phase = 0
  # add 1 data bootstrap file
  starter_boot$N_bootstraps = 3
  # add the seed
  starter_boot$seed <- seed
  
  # write new starter file
  r4ss::SS_writestarter(starter_boot, dir_OM_Boot, verbose = FALSE, overwrite = TRUE)
  

  # run the bootstrap model
  command <- paste("cd", dir_OM_Boot, "& go_nohess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  return(dir_OM_Boot)
}