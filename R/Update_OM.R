#' Update OM's data file with "true" data
#'
#' @param dir_OM the directory of OM
#' @param dir_OM_Boot the directory of the OM bootstrap in the current step
#' @param Mcycle the number of years within a management cycle
#' @param dat_name name of the SS data file
#' 
#' @author Haikun Xu 
#' @export

Update_OM = function(dir_OM, dir_OM_Boot, dir_OM_Final, Mcycle, dat_name, ss_name) {
  
  # change data file
  dat_OM <- r4ss::SS_readdat_3.30(file = paste0(dir_OM, dat_name), verbose = FALSE)
  dat_OM_Boot <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Boot, dat_name), verbose = FALSE)
  data_true <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Boot, "data_expval.ss"), verbose = FALSE)
  
    # add new CPUE
  CPUE <- dat_OM$CPUE
  CPUE_true <- data_true$CPUE[(nrow(CPUE)+1):nrow(data_true$CPUE),]
  CPUE_new <- rbind(CPUE, CPUE_true)
  
  # add new LF
  LF <- dat_OM$sizefreq_data_list[[1]]
  LF_true <- dplyr::filter(data_true$sizefreq_data_list[[1]], year > max(LF$year))
  LF_new <- dplyr::arrange(rbind(LF, LF_true), fleet, year)
  
  # save data file
  dat_OM$catch <- dat_OM_Boot$catch
  dat_OM$CPUE <- CPUE_new
  dat_OM$sizefreq_data_list[[1]] <- LF_new
  dat_OM$Nobs_per_method <- nrow(LF_new)
  dat_OM$endyr <- dat_OM$endyr + Mcycle * 4
  r4ss::SS_writedat_3.30(dat_OM, paste0(dir_OM_Boot, dat_name), verbose = FALSE, overwrite = TRUE)
  
  # run the OM to get current F
  dir.create(dir_OM_Final)
  
  # copy files to the new folder
  files = c(
    paste0(dir_OM, "starter.ss"),
    paste0(dir_OM, ss_name),
    paste0(dir_OM, "go_nohess.bat"),
    paste0(dir_OM_Boot, dat_name)
  )
  file.copy(from = files, to = dir_OM_Final, overwrite = TRUE)
  
  # read data file
  dat <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Final, dat_name), verbose = FALSE)
  
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
    outfile = paste0(dir_OM_Final, ctl_name),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  # change forecast file
  Forecast <- r4ss::SS_readforecast(paste0(dir_OM, "forecast.ss"), verbose = FALSE)
  Forecast$Nforecastyrs <- 1
  r4ss::SS_writeforecast(Forecast, dir_OM_Final, verbose = FALSE, overwrite = TRUE)
  
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
  
  writeLines(ParFile, paste0(dir_OM_Final, "/ss3.par"))
  
  # starter file
  starter <- r4ss::SS_readstarter(paste0(dir_OM_Final, "starter.ss"), verbose = FALSE)
  
  #specify to use the ss3.par as parameters
  starter$init_values_src = 1
  #turn off estimation of parameters 
  starter$last_estimation_phase = 0
  #
  starter$maxyr_sdreport <- istep * Mcycle * 4 + endquarter + 1
  
  #write new starter file
  r4ss::SS_writestarter(starter, dir_OM_Final, verbose = FALSE, overwrite = TRUE)
  
  # run the OM
  command <- paste("cd", dir_OM_Final, "& go_nohess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  return()

}