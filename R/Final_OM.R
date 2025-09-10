#' Make the final OM for extracting MSE results
#' 
#' @param pdir parent directory path
#' @param dir_itr the directory of iteration itr
#' @param istep step number
#' @param dir_OM the directory of OM
#' @param dir_OM_Boot the directory of the OM bootstrap in the current step
#' @param Mcycle the number of years within a management cycle
#' @param endquarter the last quarter of the assessment model
#' @param dat_name name of the SS data file
#' @param ctl_name name of the SS control file
#' @param ss_name name of the SS exe file
#' @param clean if TRUE, all intermediate folders of the MSE simulation will be deleted to save storage space
#' 
#' @author Haikun Xu 
#' @export

Final_OM = function(pdir, dir_itr, istep, dir_OM, dir_OM_Boot, Mcycle, endquarter, dat_name, ctl_name, ss_name, clean) {
  
  # step 1: create a new folder for the OM bootstrap
  dir_OM_Final <- paste0(dir_itr, "OM_Final/")
  dir.create(dir_OM_Final)
  
  # copy files to the new folder
  files = c(
    paste0(dir_OM, "starter.ss"),
    paste0(dir_OM, ss_name),
    paste0(dir_OM, "go_nohess.bat"),
    paste0(dir_OM_Boot, dat_name),
    paste0(pdir, "CLEAN.BAT")
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
  
  if(clean == TRUE) {
    # clean unused files in this folder to save storage space
    command <- paste("cd", dir_OM_Final, "& CLEAN.BAT", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
  }
  
  return(dir_OM_Final)
}