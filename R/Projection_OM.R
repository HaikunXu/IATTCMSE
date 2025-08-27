#' Make projection based on simulated R devs and HCR
#'
#' @param pdir parent directory path
#' @param HS the name of the harvest strategy
#' @param HCR the name of the harvest control rule
#' @param OM the name of the operating model
#' @param itr iteration number
#' @param istep step number
#' @param Fscale input F scaler 
#' @param dir_OM_previous the directory of the OM in the previous step
#' @param dir_EM_previous the directory of the EM in the previous step
#' @param R_devs simulated R devs
#' 
#' @author Haikun Xu 
#' @export

Projection_OM = function(pdir, HS, HCR, OM, itr, istep, Fscale, dir_OM_previous, dir_EM_previous, R_devs) {
  
  # create directory for new time step where the new dat file will be saved
  dir_istep <- paste0(pdir, HS, HCR, OM, itr, "step", istep, "/")
  dir.create(dir_istep)
  
  # step 1: create a new folder for the OM
  dir_OM <- paste0(dir_istep, "OM/")
  dir.create(dir_OM)
  
  files = c(
    paste0(dir_OM_previous, "BET-EPO.ctl"),
    paste0(dir_EM_previous, "BET-EPO_all.dat"),
    paste0(dir_OM_previous, "ss.exe"),
    paste0(dir_OM_previous, "go_nohess.bat")
  )
  file.copy(from = files, to = dir_OM, overwrite = TRUE)
  
  file.rename(from = paste0(dir_OM, "BET-EPO_all.dat"), to = paste0(dir_OM, "BET-EPO.dat")) # load the full dataset
  
  # step 2: change par file
  ParDir <- paste0(dir_OM_previous, "ss3.par")
  ParFile <- readLines(ParDir, warn = F)
  
  Line <- match("# Fcast_recruitments:", ParFile)
  
  R_forecast <- read.table(
    file = ParDir,
    nrows = 1,
    skip = Line
  )
  
  if(n_extra_R == 0)
    R_forecast_new <- c(R_devs[((istep-1)*Mcycle*4+1):(istep*Mcycle*4)], 0)
  else 
    R_forecast_new <- c(as.vector(t(R_forecast[1,1:n_extra_R])), 
                        R_devs[((istep-1)*Mcycle*4+1):(istep*Mcycle*4)],
                        0)
  
  ParFile[Line + 1] <- gsub(",", "", toString(R_forecast_new)) # do R bias adjustment???
  writeLines(ParFile, paste0(dir_OM, "/ss3.par"))
  
  # step 3: change forecast file
  ForecastDir <- paste0(dir_OM_previous, "forecast.ss")
  ForecastFile <- readLines(ForecastDir, warn = F)
  
  ForecastFile[12] <- 5 # use F scaler
  ForecastFile[14] <- Fscale # input F scaler
  ForecastFile[13] <- 1 + Mcycle * 4 # number of forecast years
  
  writeLines(ForecastFile, paste0(dir_OM, "/forecast.ss"))
  
  
  # step 4: change starter file
  starter <- r4ss::SS_readstarter(paste0(dir_EM_previous, "/starter.ss"), verbose = FALSE)
  
  #specify to use the ss3.par as parameters
  starter$init_values_src = 1
  #turn off estimation of parameters 
  starter$last_estimation_phase = 0
  
  #write new starter file
  r4ss::SS_writestarter(starter, dir_OM, verbose = FALSE, overwrite = TRUE)
  
  
  # step 5: run ss
  # setwd(dir_OM)
  command <- paste("cd", dir_OM, "& go_nohess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  return(list("dir_istep" = dir_istep, "dir_OM" = dir_OM))
  
}