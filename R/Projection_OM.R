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

Projection_OM = function(pdir, HS, HCR, OM, itr, istep, Fscale, dir_OM_previous, dir_EM_previous, R_devs, n_extra_R, Mcycle, plot = NA) {
  
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
  Forecast <- r4ss::SS_readforecast(paste0(dir_OM_previous, "forecast.ss"), verbose = FALSE)
  
  Forecast$F_scalar <- Fscale # Fscale # input F scaler
  Forecast$Forecast <- 5 # use F scaler
  Forecast$Nforecastyrs <- 1 + Mcycle * 4 # number of forecast years
  Forecast$ControlRuleMethod <- 0 # Harvest control rule method
  Forecast$First_forecast_loop_with_stochastic_recruitment <- 1

  r4ss::SS_writeforecast(Forecast, dir_OM, verbose = FALSE, overwrite = TRUE)

  
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
  
  
  # plot
  if(sum(is.na(plot)) == 0) {
    om_out = r4ss::SS_output(dir = dir_OM, covar = F, verbose = FALSE, printstats = FALSE)
    r4ss::SS_plots(replist=om_out, uncertainty=F, datplot=T, plot = plot, forecastplot = TRUE, verbose = FALSE)
  }
  
  return(list("dir_istep" = dir_istep, "dir_OM" = dir_OM))
  
}