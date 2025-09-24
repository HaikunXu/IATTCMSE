#' Make projection based on simulated R devs and HCR
#'
#' @param dir_istep the directory of step i
#' @param R0 true R0
#' @param dir_EM_previous the directory of the EM in the previous step
#' @param dir_OM_Boot the directory of the OM bootstrap in the current step
#' @param Mcycle the number of years within a management cycle
#' @param dat_name name of the SS data file
#' @param ctl_name name of the SS control file
#' @param ss_name name of the SS exe file
#' @param plot whether to plot EM's results
#' @param from_par whether to start the EM from the par file
#' @param include_LF whether to fit the EM to bootstrapped LF data
#' 
#' @author Haikun Xu 
#' @export

Estimation_EM = function(dir_istep, R0, dir_EM_previous, dir_OM_Boot, Mcycle, dat_name, ctl_name, ss_name, plot = FALSE, include_LF = TRUE) {
  
  # step 1: create a new folder for the EM
  dir_EM <- paste0(dir_istep, "EM/")
  dir.create(dir_EM)
  
  # copy files to the new folder
  files = c(
    paste0(pdir, ss_name),
    paste0(dir_EM_previous, "go_nohess.bat"),
    paste0(dir_EM_previous, "forecast.ss"),
    paste0(dir_EM_previous, "starter.ss")
  )
  file.copy(from = files, to = dir_EM, overwrite = TRUE)
  
  # change data file
  dat_EM_previous <- r4ss::SS_readdat_3.30(file = paste0(dir_EM_previous, dat_name), verbose = FALSE)
  data_boot <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Boot, "data_boot_001.ss"), verbose = FALSE)
  
  # add new CPUE
  CPUE <- dat_EM_previous$CPUE
  CPUE_boot <- data_boot$CPUE[(nrow(CPUE)+1):nrow(data_boot$CPUE),]
  CPUE_new <- rbind(CPUE, CPUE_boot)
  
  # add new catch
  catch <- dat_EM_previous$catch
  catch_boot <- data_boot$catch[which(data_boot$catch$year>max(catch$year)),]
  catch_new <- dplyr::arrange(rbind(catch, catch_boot), fleet, year)
  
  # add new LF
  LF <- dat_EM_previous$sizefreq_data_list[[1]]
  LF_boot <- dplyr::filter(data_boot$sizefreq_data_list[[1]], year > max(LF$year))
  LF_new <- dplyr::arrange(rbind(LF, LF_boot), fleet, year)
  
  # save data file
  dat_EM_previous$catch <- catch_new
  dat_EM_previous$CPUE <- CPUE_new
  if(include_LF == TRUE) {
    dat_EM_previous$sizefreq_data_list[[1]] <- LF_new
    dat_EM_previous$Nobs_per_method <- nrow(LF_new)
  }
  dat_EM_previous$endyr <- dat_EM_previous$endyr + Mcycle * 4
  r4ss::SS_writedat_3.30(dat_EM_previous, paste0(dir_EM, dat_name), verbose = FALSE, overwrite = TRUE)
  
  # change control file
  ctl <- r4ss::SS_readctl_3.30(
    file = paste0(dir_EM_previous, ctl_name),
    verbose = FALSE,
    datlist = dat_EM_previous,
    use_datlist = TRUE
  )
  ctl$MainRdevYrLast <- ctl$MainRdevYrLast + Mcycle * 4 # increase the main recruitment last year
  
  ctl$SR_parms$INIT[1] <- R0 + 0.25 # adding 0.25 makes the EM more likely to converge
  
  r4ss::SS_writectl_3.30(
    ctl,
    outfile = paste0(dir_EM, ctl_name),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  starter <- r4ss::SS_readstarter(paste0(dir_EM, "/starter.ss"), verbose = FALSE)
  
  #specify to not use the ss3.par as parameters
  starter$init_values_src = 0
  
  #write new starter file
  r4ss::SS_writestarter(starter, dir_EM, verbose = FALSE, overwrite = TRUE)
  
  # run the estimation model
  command <- paste("cd", dir_EM, "& go_nohess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  if(plot == TRUE) {
    em_out = r4ss::SS_output(dir = dir_EM, covar = F, verbose = FALSE, printstats = FALSE)
    r4ss::SS_plots(replist = em_out, uncertainty = F, datplot = T, verbose = FALSE)
  }
  
  return(dir_EM)

}