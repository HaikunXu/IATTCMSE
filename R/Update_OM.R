#' Make projection based on simulated R devs and HCR
#'
#' @param dir_istep the directory of step i
#' @param R0 true R0
#' @param dir_EM_previous the directory of the EM in the previous step
#' @param dir_OM_Boot the directory of the OM bootstrap in the current step
#'  
#' @author Haikun Xu 
#' @export

Update_OM = function(dir_OM, dir_OM_Boot, Mcycle) {
  
  # change data file
  dat_OM <- r4ss::SS_readdat_3.30(file = paste0(dir_OM, "BET-EPO.dat"), verbose = FALSE)
  data_true <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Boot, "data_expval.ss"), verbose = FALSE)
  
  # add new CPUE
  CPUE <- dat_OM$CPUE
  CPUE_boot <- data_true$CPUE[(nrow(CPUE)+1):nrow(data_true$CPUE),]
  CPUE_new <- rbind(CPUE, CPUE_boot)
  
  # add new catch
  catch <- dat_OM$catch
  catch_boot <- data_true$catch[which(data_true$catch$year>max(catch$year)),]
  catch_new <- dplyr::arrange(rbind(catch, catch_boot), fleet, year)
  
  # add new LF
  LF <- dat_OM$sizefreq_data_list[[1]]
  LF_boot <- dplyr::filter(data_true$sizefreq_data_list[[1]], year > max(LF$year))
  LF_new <- dplyr::arrange(rbind(LF, LF_boot), fleet, year)
  
  # save data file
  dat_OM$catch <- catch_new
  dat_OM$CPUE <- CPUE_new
  dat_OM$sizefreq_data_list[[1]] <- LF_new
  dat_OM$Nobs_per_method <- nrow(LF_new)
  dat_OM$endyr <- dat_OM$endyr + Mcycle * 4
  r4ss::SS_writedat_3.30(dat_OM, paste0(dir_OM_Boot, "BET-EPO.dat"), verbose = FALSE, overwrite = TRUE)
  
  return()

}