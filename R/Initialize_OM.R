#' Initialize the OM based on the benchmark assessment model
#' 
#' @param pdir parent directory path
#' @param sdir the directory of the assessment models with F30% reference points
#' @param HS the name of the harvest strategy
#' @param HCR the name of the harvest control rule
#' @param OM name of the reference model
#' @param dat_name name of the SS data file
#' @param ctl_name name of the SS control file
#' @param ss_name name of the SS exe file
#' 
#' @author Haikun Xu 
#' @export

Initialize_OM = function(pdir, sdir, HS, HCR, OM, dat_name, ctl_name, ss_name) {
  
  # *************************************************************************************
  # Step 0: get the OM from the 2024 assessment
  # *************************************************************************************
  
  # first get the OM from the 2024 benchmark assessment with F30% reference points
  dir_OM_benchmark <- paste0(sdir, OM)
  dir_OM_MSE <- paste0(pdir, HS, HCR, OM, "itr0/")
  dir.create(dir_OM_MSE)

  files = c(
    paste0(dir_OM_benchmark, ctl_name),
    paste0(dir_OM_benchmark, dat_name),
    paste0(dir_OM_benchmark, ss_name),
    paste0(dir_OM_benchmark, "go_nohess.bat"),
    paste0(sdir, "forecast.ss"),
    paste0(sdir, "starter.ss"),
    paste0(dir_OM_benchmark, "ss3.par"),
    paste0(pdir, "CLEAN.BAT")
  )
  file.copy(from = files, to = dir_OM_MSE, overwrite = TRUE)
  
  # run the estimation model
  command <- paste("cd", dir_OM_MSE, "& go_nohess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  # clean unused files in this folder to save storage space
  command <- paste("cd", dir_OM_MSE, "& CLEAN.BAT", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  return(ss)
}