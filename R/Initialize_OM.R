#' Initialize the OM based on the benchmark assessment model
#'
#' @param sdir the directory of the assessment models with F30% reference points
#' @param OM name of the reference model
#' 
#' @author Haikun Xu 
#' @export

Initialize_OM = function(sdir, OM) {
  
  # *************************************************************************************
  # Step 0: get the OM from the 2024 assessment
  # *************************************************************************************
  
  # first get the OM from the 2024 benchmark assessment with F30% reference points
  dir_OM_benchmark <- paste0(sdir, OM)
  dir_OM_MSE <- paste0(pdir, HS, HCR, OM, "itr0/")
  dir.create(dir_OM_MSE)

  files = c(
    paste0(dir_OM_benchmark, "BET-EPO.ctl"),
    paste0(dir_OM_benchmark, "BET-EPO.dat"),
    paste0(dir_OM_benchmark, "ss.exe"),
    paste0(dir_OM_benchmark, "go_nohess.bat"),
    paste0(sdir, "forecast.ss"),
    paste0(sdir, "starter.ss"),
    paste0(dir_OM_benchmark, "ss3.par")
  )
  file.copy(from = files, to = dir_OM_MSE, overwrite = TRUE)

  ParDir <- paste0(dir_OM_MSE, "ss3.par")
  ParFile <- readLines(ParDir, warn = F)
  
  Line_R0 <- match("# SR_parm[1]:", ParFile)
  R0 <- as.numeric(ParFile[Line_R0 + 1])
  
  return(list("R0" = R0))
}