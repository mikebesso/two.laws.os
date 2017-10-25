#' @include MacOS.R
#' @include WinOS.R
#'
NULL


#' @export
CreateOS <- function(verbose = FALSE){

  OSName <- Sys.info()[["sysname"]]

  OS <- switch(
    OSName,
    Darwin = MacOS$new(verbose = verbose),
    Windows = WinOS$new(verbose = verbose),
    NULL
  )

  if(is.null(OS)){
    stop(OSName, " not yet supported.")
  }

  return(OS)
}




TwoLawsOSClass <- R6Class(
  "two.laws.os.project",
  inherit = ProjectBaseClass,
  public = list(

    initialize = function(verbose = FALSE, testthat = FALSE){
      super$initialize(verbose = verbose)
    }
  )
)

  ProjectBaseClass




if (!devtools::uses_testthat()){
  devtools::use_testthat()
}
