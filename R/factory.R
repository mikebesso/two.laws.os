#' @include MacOS.R
#' @include WinOS.R
#' @include WinOS.R
#' @include MacOS.R
#' NULL


#' @export
#'
CreateOS <- function(verbose = TRUE){

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

# A closure that creates the OS singleton factory
#
CreateOSFactory <- function(){

  Singleton <- NULL

  # return a function
  function(){

    if (is.null(Singleton)){
      Singleton <<- CreateOS()
    }

    return(Singleton)

  }


}


# The OS factory
OSFactory <- CreateOSFactory()



#' @export
GetOS <- function(){
  OSFactory()
}

