#' @include OSBaseClass.R

#' @export
MacOS <- R6Class(
  "MacOS",
  inherit = OSBaseClass,
  public = list(



    initialize = function(verbose = FALSE){
      super$initialize(verbose = verbose)

      AssertAllMatchFixed(self$OSName, "Darwin")
    }

  )

)
