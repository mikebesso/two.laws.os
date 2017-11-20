#' @include OSBaseClass.R
NULL


#' @export
MacOS <- two.laws.big.bang::R6Class(
  "MacOS",
  inherit = OSBaseClass,
  public = list(



    initialize = function(verbose = FALSE){
      super$initialize(verbose = verbose)

      AssertAllMatchFixed(self$OSName, "Darwin")
    }

  )

)
