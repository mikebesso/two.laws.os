#' @import two.laws.big.bang
NULL




TwoLawsOSClass <- two.laws.big.bang::R6Class(
  "TwoLawsOSClass",
  inherit = ProjectBaseClass,
  public = list(

    initialize = function(verbose = FALSE, testthat = FALSE){
      super$initialize(verbose = verbose)
    }
  )
)




if (!devtools::uses_testthat()){
  devtools::use_testthat()
}
