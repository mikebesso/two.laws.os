NULL




TwoLawsOSClass <- R6Class(
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
