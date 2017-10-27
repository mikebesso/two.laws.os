#' @include OSBaseClass.R

#' @export
WinOS <- R6Class(
  "WinOS",
  inherit = OSBaseClass,
  public = list(


    AssertDateFormat = function(dateFormat){
      AssertAllMatchFixed(GetDateFormat(), dateFormat)
    },

    AssertStandardDateFormat = function(){
      AssertAllMatchFixed(GetDateFormat(), "yyyy-MM-dd")
    },

    GetDateFormat = function(){

      Results <- self$System(
        command = "REG",
        args = c("QUERY", shQuote("HKCU\\Control Panel\\International"), "/v", "sShortDate")
      )

      DateFormat <- tail(str_split(Results$StdOut[3], "\\s")[[1]], 1)

      return(DateFormat)

    },

    RoboCopy = function(fromFolder, toFolder, files = character(), args = character()){

      self$System(
        "robocopy",
        args = c(fromFolder, toFolder, files, args)
      )

    },


    MoveFolder = function(fromFolder, toFolder){

      self$Message("Moving Folder: ", fromFolder, ", ", toFolder)

      self$RoboCopy(
        fromFolder = shQuote(fromFolder),
        toFolder = shQuote(toFolder),
        args = c("/move", "/e")
      )

    },

    MoveFiles = function(fromFolder, toFolder){

      self$Message("Moving Files: ", fromFolder, ", ", toFolder)

      self$RoboCopy(
        fromFolder = fromFolder,
        toFolder = toFolder,
        args = c("/mov")
      )
    },

    MoveFolderContents = function(fromFolder, toFolder){

      self$Message("Moving Folder Contents: ", fromFolder, ", ", toFolder)



      # Move the files in fromFolder
      self$Message("Moving Root Files")
      self$MoveFiles(fromFolder, toFolder)

      # Get all of the folders in fromFolder, excluding the fromFolder
      # which is the first item in the list
      FromFolders <- list.dirs(fromFolder, full.names = TRUE)[-1]
      ToFolders <- str_replace_all(FromFolders, fixed(fromFolder), toFolder)

      # Move all of the folders and their contents
      self$Message("Moving sub folders")
      if (length(FromFolders) > 0){
        mapply(
          self$MoveFolder,
          FromFolders,
          ToFolders
        )
      } else {
        self$Message("No folders to move")
      }

    },




    initialize = function(verbose = FALSE){
      super$initialize(verbose = verbose)

      AssertAllMatchFixed(self$OSName, "Windows")
    }

  )

)
