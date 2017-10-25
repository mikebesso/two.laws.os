#' @include OSBaseClass.R


WinOS <- R6Class(
  "WinOS",
  inherit = OSBaseClass,
  public = list(

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
    }

  )

)
