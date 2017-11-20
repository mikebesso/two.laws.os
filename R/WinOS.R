#' @include OSBaseClass.R
NULL


#' @export
WinOS <- R6Class(
  "WinOS",
  inherit = OSBaseClass,
  public = list(


    AssertDateFormat = function(dateFormat){
      AssertAllMatchFixed(self$GetDateFormat(), dateFormat)
    },

    AssertStandardDateFormat = function(){
      AssertAllMatchFixed(self$GetDateFormat(), "yyyy-MM-dd")
    },

    GetDateFormat = function(){

      if (is.na(private$DateFormat)){

        Results <- self$System(
          command = "REG",
          args = c("QUERY", shQuote("HKCU\\Control Panel\\International"), "/v", "sShortDate")
        )
        private$DateFormat <- tail(str_split(Results$StdOut[3], "\\s")[[1]], 1)
      }


      return(private$DateFormat)

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


    QueryScheduledTasks = function(refresh = FALSE){

      self$AssertStandardDateFormat()

      if (refresh || is.data.frame(private$ScheduledTasks)){

        self$Message("QueryScheduledTasks refreshing")

        Results <- self$System(
          "schtasks",
          args = c("/Query", "/FO CSV", "/V")
        )

        HeaderRepeatsRemoved <- c(
          Results$StdOut[1],
          Results$StdOut[!str_detect(Results$StdOut, '("HostName","TaskName",|Microsoft)')]
        )

        private$ScheduledTasks <- read_csv(
          paste(
            HeaderRepeatsRemoved,
            collapse = "\n"
          )
        )
      } else {
        self$Message("QueryScheduledTasks returning cached results")
      }

      return(private$ScheduledTasks)
    },

    DeleteScheduledTask = function(taskName){

      ScheduledTasks <- self$QueryScheduledTasks(refresh = TRUE)


      if (any(ScheduledTasks[["TaskName"]] == paste0("\\", taskName))){
        Results <- self$System(
          "schtasks",
          args = c("/Delete", "/TN", shQuote(taskName), "/F")
        )
      } else {
        Results = list(StatusCode = 0, StdOut = character(0), StdErr = character(0))
      }


      ScheduledTasks <- self$QueryScheduledTasks(refresh = TRUE)


      return(Results)
    },

    CreateScheduledTask = function(
      taskName,
      rscript,
      schedule = c('DAILY', 'WEEKLY'),
      time = "07:00",
      days = c('SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT')

    ) {

      self$AssertStandardDateFormat()

      self$DeleteScheduledTask(taskName)



      Schedule = match.arg(schedule)


      Command <- "cmd.exe"
      Args = c("/Create", "/TN", shQuote(taskName), "/TR", Command, "/SC", Schedule, '/SD', format(Sys.Date()), "/ST", time)


      if (!missing(days)){
        Days = paste(match.arg(days, several.ok = TRUE), collapse = ",")
        Args <- c(Args, '/D', Days)
      }



      Results <- self$System(
        "schtasks",
        args = Args
      )


      if (length(Results$StdErr) > 0){
        stop(
          "CreateScheduledTask Failed",
          paste(
            Results$StdErr,
            collapse = TRUE
          )
        )
      }

      ScheduledTasks <- self$QueryScheduledTasks(refresh = TRUE)

      Results$Task <- ScheduledTasks %>%
        dplyr::filter(TaskName == paste0("\\", taskName))

      return(Results)

    },


    initialize = function(verbose = FALSE){
      super$initialize(verbose = verbose)

      AssertAllMatchFixed(self$OSName, "Windows")
    }

  ),

  private = list(
    DateFormat = NA,
    ScheduledTasks = NA
  )

)
