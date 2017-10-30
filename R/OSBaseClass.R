
#' @export
OSBaseClass <- R6Class(

  "OSBaseClass",
  inherit = BaseClass,
  public = list(

    Folders = list(),
    OSName = NA,

    TempFile = function(prefix = "file", fileExtension = ""){

      FileExtension <- if ((nchar(fileExtension) > 0) & substr(fileExtension, 1, 1) != ".") {
        paste0(".", fileExtension)
      } else {
        fileExtension
      }

      TempFilePath <- file.path(self$Folders$Temp, paste0(basename(tempfile(prefix)), FileExtension))
      return(TempFilePath)
    },

    GetTempPath = function(prefix = "file", fileExtension = ""){

      Path <- self$TempFile(prefix = prefix, fileExtension = fileExtension)

      if (!dir.exists(dirname(Path))){
        TempFolder <- dirname(Path)
        self$Message("Creating Temp Folder: ", TempFolder)
        self$CreateFolder(TempFolder)

      }


      return(Path)
    },

    GetDateFormat = function(){
      stop("Generic GetDateFormat() not yet written")
    },

    System = function(
      command,
      args = character(),
      stdin = "",
      input = NULL,
      env = character(),
      wait = TRUE,
      uiMinimized = FALSE,
      uiInvisible = TRUE
    ) {


      StdOut <- self$GetTempPath(prefix = "GetDateFormat-", fileExtension = "stdout");
      StdErr <- self$GetTempPath(prefix = "GetDateFormat-", fileExtension = "stderr");



      self$Message(StdOut)
      self$Message(StdErr)

      StatusCode <- tryCatch(
         private$system2(
          command,
          args = args,
          stdout = StdOut,
          stderr = StdErr,
          stdin = stdin,
          input = input,
          env = env,
          wait = wait,
          minimized = uiMinimized,
          invisible = uiInvisible
        ),
        warning = function(w){

        },
        error = function(e){
          stop(e)
        }

      )

      Results <- list(
        StatusCode = StatusCode,
        StdOut = readLines(StdOut),
        StdErr = readLines(StdErr)
      )

       return(Results)
    },

    CreateFolder = function(folder){

      dir.create(folder, showWarnings = FALSE, recursive = TRUE)

      AssertFolderExists(folder)

    },

    DeleteFolder = function(folder){

      unlink(folder, recursive = TRUE)

      # Need to assert folder is gone
    },


    CopyFiles = function(from, to){
      stop("Generic CopyFiles() not yet written")
    },

    MoveFolder = function(from, to){
      stop("Generic MoveFiles() not yet written")
    },

    MoveFolderContents = function(from, to){
      stop("Generic MoveFolderContents() not yet written")
    },




    CreateTestSet = function(folderRoot = ".", folderHierarchy = c(a = 1), rootCount = 1){

      # Create root folder
      self$CreateFolder(folderRoot)

      # Put the rootCount files at the root
      if (rootCount > 0){
        for (i in 1:rootCount){
          File <- paste0("root-", i, ".txt")
          Path <- file.path(folderRoot, File)
          writeLines(Path, Path)
        }
      }

      # Create the folder hierarchy... and put the specified number of files in each folder
      FolderHierarchyLength <- length(folderHierarchy)
      if (FolderHierarchyLength > 0){
        for (i in 1:length(folderHierarchy)){

          Folder <- file.path(
            folderRoot,
            str_replace_all(names(folderHierarchy)[[i]], "-", "/")
          )

          self$CreateFolder(Folder)

          if (folderHierarchy[[i]] > 0){
            for(j in 1:folderHierarchy[[i]]){

              File <- paste0(names(folderHierarchy)[[i]], "-", j, ".txt")
              Path <- file.path(Folder, File)

              # Put the path as text inside the file
              writeLines(Path, Path)

            }
          }
        }
      }

      Folders <- list.dirs(folderRoot, full.names = TRUE, recursive = TRUE)
      Files <- list.files(folderRoot, full.names = TRUE, recursive = TRUE)

      AssertFolderExists(Folders)
      AssertFilesExists(Files)

      return(
        list(
          Folders = Folders,
          Files = Files
        )

      )
    },


    initialize = function(verbose = FALSE){
      super$initialize(verbose = verbose)

      self$OSName <- Sys.info()[["sysname"]]

      TempFolder <- tempdir()

      self$Folders$Temp = file.path(dirname(TempFolder), "TwoLaws", basename(TempFolder))

    }
  ),

  private = list(

    system2 = function (command, args = character(), stdout = "", stderr = "",
              stdin = "", input = NULL, env = character(), wait = TRUE,
              minimized = FALSE, invisible = TRUE)
    {

      if (!is.logical(wait) || is.na(wait))
        stop("'wait' must be TRUE or FALSE")

      if (!is.logical(minimized) || is.na(minimized))
        stop("'minimized' must be TRUE or FALSE")

      if (!is.logical(invisible) || is.na(invisible))
        stop("'invisible' must be TRUE or FALSE")

      command <- paste(c(shQuote(command), env, args), collapse = " ")

      self$Message("\n", rep("*", 10), "\nRunning System Command\n\n\t", command, "\n", rep("*", 10), "\n\n")

      if (is.null(stdout))
        stdout <- FALSE
      if (is.null(stderr))
        stderr <- FALSE
      if (length(stdout) != 1L)
        stop("'stdout' must be of length 1")
      if (length(stderr) != 1L)
        stop("'stderr' must be of length 1")

      if (!is.null(input)) {
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
      }
      else f <- stdin
      flag <- if (isTRUE(stdout) || isTRUE(stderr))
        3L
      else if (wait)
        ifelse(identical(stdout, ""), 2L, 1L)
      else 0L
      if (invisible)
        flag <- 20L + flag
      else if (minimized)
        flag <- 10L + flag
      .Internal(system(command, flag, f, stdout, stderr))
    }

  )
)
