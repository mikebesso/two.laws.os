



TwoLawsOS <- two.laws.os:::TwoLawsOSClass$new(testthat = TRUE, verbose = TRUE)
OS <- CreateOS(verbose = TRUE)

TempFolder <- file.path(TwoLawsOS$Folders$TestThatTemp)
FromFolder <- file.path(TempFolder, "from")
ToFolder <- file.path(TempFolder, "to")




Prepare <- function(folderRoot = ".", folderHierarchy = numeric(0), rootCount = 1){


  OS$DeleteFolder(TempFolder)
  OS$CreateFolder(TempFolder)

  OS$CreateTestSet(folderRoot = folderRoot, folderHierarchy = folderHierarchy, rootCount = rootCount)
}


context("RoboCopy - Move")

test_that(
  "Move folder with single file",
  {
    TestSet <- Prepare(folderRoot = FromFolder, folderHierarchy = c(a = 1))

    OS$MoveFolder(FromFolder, ToFolder)

    ToFolders <- str_replace(TestSet$Folders, fixed(FromFolder), ToFolder)
    ToFiles <- str_replace(TestSet$Files, fixed(FromFolder), ToFolder)

    expect_true(dir.exists(ToFolders[[1]]))
    expect_true(file.exists(ToFiles[[1]]))

    # When moving a folder's contents, the from folder should get deleted
    expect_true(!dir.exists(FromFolder))

  }
)



test_that(
  "Move folder complex folder hierarchy",
  {
    TestSet <- Prepare(folderRoot = FromFolder, folderHierarchy = c(a = 2, b = 3, `a-a` = 2, `b-a` = 0), rootCount = 3)

    OS$MoveFolder(FromFolder, ToFolder)

    ToFolders <- str_replace(TestSet$Folders, fixed(FromFolder), ToFolder)
    ToFiles <- str_replace(TestSet$Files, fixed(FromFolder), ToFolder)

    expect_true(dir.exists(ToFolders[[1]]))
    expect_true(file.exists(ToFiles[[1]]))

    # When moving a folder's contents, the from folder should get deleted
    expect_true(!dir.exists(FromFolder))

  }
)


context("RoboCopy - Move Contents")


test_that(
  "Move folder contents with single file",
  {
    TestSet <- Prepare(folderRoot = FromFolder, folderHierarchy = numeric(0), rootCount = 1)

    OS$MoveFolderContents(FromFolder, ToFolder)

    ToFolders <- str_replace(TestSet$Folders, fixed(FromFolder), ToFolder)
    ToFiles <- str_replace(TestSet$Files, fixed(FromFolder), ToFolder)

    expect_true(dir.exists(ToFolders[[1]]))
    expect_true(file.exists(ToFiles[[1]]))

    # When moving a folder's contents, the from folder should not get deleted
    expect_true(dir.exists(FromFolder))

  }
)





test_that(
  "Move folder contents with single file and folder",
  {
    TestSet <- Prepare(folderRoot = FromFolder, folderHierarchy = c(a = 0), rootCount = 1)

    OS$MoveFolderContents(FromFolder, ToFolder)

    ToFolders <- str_replace(TestSet$Folders, fixed(FromFolder), ToFolder)
    ToFiles <- str_replace(TestSet$Files, fixed(FromFolder), ToFolder)

    expect_true(dir.exists(ToFolders[[1]]))
    expect_true(file.exists(ToFiles[[1]]))

    # When moving a folder's contents, the from folder should not get deleted
    expect_true(dir.exists(FromFolder))

  }
)

