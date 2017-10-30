library(testthat)




OS <- WinOS$new()

OS$CreateScheduledTask("testR1", "testR1")


x <- OS$QueryScheduledTasks()
