library("dummyPkg")

executionTimes <- replicate(3, {
    startTime <- Sys.time()
    sleepy()
    endTime <- Sys.time()
    as.numeric(difftime(endTime, startTime, units = "secs"))
})

executionTimes