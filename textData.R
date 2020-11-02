library(R6)

# basic class to work with (large) textfiles

textData <- R6Class("textData",
                    private = list(
                            # name of the file
                            dataFile = NA,
                            # text file format data
                            allLines = NA
                    ),
                    public = list(
                            # fileMode : if "file" then theData = filename, else theData = actual data
                            initialize = function(theData, fileMode = TRUE, ...){
                                    if (fileMode){
                                            self$loadFile(theData, ...)
                                    } else {
                                            # dataFile remains NA
                                            private$allLines = theData
                                    }
                              invisible(self)
                            },
                            # load all (text) data into the private allLines variable
                            loadFile = function(fileName){
                                    if (!is.na(fileName)){
                                            private$dataFile <- fileName
                                            private$allLines <- readLines(private$dataFile, n = -1)
                                    } # else do nothing!
                              invisible(self)
                            },
                            # save all (text) data to a file
                            saveFile = function(fileName = NA, theData = NA){
                                    if (is.na(fileName)){
                                            aFile <- file(private$dataFile, open = "w")
                                    } else {
                                            aFile <- file(fileName, open = "w")
                                            if (is.na(private$dataFile)){
                                                    private$dataFile <- fileName
                                            }
                                    }
                                    if (identical(theData,NA)){
                                            if (self$inMemory){
                                                    writeLines(private$allLines,aFile)
                                            } else {
                                                    tempLines <- eadLines(private$dataFile, n = -1)
                                                    writeLines(tempLines,aFile)
                                            }
                                    } else {
                                            writeLines(theData,aFile)
                                    }
                                    close(aFile)
                            },
                            # placeholder virtual method
                            correct = function(problem = NA,...){
                                    # do nothing
                              invisible(self)
                            }
                    ),
                    active = list(
                            # in case access is needed to the original (text) data
                            rawData = function(value){
                                    if (missing(value)){
                                      return(private$allLines)
                                    } else {
                                      private$allLines <- value
                                    }
                            },
                            # to get the name of the file containing the mascot data
                            # is specified in intialization
                            fileName = function(value){
                                    if (missing(value)){
                                            return(private$dataFile)
                                    } else {
                                            private$dataFile <- value
                                    }
                            },
                            # get the length = number of lines of (text) data
                            length = function(value){
                                    if (missing(value)){
                                      return(length(private$allLines))
                                    } else {
                                      # do nothing
                                    }
                            }
                    )
)