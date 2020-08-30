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
                            # inMemory : only use together with fileMode == TRUE. if TRUE data gets all
                            #            loaded into the object, otherwise not 
                            initialize = function(theData, fileMode = TRUE, inMemory = FALSE,...){
                                    if (fileMode){
                                            self$loadFile(theData, inMemory = inMemory)
                                    } else {
                                            # dataFile remains NA
                                            private$allLines = theData
                                    }
                              invisible(self)
                            },
                            # load all (text) data into the private allLines variable
                            loadFile = function(fileName, inMemory = FALSE){
                                    if (!is.na(fileName)){
                                            private$dataFile <- fileName
                                            if (inMemory) {
                                                    private$allLines <- readLines(private$dataFile, n = -1)
                                            }
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
                            # display if the lines of the text file are in memory
                            inMemory = function(value){
                                    if (missing(value)){
                                            return(!identical(private$allLines,NA))
                                    } else {
                                            # nothing, read-only!
                                    }
                            },
                            # in case access is needed to the original (text) data
                            # cannot change data via this!!
                            rawData = function(value){
                                    if (missing(value)){
                                            if (self$inMemory){
                                                    return(private$allLines)
                                            } else {
                                                    return(readLines(private$dataFile, n = -1))
                                            }
                                    } else {
                                            # nothing, read-only!
                                    }
                            },
                            # to get the name of the file containing the mascot data
                            # is specified in intialization
                            fileName = function(value){
                                    if (missing(value)){
                                            return(private$dataFile)
                                    } else {
                                            # nothing, read-only!
                                    }
                            },
                            # get the length = number of lines of (text) data
                            length = function(value){
                                    if (missing(value)){
                                            if (self$inMemory){
                                                    return(length(private$allLines))
                                            } else {
                                                    tempLines <- readLines(private$dataFile, n = -1)
                                                    return(length(tempLines))
                                            }
                                    } else {
                                            # nothing, read-only!
                                    }
                            }
                    )
)