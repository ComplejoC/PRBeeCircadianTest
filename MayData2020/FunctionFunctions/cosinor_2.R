Cosinor_2 <- function (TimeCol = 1, Y = 2, Components = 1, window = "noTaper", 
                       RefDateTime = NA, timeFormat = "%Y%m%d%H%M", RangeDateTime = list(Start = NA, 
                                                                                         End = NA), Units = "hours", dt = 0, Progressive = list(Interval = 144, 
                                                                                                                                                Increment = 8), Period = list(Set = 0, Start = 0, Increment = 1, 
                                                                                                                                                                              End = 0), header = F, Skip = 0, Colors = "BW", Graphics = "pdf", 
                       Output = list(Txt = F, Dat = T, Doc = T, Graphs = F), yLabel = "", 
                       Console = F, Debug = FALSE, IDcol = "fileName", fileName = fileName, 
                       functionName = "")
  
  
  
  
  {
  tz = "GMT"
  GraphSet <- list(Data = T, Model = T, MESOR = T, Amp = T, P = T, Phi = T, NPts = T, HeatMap = F)
  if (length(fileName) == 0) {
    fileName <- file.choose()
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  LCM <- Period$Set[1]
  if (all(is.wholenumber(Period$Set))) {
    tempPeriods <- Period$Set
    fractionModifier <- 1
  } else {
    fractionModifier <- 100
    tempPeriods <- round(Period$Set, digits = 2) * fractionModifier
  }
  minPeriod <- min(Period$Set)
  if (Period$Set[1] > 0) {
    LCM <- max(Period$Set)
    if (Components > 1) {
      "%gcd%" <- function(u, v) {
        ifelse(u%%v != 0, v %gcd% (u%%v), v)
      }
      "%lcm%" <- function(u, v) {
        abs(u * v)/(u %gcd% v)
      }
      LCM <- 1
      for (i in 1:Components) {
        LCM <- as.integer(tempPeriods[i]) %lcm% LCM
      }
      LCM <- LCM/fractionModifier
    }
  } else {
    LCM <- 24
  }
  print(LCM)
  StartDate <- RangeDateTime$Start
  EndDate <- RangeDateTime$End
  Interval <- Progressive$Interval
  Increment <- Progressive$Increment
  FreqInc <- Period$Increment
  oneCycle <- sort(Period$Set, decreasing = TRUE)
  Ys_end <- length(Y)
  paramMsg <- paste("\n  TimeCol=", TimeCol, ",  Y=", Y, ",  header=", header, "\n --  Periods=", Period["Set"], ", Units=", Units, ",  Interval=", format(Interval, nsmall = 3), ",  Increment=", format(Increment, nsmall = 3), "\nPeriod$Start=", format(Period$Start, nsmall = 3), ",  FreqInc=", format(FreqInc, nsmall = 3), ",  Period$End=", format(Period$End, nsmall = 3), "\nRefDateTime=", RefDateTime, ", StartDate=", format(StartDate, nsmall = 3), ", EndDate=", format(EndDate, nsmall = 3), "Percent of missing (blank) sample values: unknown", 
                    "\n", functionName, "\n")
  CosMatrixDim <- Components * 2 + 1
  if (Components > 1 && length(oneCycle) != Components) {
    errMsg <- paste("Error:  When parameter Components is >1, Period$Set must have a corresponding number of components.  \nComponents=", Components, "; Period$Set=", Period$Set)
    stop(errMsg)
  }
  errMsg <- ""
  errKey <- c("*", "**", "+", "++", "@", "*@", "@@@", "@@")
  keyMsgs <- vector(mode = "character", length = 10)
  keyMsgs[1] <- paste("*  : Error1:  The matrix cannot be inverted for these points and period. This may happen if there are not enough data points for the period selected. Check sampling interval: there should be at least 2p+1 data points per period.\n")
  keyMsgs[2] <- paste("** : Error2:  The matrix cannot be inverted for these points and period. This may happen if there are not enough data points for the period selected. Check sampling interval: there should be at least 2p+1 data points per period.\n")
  keyMsgs[3] <- "+  : Error:  This model does not allow calculation of PR for the individual components. PR is negative, which is invalid.\n"
  keyMsgs[4] <- "++ : Error:  Coefs cannot be calculated\n"
  keyMsgs[5] <- "@  : Error:  Requested analysis violates Nyquist.  Time span should be > 2C, where C=#cycles.  Results may be invalid.\n"
  keyMsgs[6] <- "*@ : Warning:  RSS=0.  Not enough data to calculate S.E.  \n"
  keyMsgs[7] <- "@@@: Error:  This interval must be at least as long as the trial period (Period$Set).  No analysis performed when Interval<90% of target period.\n"
  keyMsgs[8] <- "@@: Warning:  The interval must be at least as long as the trial period (Period$Set).  Results may be unreliable.\n"
  keyMsgs[9] <- "*@@: ERROR:  RSS/df2 will be infinite because df2<0, so this case was skipped.  (Number of data points <= number of components*2+1)\n"
  keyMsgs[10] <- "*@@@: Warning:  RSS/df2 will be infinite because df2=0, so s.e.s, F and P calculations are skipped.  (Number of data points <= number of components*2+1)\n"
  if (.Platform$file.sep == "/") {
    m = regexec("[^/]*$", fileName)
  } else {
    m = regexec("[^\\]*$", fileName)
  }
  fileName1 <- regmatches(fileName, m)
  fileLen <- nchar(fileName1)
  fileName6 <- substring(fileName1, 1, fileLen - 4)
  BaseTime <- Sys.time()
  thisTime <- format(BaseTime, "--%d%b%y--%H-%M-%OS")
  fileName2 <- paste(fileName, window, thisTime, functionName, "Cos.txt", sep = "")
  fileName3 <- paste(fileName, window, thisTime, functionName, "Cos.rtf", sep = "")
  if (Output$Txt) {
    sink(fileName2, append = FALSE, split = TRUE)
  }
  MyDataDelimiter <- read.csv(fileName, nrows = 6, header = header, stringsAsFactors = FALSE, skip = Skip)
  colCount <- length(MyDataDelimiter[5, ])
  if (colCount > 1) {
    delim <- "\t"
  } else {
    delim <- ","
  }
  headerChar1 <- ""
  myHeader <- read.csv(fileName, header = FALSE, nrows = 1, sep = delim, skip = Skip)
  if (delim == "\t") {
    if (header == TRUE) {
      myHeader <- read.csv(fileName, header = header, nrows = 1, skip = Skip)
    }
    MyDataReada <- read.csv(fileName, header = header, stringsAsFactors = FALSE, skip = Skip, col.names = names(myHeader), blank.lines.skip = TRUE, na.strings = c(" ", "--"))
  } else {
    colCount <- length(myHeader)
    tableCols <- c(TimeCol, Y)
    tableColLen <- colCount
    tableColTypes <- rep("character", tableColLen)
    tableColTypes[Y] <- "numeric"
    if (all(TimeCol > 0) && all(TimeCol <= 10)) {
      tableColTypes[TimeCol] <- "character"
    }
    MyDataReada <- read.csv(fileName, header = header, stringsAsFactors = FALSE, skip = Skip, blank.lines.skip = TRUE, na.strings = c(" ", "--"))
  }
  if (FALSE) {
    weirdHour <- as.integer(substr(MyDataReada[, 1], 9, 10))
    weirdHourIdx <- which(weirdHour > 23)
    weirdHourInt <- as.integer(substr(MyDataReada[weirdHourIdx, 1], 9, 10))
    Hour <- strsplit(MyDataReada[weirdHourIdx, 1], substr(MyDataReada[weirdHourIdx, 1], 9, 10), fixed = TRUE)
    zz <- 1
    View(weirdHourIdx)
    for (z in weirdHourIdx) {
      if (weirdHourInt[zz] - 24 < 10) {
        zeroPad <- "0"
      }
      else {
        zeroPad <- ""
      }
      saveRead <- MyDataReada[z, 1]
      if (Hour[[zz]][2] == "") {
        Hour[[zz]][2] <- weirdHourInt[zz]
      }
      MyDataReada[z, 1] <- paste(Hour[[zz]][1], zeroPad, weirdHourInt[zz] - 24, Hour[[zz]][2], sep = "")
      Day <- strptime(MyDataReada[z, 1], "%Y%m%d%H%M", tz = tz)
      MyDataReada[z, 1] <- format(Day + (24 * 3600), "%Y%m%d%H%M")
      cat(saveRead, "zz", zz, "weirdHourInt[zz]", weirdHourInt[zz], "--", "z", z, MyDataReada[z, 1], "\n")
      zz <- zz + 1
    }
  }
  if (IDcol == "fileName") {
    SubjectID <- fileName6
  } else {
    SubjectID <- MyDataReada[1, IDcol]
  }
  keepers <- c()
  for (y in c(TimeCol, Y)) {
    is.na(MyDataReada[, y]) <- which(MyDataReada[, y] == "")
    if (all(is.na(MyDataReada[, y]))) {
      message <- paste("ERROR:  You have selected a column that has no data in it:  column", y)
      errMsg <- paste(errMsg, message)
      closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
      stop(message)
    }
    keepersY <- na.omit(MyDataReada[, y])
    keepers <- union(na.action(keepersY), keepers)
  }
  if (length(keepers) > 0) {
    MyDataRead <- MyDataReada[-keepers, ]
  } else {
    MyDataRead <- MyDataReada
  }
  missingData <- (length(MyDataReada[, TimeCol]) - length(MyDataRead[, TimeCol]))/length(MyDataReada[, TimeCol])
  if (timeFormat == "numeric") {
    BaseDate <- format(Sys.Date(), "%Y%m%d%H%M")
    origin <- as.POSIXct("1970-01-01 00:00:00", tz = tz)
    offset <- as.numeric(origin)
    newOrder <- MyDataRead[order(MyDataRead[, TimeCol], na.last = TRUE, decreasing = FALSE), ]
    MyDataRead <- newOrder
    browser()
    MyDataRead[, TimeCol] <- format(as.POSIXct(as.numeric(as.POSIXct(strptime(BaseDate, "%Y%m%d%H%M", tz = tz))) + (as.numeric(MyDataRead[, TimeCol]) * 3600), origin = origin, tz), "%Y%m%d%H%M")
    timeFormat <- "%Y%m%d%H%M"
  }
  str(MyDataRead)
  AnalyzeLength <- (as.POSIXct(strptime(tail(MyDataRead[, TimeCol], 1), "%Y%m%d%H%M", tz = tz))) - as.POSIXct(strptime(MyDataRead[1, TimeCol], "%Y%m%d%H%M", tz = tz))
  MyData_HRs <- (AnalyzeLength) * 24
  if (is.na(StartDate)) {
    StartDate <- format(as.POSIXct(strptime(MyDataRead[1, TimeCol], "%Y%m%d%H%M", tz = tz)), "%Y%m%d%H%M")
  } else {
    if (StartDate == 0) {
      StartDate <- paste(substr(MyDataRead[1, TimeCol], 1, 8), "0000", sep = "")
    }
    else StartDate <- format(as.POSIXct(strptime(StartDate, timeFormat, tz = tz)), "%Y%m%d%H%M")
  }
  StartIndex <- which(MyDataRead[, TimeCol] >= StartDate)
  MyData_length <- length(StartIndex)
  if (is.na(EndDate)) {
    EndDate <- format(as.POSIXct(strptime(tail(MyDataRead[StartIndex, TimeCol], 1), "%Y%m%d%H%M", tz = tz)), "%Y%m%d%H%M")
  } else {
    if (EndDate == 0) {
      EndDate <- paste(substr(format(as.POSIXct(strptime(MyDataRead[MyData_length, TimeCol], "%Y%m%d%H%M", tz = tz)) + 86400, "%Y%m%d%H%M"), 1, 8), "0000", sep = "")
    }
    else EndDate <- format(as.POSIXct(strptime(EndDate, timeFormat, tz = tz)), "%Y%m%d%H%M")
  }
  EndIndex <- which(MyDataRead[, TimeCol] <= EndDate)
  MyData <- MyDataRead[StartIndex[1]:tail(EndIndex, n = 1), ]
  cat("There are ", MyData_HRs, " actual hours of data in this file, ", MyDataRead[1, TimeCol], " to ", MyDataRead[tail(EndIndex, n = 1), TimeCol], ", and ", MyData_length, "data points.\n  %", missingData, "of data points are missing.\n")
  print(MyDataRead[1, ])
  print(tail(MyDataRead, 1))
  MyData$time = as.POSIXct(strptime(MyData[, TimeCol], "%Y%m%d%H%M", tz = tz))
  if (is.na(RefDateTime)) {
    RefTime <- as.POSIXct(strptime(MyData[1, TimeCol], "%Y%m%d%H%M", tz = tz))
    RefTimeString <- MyData[1, TimeCol]
  } else if (RefDateTime == 0) {
    RefTime <- as.POSIXct(strptime(MyData[1, TimeCol], "%Y%m%d", tz = tz))
    RefTimeString <- paste(substr(MyData[1, TimeCol], 1, 8), "0000", sep = "")
  } else {
    RefTime <- as.POSIXct(strptime(RefDateTime, "%Y%m%d%H%M", tz = tz))
    RefTimeString <- RefDateTime
  }
  StartTime <- (as.numeric(as.POSIXct(strptime(StartDate, "%Y%m%d%H%M", tz = tz))) - as.numeric(RefTime))/3600
  if (is.na(StartTime)) {
    message <- paste("ERROR:  StartDate or Date format is invalid: ", StartDate)
    errMsg <- paste(errMsg, message)
    closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
    stop(message)
  }
  EndTime <- (as.numeric(as.POSIXct(strptime(EndDate, "%Y%m%d%H%M", tz = tz))) - as.numeric(RefTime))/3600
  if (is.na(EndTime)) {
    message <- paste("ERROR:  EndDate or Date format is invalid: ", EndTime)
    errMsg <- paste(errMsg, message)
    closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
    stop(message)
  }
  MyData$time.n = as.numeric(MyData$time) - as.numeric(RefTime)
  MyData$time.hour <- MyData$time.n/3600
  MyData_length <- length(MyData[, 1])
  drawPgram <- FALSE
  if (dt == 0) {
    drawPgram <- TRUE
    sumTab <- diff(MyData$time.hour)
    dt <- median(sumTab, na.rm = TRUE)
  }
  MyData_hours <- as.numeric(EndTime - StartTime) + dt
  cat("The program is using ", MyData_hours, " hours for the analysis, starting at hour", StartTime, "and ending at hour", EndTime, " relative to RefTime:", RefTimeString, "\n")
  Interval <- MyData_hours
  Increment <- MyData_hours
  Period$Start <- Interval
  if (FreqInc == 0) {
    FreqInc <- 1
    paramMsg <- paste("\n  TimeCol=", TimeCol, ",  Y=", Y, ",  header=", header, "\n --  Periods=", Period["Set"], ", Units=", Units, ",  Interval=", format(Interval, nsmall = 3), ",  Increment=", format(Increment, nsmall = 3), "\nPeriod$Start=", format(Period$Start, nsmall = 3), ",  FreqInc=", format(FreqInc, nsmall = 3), ",  Period$End=", format(Period$End, nsmall = 3), "\nRefDateTime=", RefDateTime, ", StartTime=", format(StartTime, nsmall = 3), ", EndTime=", format(EndTime, nsmall = 3), "Percent of missing (blank) sample values: %", 
                      missingData, "\n", functionName, "\n")
  } else if (FreqInc < 0) {
    message <- "ERROR:  The parameter Period$Increment cannot be negative.\n"
    errMsg <- paste(errMsg, message)
    closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
    stop(message)
  }
  if (Period$End == 0) {
    if (dt == 0) {
      Period$End <- 4
    }
    else if (Period$End < 0) {
      message <- "ERROR:  The parameter Period$End cannot be negative.\n"
      errMsg <- paste(errMsg, message)
      closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
      stop(message)
    }
    else if (Period$End > MyData_hours) {
      message <- "ERROR:  The parameter Period$End is larger than the number of hours.  ???\n"
      errMsg <- paste(errMsg, message)
      print(message)
    }
    else Period$End <- 2 * dt
    if (Period$End > Period$Start) {
      Period$End <- Period$Start/4
      FreqInc <- 0.1
    }
  }
  cat(Period$Start, " Period$Start ", Period$End, " Period$End ", Period$Start, "Period$Start", Interval, "Interval", Increment, "Increment\n")
  paramMsg <- paste("\n  TimeCol=", TimeCol, ",  Y=", Y, ",  header=", header, "\n --  Periods=", Period["Set"], ", Units=", Units, ",  Interval=", format(Interval, nsmall = 3), ",  Increment=", format(Increment, nsmall = 3), "\nPeriod$Start=", format(Period$Start, nsmall = 3), ",  FreqInc=", format(FreqInc, nsmall = 3), ",  Period$End=", format(Period$End, nsmall = 3), "\nRefDateTime=", RefDateTime, ", StartTime=", format(StartTime, nsmall = 3), ", EndTime=", format(EndTime, nsmall = 3), "Percent of missing (blank) sample values: %", 
                    missingData, "\n", functionName, "\n")
  EndIdx <- 0
  if (Interval > MyData_hours) {
    message <- paste("Warning: Chosen Progressive$Interval (", Interval, ") is longer than data file (", MyData_hours, ") (MyData_hours).\n")
    print(message)
    errMsg <- paste(errMsg, message)
  }
  if (Increment > MyData_hours) {
    message <- paste("Warning:  Chosen Progressive$Increment (", Increment, ") is longer than data file (", MyData_hours, ") (MyData_hours).\n")
    print(message)
    errMsg = paste(errMsg, message)
  }
  if (MyData_hours == Interval) {
    StartSpans <- seq(from = 0, to = MyData_hours - 1, by = Increment)
  } else {
    if ((MyData_hours - Interval + Increment) <= 0) {
      errMsg <- paste(errMsg, "Error:  MyData_hours-Progressive$Interval+Progressive$Increment is < 1:  ", MyData_hours, "-", Interval, "+", Increment, ".  Using ", MyData_hours, "as Interval.\n")
      closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
      Interval <- MyData_hours
    }
    StartSpans <- seq(from = 0, to = (MyData_hours - Interval + Increment), by = Increment)
  }
  Progression_end <- length(StartSpans)
  sumN <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  sumLow <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  sumHi <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  sumMean <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  sumMedian <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  sumMode <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  sumSD <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  sumT <- matrix(data = NA, nrow = Ys_end, ncol = Progression_end + 1)
  yNew <- 0
  for (y in Y) {
    yNew <- yNew + 1
    sumN[yNew, Progression_end + 1] <- MyData_length
    sumLow[yNew, Progression_end + 1] <- min(MyData[, y], na.rm = TRUE)
    sumHi[yNew, Progression_end + 1] <- max(MyData[, y], na.rm = TRUE)
    sumMean[yNew, Progression_end + 1] <- mean(MyData[, y], na.rm = TRUE)
    sumMedian[yNew, Progression_end + 1] <- median(MyData[, y], na.rm = TRUE)
    sumSD[yNew, Progression_end + 1] <- sqrt(var(MyData[, y], y = NULL))
    sumTab <- tabulate(MyData[, y] * 1000)
    dTtest <- which(sumTab == max(sumTab, na.rm = TRUE))/1000
    sumT[yNew, Progression_end + 1] <- dt
    if (length(dTtest) > 1) {
      sumMode[yNew, Progression_end + 1] <- mean(dTtest, na.rm = TRUE)
    }
    else {
      sumMode[yNew, Progression_end + 1] <- dTtest
    }
  }
  Page <- 0
  if (oneCycle[1] == 0) {
    if (Period$End <= 0 || FreqInc <= 0) {
      message <- paste("ERROR:  Period$End (", Period$End, ") and Period$Increment (", FreqInc, ") cannot be 0 when Period$Set is 0.\n")
      errMsg <- paste(errMsg, message)
      closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
      stop(message)
    }
    if (Period$Start < Period$End) {
      message <- paste("ERROR:  The parameter Period$Start (", Period$Start, ") cannot be smaller than Period$End (", Period$End, ") when Period$Set is 0.\n")
      errMsg <- paste(errMsg, message)
      closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
      stop(message)
    }
    if (FreqInc > Period$Start) {
      message <- paste("ERROR:  Period$Increment (", FreqInc, ") cannot be larger than Period$Start (", Period$Start, ") (or Progressive$Interval) when Period$Set is 0.\n")
      errMsg <- paste(errMsg, message)
      closeOutput(file = fileName4, output = Output, console = Console, opar = opar, ERR = TRUE, errMsg = errMsg, paramMsg = paramMsg)
      stop(message)
    }
    else if (FreqInc > 1) {
      message <- "Warning:  Chosen Increment (>1) is less than optimal.  Increments would optimally be <=1.\n"
      print(message)
      errMsg = paste(errMsg, message)
    }
    RowCnt <- floor(1 + ((Period$Start/Period$End) - 1)/FreqInc)
    RowCntAlt2 <- floor(1 + ((1/Period$End)/(1/Period$Start) - 1)/FreqInc)
    if (RowCnt < 1) {
      RowCnt <- 1
    }
  } else {
    if (Components > 1) {
      RowCnt <- 1
    }
    else {
      RowCnt <- length(oneCycle)
    }
    print("resetting freq start and inc")
  }
  for (y in 1:Ys_end) {
    Ycol <- Y[y]
    paramMsg <- paste("\n  TimeCol=", TimeCol, ",  Y=", Y, ",  header=", header, "\n --  Periods=", Period["Set"], ", Units=", Units, ",  Interval=", format(Interval, nsmall = 3), ",  Increment=", format(Increment, nsmall = 3), "\nPeriod$Start=", format(Period$Start, nsmall = 3), ",  FreqInc=", format(FreqInc, nsmall = 3), ",  Period$End=", format(Period$End, nsmall = 3), "\nRefDateTime=", RefDateTime, ", StartTime=", format(StartTime, nsmall = 3), ", EndTime=", format(EndTime, nsmall = 3), "Percent of missing (blank) sample values: %", 
                      missingData, "\n", functionName, "\n")
    n <- length(MyData[, Ycol])
    if (is.numeric(MyData[n, TimeCol])) {
      startDateP <- as.POSIXct(strptime(x = MyData[1, TimeCol], format = "%Y%m%d%H%M", tz = tz))
      endDateP <- as.POSIXct(strptime(x = MyData[n, TimeCol], format = "%Y%m%d%H%M", tz = tz))
    } else {
      startDateP <- MyData[1, TimeCol]
      endDateP <- MyData[n, TimeCol]
    }
    cat("\n\tPR\t\t   F\t\t\tP \tSS[j]\t i\t cycle \t  Mesor \t  s.e. \t \t  Amp \t\t s.e. \t \tPhi\t\t s.e.\n")
    realLCM <- LCM
    if (minPeriod == 0) {
      modelLen <- 360
    } else {
      modelLen <- (LCM/minPeriod) * 6
      if (LCM > MyData_hours) {
        LCM <- MyData_hours
        modelLen <- (LCM/minPeriod) * 24
      }
    }
    if (modelLen < 360) {
      modelLen <- 360
    }
    M <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    Model_Y <- matrix(data = NA, nrow = RowCnt, ncol = modelLen)
    Model_Y_mag <- matrix(data = NA, nrow = RowCnt, ncol = modelLen)
    plotModel <- vector(mode = "numeric", length = modelLen)
    multi_Model_Y <- matrix(data = NA, nrow = RowCnt, ncol = modelLen)
    multi_Model_mag <- matrix(data = NA, nrow = RowCnt, ncol = modelLen)
    newPR <- array(data = NA, c(RowCnt, Progression_end, Components))
    PHI <- array(data = NA, c(RowCnt, Progression_end, Components))
    PHIr <- array(data = NA, c(RowCnt, Progression_end, Components))
    A <- array(data = NA, c(RowCnt, Progression_end, Components))
    PR <- array(data = NA, c(RowCnt, Progression_end, Components))
    P <- array(data = NA, c(RowCnt, Progression_end, Components))
    testPg <- array(data = NA, c(RowCnt, Progression_end, Components))
    testPb <- array(data = NA, c(RowCnt, Progression_end, Components))
    F <- array(data = NA, c(RowCnt, Progression_end, Components))
    mesor_se <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    phi_se <- array(data = NA, c(RowCnt, Progression_end, Components))
    amp_se <- array(data = NA, c(RowCnt, Progression_end, Components))
    Cycle <- array(data = NA, c(RowCnt, Progression_end, Components))
    Err <- array(data = "", c(RowCnt, Progression_end, Components))
    hours <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    nPts <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    sPts <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    time <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    yVar <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    MSS <- vector(mode = "integer", length = Components)
    printP <- array(data = NA, c(RowCnt, Progression_end, Components))
    if (Components > 1) {
      multi_P <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
      multi_PR <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
      Magnitude <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    }
    Orthophase <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    Bathyphase <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    newData <- matrix(data = NA, nrow = RowCnt, ncol = Progression_end)
    Page <- 0
    for (j in 1:Progression_end) {
      maxAmp <- max2Amp <- max3Amp <- 1
      StartSpanTime <- StartTime + StartSpans[j]
      EndSpanTime <- StartSpanTime + Interval
      TimeIdx <- which(StartSpanTime <= MyData$time.hour)
      if (is.na(TimeIdx[1])) {
        StartIdx <- length(MyData$time.hour)
      }
      else StartIdx <- TimeIdx[1]
      if (EndSpanTime > MyData_hours + StartTime) {
        EndSpanTime <- MyData_hours + StartTime
      }
      if (Progression_end == j) {
        TimeIdx <- which(round(EndSpanTime, digits = 10) >= round(MyData$time.hour, digits = 10))
      }
      else TimeIdx <- which(EndSpanTime > MyData$time.hour)
      EndIdx <- which.max(TimeIdx)
      Interval_hours <- EndSpanTime - StartSpanTime
      thisIdxCnt <- EndIdx - StartIdx + 1
      Loops <- RowCnt
      if (Components == 1 && oneCycle[1] > 0) {
        Cycles <- oneCycle
        Loops <- length(Cycles)
      }
      else if (Components > 1) {
        Cycles <- oneCycle
        Loops <- 1
      }
      newData <- MyData[StartIdx:EndIdx, Ycol]
      sumN[y, j] <- length(newData)
      sumLow[y, j] <- min(newData, na.rm = TRUE)
      sumHi[y, j] <- max(newData, na.rm = TRUE)
      sumMean[y, j] <- mean(newData, na.rm = TRUE)
      sumMedian[y, j] <- median(newData, na.rm = TRUE)
      sumSD[y, j] <- sqrt(var(newData, y = NULL))
      sumTab <- tabulate(newData * 1000)
      dTtest <- which(sumTab == max(sumTab, na.rm = TRUE))/1000
      sumT[y, j] <- dt
      if (length(dTtest) > 1) {
        sumMode[y, j] <- mean(dTtest, na.rm = TRUE)
      } else {
        sumMode[y, j] <- dTtest
      }
      for (i in 1:Loops) {
        if (oneCycle[1] == 0) {
          cycle <- Period$Start/(1 + ((i - 1) * FreqInc))
        }
        else cycle <- Cycles[i]
        if (IDcol != "fileName") {
          if (!is.numeric(IDcol)) {
            SubjectID[i] <- " "
          }
          else {
            SubjectID[i] <- MyData[StartIdx, IDcol]
          }
        }
        Cycle[i, j, 1] <- cycle
        yVar[i, j] <- names(MyData)[Y[y]]
        hours[i, j] <- paste(format(StartSpanTime, nsmall = 1), " - ", format(EndSpanTime, nsmall = 1), sep = "")
        cycleCnt <- (EndSpanTime - StartSpanTime)/cycle
        minPtCnt <- 2 * cycleCnt + 1
        nPts[i, j] <- EndIdx - StartIdx + 1
        sPts[i, j] <- paste(StartIdx, "-", EndIdx, "\n(", nPts[i, j], ")")
        time[i, j] <- paste(MyData[StartIdx, TimeCol], "-\n", MyData[EndIdx, TimeCol])
        if (nPts[i, j] <= minPtCnt) {
          print(paste(keyMsgs[5], "C=", 2 * cycleCnt + 1, "min pts=", minPtCnt))
          Err[i, j, ] <- paste(Err[i, j, ], errKey[5], "min pts=", minPtCnt)
        }
        if (cycle > Interval) {
          if ((0.9 * cycle) > (MyData$time.hour[EndIdx] - MyData$time.hour[StartIdx])) {
            print(keyMsgs[7])
            Err[i, j, ] <- paste(Err[i, j, ], errKey[7])
          }
          else {
            print(keyMsgs[8])
            Err[i, j, ] <- paste(Err[i, j, ], errKey[8])
          }
        }
        CosMatrix <- matrix(data = NA, nrow = CosMatrixDim, ncol = CosMatrixDim)
        testXmean <- matrix(data = NA, nrow = CosMatrixDim, ncol = Components)
        YMatrix <- matrix(data = NA, nrow = CosMatrixDim, ncol = 1)
        CosCoMatrix1 <- matrix(data = NA, nrow = CosMatrixDim, ncol = thisIdxCnt)
        CosCoMatrix1[1, ] <- rep(x = 1, times = thisIdxCnt)
        for (m in 1:Components) {
          if (oneCycle[1] > 0) {
            if (LCM <= Interval) {
              dTime <- LCM/modelLen
            }
            else {
              dTime <- Interval/modelLen
            }
            magTime <- vector(mode = "integer", length = modelLen)
            magDt <- (modelLen * dTime)
            magTime <- seq(from = StartSpanTime, by = dTime, to = (StartSpanTime + (magDt - dTime)))
          }
          if (Components > 1) {
            cycle <- Cycles[m]
            Cycle[i, j, m] <- cycle
          }
          CosCoMatrix1[m * 2, ] <- cos(MyData$time.hour[StartIdx:EndIdx] * (2 * pi)/cycle)
          CosCoMatrix1[m * 2 + 1, ] <- sin(MyData$time.hour[StartIdx:EndIdx] * (2 * pi)/cycle)
          YMatrix[m * 2] <- sum(newData %*% (cos(MyData$time.hour[StartIdx:EndIdx] * (2 * pi)/cycle)))
          YMatrix[m * 2 + 1] <- sum(newData %*% (sin(MyData$time.hour[StartIdx:EndIdx] * (2 * pi)/cycle)))
        }
        for (m in 1:CosMatrixDim) {
          for (o in 1:CosMatrixDim) {
            CosMatrix[o, m] <- sum(CosCoMatrix1[m, ] * CosCoMatrix1[o, ])
          }
        }
        CosMatrix[1, 1] <- thisIdxCnt
        YMatrix[1] <- sum(newData)
        mdat <- CosMatrix
        mdat2 <- YMatrix
        if (any(is.na(mdat))) {
          Err[i, j, ] <- paste(Err[i, j, ], errKey[1])
          print(keyMsgs[1])
          next
        }
        else {
          if (det(mdat) < 0.00000000001) {
            Err[i, j, ] <- paste(Err[i, j, ], errKey[2])
            print(keyMsgs[2])
            next
          }
          else {
            mdatInv <- solve(mdat)
            coefs <- mdatInv %*% mdat2
            if (Debug == TRUE || i == Interval) {
              print(mdat)
              print(mdat2)
              print(mdatInv)
              cat("matrix coefs ", coefs, "\n")
            }
            M[i, j] <- coefs[1]
            MeanR <- mean(newData, na.rm = TRUE)
            multi_Model_mag <- multi_Model_Y <- Model_Y <- M[i, j]
            for (m in 1:Components) {
              beta <- m * 2
              gamma <- m * 2 + 1
              if (Components > 1) {
                cycle <- Cycles[m]
                Cycle[i, j, m] <- cycle
              }
              if (!is.na(coefs[beta]) && !is.na(coefs[gamma])) {
                A[i, j, m] <- (coefs[beta]^2 + coefs[gamma]^2)^0.5
                if (is.na(A[maxAmp, j, m])) {
                  maxAmp <- i
                }
                else if (A[i, j, m] > A[maxAmp, j, m]) {
                  max3Amp <- max2Amp
                  max2Amp <- maxAmp
                  maxAmp <- i
                }
                else if (A[i, j, m] > A[max2Amp, j, m]) {
                  max3Amp <- max2Amp
                  max2Amp <- i
                }
                else if (A[i, j, m] > A[max3Amp, j, m]) {
                  max3Amp <- i
                }
                Model_Y = M[i, j] + (coefs[beta] * cos(2 * pi * MyData$time.hour[StartIdx:EndIdx]/cycle)) + (coefs[gamma] * sin(2 * pi * MyData$time.hour[StartIdx:EndIdx]/cycle))
                if (oneCycle[1] > 0) {
                  Model_Y_mag[i, ] = M[i, j] + (coefs[beta] * cos(2 * pi * magTime/cycle)) + (coefs[gamma] * sin(2 * pi * magTime/cycle))
                }
                if (Components > 1) {
                  multi_Model_Y = multi_Model_Y + (coefs[beta] * cos(2 * pi * MyData$time.hour[StartIdx:EndIdx]/cycle)) + (coefs[gamma] * sin(2 * pi * MyData$time.hour[StartIdx:EndIdx]/cycle))
                  multi_Model_mag = multi_Model_mag + coefs[beta] * cos(2 * pi * magTime/Cycles[m]) + coefs[gamma] * sin(2 * pi * magTime/Cycles[m])
                }
                MYhat_Ymean <- Model_Y - MeanR
                MSS[m] <- sum(MYhat_Ymean^2)
                yTSS <- newData - MeanR
                TSS_2 <- sum((yTSS)^2)
                newPR[i, j, m] <- 100 * (coefs[beta] * sum(yTSS * cos(2 * pi * MyData$time.hour[StartIdx:EndIdx]/cycle)) + coefs[gamma] * sum(yTSS * sin(2 * pi * MyData$time.hour[StartIdx:EndIdx]/cycle)))/TSS_2
                if (newPR[i, j, m] < 0) {
                  Err[i, j, m] <- paste(Err[i, j, m], errKey[3])
                  print(keyMsgs[3])
                }
                ph <- atan(-coefs[gamma]/coefs[beta])
                if (coefs[beta] >= 0) {
                  phi2 <- ph
                }
                if (coefs[beta] < 0 & coefs[gamma] >= 0) {
                  phi2 <- ph + pi
                }
                if (coefs[beta] < 0 & coefs[gamma] < 0) {
                  phi2 <- ph - pi
                }
                PHIr[i, j, m] <- phi2
                if (phi2 < 0) {
                  PHIr[i, j, m] <- phi2 + (2 * pi)
                }
                if (phi2 > (2 * pi)) {
                  PHIr[i, j, m] <- phi2 - (2 * pi)
                }
                PHI[i, j, m] <- (PHIr[i, j, m]/(2 * pi)) * 360
                if (PHI[i, j, m] > 0) {
                  PHI[i, j, m] <- PHI[i, j, m] - 360
                }
              }
              else {
                Err[i, j, m] <- paste(Err[i, j, m], errKey[4])
                print(keyMsgs[4])
                next
              }
            }
            MY_Yhat <- newData - Model_Y
            RSS <- sum(MY_Yhat^2)
            if (RSS == 0) {
              print(keyMsgs[6])
              Err[i, j, ] <- paste(Err[i, j, ], errKey[6])
              next
            }
            df1 <- Components * 2
            df2 <- thisIdxCnt - CosMatrixDim
            if (df2 < 0) {
              print(paste(keyMsgs[9], "2*Components+1=", CosMatrixDim, "thisIdxCnt=", thisIdxCnt))
              Err[i, j, ] <- paste(Err[i, j, ], errKey[9], "thisIdxCnt=", thisIdxCnt)
              next
            }
            else if (df2 < 1) {
              print(paste(keyMsgs[10], "2*Components+1=", CosMatrixDim, "thisIdxCnt=", thisIdxCnt))
              Err[i, j, ] <- paste(Err[i, j, ], errKey[10], "thisIdxCnt=", thisIdxCnt)
              df2 <- NA
            }
            if (Components > 1) {
              multi_F <- (multi_MSS/df1)/(RSS/(df2))
              multi_P[i, j] <- 1 - pf(multi_F, df1 = df1, df2 = df2)
              multi_PR[i, j] <- (multi_MSS/(RSS + multi_MSS)) * 100
              cat("F-multi:", multi_F, "\n")
            }
            SigmaHat <- (RSS/(df2))^0.5
            mesor_se[i, j] <- SigmaHat * mdatInv[1, 1]^0.5
            for (m in 1:Components) {
              F[i, j, m] <- (MSS[m]/2)/(RSS/(df2))
              P[i, j, m] <- 1 - pf(F[i, j, m], df1 = 2, df2 = df2)
              cat("F:df1-df2", F[i, j, m], 2, df2, "MSS", MSS[m], "RSS", RSS, "\n")
              tInd1 <- 2 * m
              tInd2 <- 2 * m + 1
              beta <- 2 * m
              gamma <- 2 * m + 1
              test <- (mdatInv[tInd2, tInd2] * coefs[beta]^2) + 2 * mdatInv[tInd1, tInd2] * coefs[beta] * coefs[gamma] + mdatInv[tInd1, tInd1] * coefs[gamma]^2
              test2 <- test/(mdatInv[tInd1, tInd1] * mdatInv[tInd2, tInd2] - mdatInv[tInd1, tInd2]^2)
              test3 <- test2/(2 * SigmaHat^2)
              testPg[i, j, m] <- 1 - pf(test3, df1 = 2, df2 = df2)
              testXmean <- (CosMatrix[1, tInd1])/thisIdxCnt
              testZmean <- (CosMatrix[1, tInd2])/thisIdxCnt
              testX2 <- sum((CosCoMatrix1[tInd1, ] * CosCoMatrix1[1, ] - testXmean)^2)
              testZ2 <- sum((CosCoMatrix1[tInd2, ] * CosCoMatrix1[1, ] - testZmean)^2)
              testXZ <- sum((CosCoMatrix1[tInd1, ] * CosCoMatrix1[1, ] - testXmean) * (CosCoMatrix1[tInd2, ] * CosCoMatrix1[1, ] - testZmean))
              testF <- (testX2 * coefs[beta]^2 + 2 * testXZ * coefs[beta] * coefs[gamma] + testZ2 * coefs[gamma]^2)/(2 * SigmaHat^2)
              testPb[i, j, m] <- 1 - pf(testF, df1 = 2, df2 = df2)
              if (Components > 1) {
                PR[i, j, m] <- (MSS[m]/(RSS + multi_MSS)) * 100
              }
              else {
                PR[i, j, m] <- (MSS[m]/(RSS + MSS[m])) * 100
              }
              amp_se[i, j, m] <- SigmaHat * (mdatInv[beta, beta] * cos(PHIr[i, j, m])^2 - (2 * mdatInv[beta, gamma] * cos(PHIr[i, j, m]) * sin(PHIr[i, j, m])) + mdatInv[gamma, gamma] * sin(PHIr[i, j, m])^2)^0.5
              phi_se[i, j, m] <- (SigmaHat * (mdatInv[beta, beta] * sin(PHIr[i, j, m])^2 + (2 * mdatInv[beta, gamma] * cos(PHIr[i, j, m]) * sin(PHIr[i, j, m])) + mdatInv[gamma, gamma] * cos(PHIr[i, j, m])^2)^0.5)/A[i, j, m]
              phi_se[i, j, m] <- phi_se[i, j, m] * 180/pi
              if (Debug == TRUE) {
                cat(" seM ", mesor_se[i, j], " seA ", amp_se[i, j, m], " sePHI ", phi_se[i, j, m], " \n")
                cat("test3", m, "   ", test3, "\n")
                cat("testPg", m, "   ", testPg[i, j, m], "\n")
                cat("testF", m, "   ", testF, "\n")
                cat("testPb", m, "   ", testPb[i, j, m], "\n")
              }
              jht <- 7.6 - (Page * 3 - 5)/j * 2
              cexVar <- 0.7
              cexMain <- 0.95
              htVar <- 0.62
              hdrRow <- 10
              cat(thisIdxCnt, "\t", "sigma ", SigmaHat, "\t", PR[i, j, m], "\t", F[i, j, m], "\t ", P[i, j, m], "\t \n")
              cat(StartSpans[j], "\t", i, "\t", format(Cycle[i, j, m], width = 3), "\t ", M[i, j], "\t", format(mesor_se[i, j], digits = 8), "\t", A[i, j, m], "\t", format(amp_se[i, j, m], digits = 8), "\t", format(PHI[i, j, m], digits = 8), "\t", format(phi_se[i, j, m], digits = 8), "\n")
              if (oneCycle[1] > 0) {
                ht <- hdrRow + 0.5 - (i + (j * htVar)) + jht - (m - 1)
              }
            }
          }
        }
      }
      if (Interval_hours < 48) {
        by.x <- 2
      }
      else if (Interval_hours < 200) {
        by.x <- 4
      }
      else if (Interval_hours < 400) {
        by.x <- 8
      }
      else if (Interval_hours < 800) {
        by.x <- 12
      }
      else if (Interval_hours < 1000) {
        by.x <- 16
      }
      else if (Interval_hours < 1200) {
        by.x <- 20
      }
      else by.x <- 40
      if (oneCycle[1] == 0) {
        if (j <= 28) {
          jMult <- j
        }
        else {
          jMult <- (j + 3)%%16 + 12.5
        }
        ht <- hdrRow - (jMult * htVar) + jht
        ht <- hdrRow - 0.2 - (jMult * htVar) + jht
        ht <- hdrRow - 0.4 - (jMult * htVar) + jht
      }
      else {
        if (yLabel == "") {
          printYlab <- names(MyData)[Y[y]]
          if (header == FALSE) {
            printYlab <- paste("Column", Y[y])
          }
        }
        else {
          printYlab <- yLabel
        }
        plotModel <- Model_Y_mag
        plotStart <- StartSpanTime * 3600
        plotEnd <- EndSpanTime * 3600
        plotTitle <- "Single-component model:"
        cosPalette <- "blue"
        cycleCnt <- length(oneCycle)
        if (Components == 1) {
          if (cycleCnt > 5) {
            cycleCnt <- 1
          }
          if (cycleCnt > 1) {
            cosPalette <- rainbow(14)
          }
          cosPalette[1] <- "blue"
        }
        if ((realLCM * 0.99) <= LCM && LCM <= (realLCM * 1.01)) {
          Bathyphase[i, j] <- 0 - which.min(plotModel)
          bathyTime <- RefTime + (which.min(plotModel) * dTime * 3600)
          Orthophase[i, j] <- 0 - which.max(plotModel)
          orthoTime <- RefTime + (which.max(plotModel) * dTime * 3600)
        }
        else {
          Bathyphase[i, j] <- NA
          bathyTime <- NA
          Orthophase[i, j] <- NA
          orthoTime <- NA
        }
        if (GraphSet$Model) {
          dateRange <- paste("Data range: ", format(MyData$time[StartIdx], "%m/%d/%Y %H:%M"), " to ", format(MyData$time[EndIdx], "%m/%d/%Y %H:%M"), "\nData analysis range: ", format(RefTime + plotStart, "%m/%d/%Y %H:%M"), " to  ", format(RefTime + plotEnd, "%m/%d/%Y %H:%M"))
          if (Components > 1 || cycleCnt == 1) {
            if ((realLCM * 0.99) <= LCM && LCM <= (realLCM * 1.01)) {
              printXlab <- list(bquote("Hours from Reference Time:  " * .(RefTimeString) * ";       One Model cycle = " * .(format(realLCM, digits = 3, nsmall = 2)) * " hrs      o " * .(Orthophase[i, j]) * degree * "     b " * .(Bathyphase[i, j]) * degree))
            }
            else {
              printXlab <- paste("Hours from Reference Time:  ", RefTimeString, ";       One Model cycle = ", format(realLCM, digits = 3, nsmall = 2))
            }
          }
          else {
            printXlab <- paste("Hours from Reference Time:  ", RefTimeString)
          }
          hi_range <- max(pmax(plotModel))
          lo_range <- min(pmin(plotModel))
          y_range <- c(lo_range, hi_range)
          CyclesLCM <- as.numeric(Interval_hours/LCM)
          if (j == 1) {
            cexLab <- 1
            cexAxis <- 0.9
          }
          else {
            cexLab <- 0.8
            cexAxis <- 0.7
          }
          if (Interval_hours < 500 && CyclesLCM < 100) {
            if (max(newData) > max(pmax(plotModel))) {
              hi_range <- max(pmax(newData))
            }
            if (min(newData) < min(pmin(plotModel))) {
              lo_range <- min(pmin(newData))
            }
            y_range <- c(lo_range, hi_range)
            x_range <- c(StartSpanTime, EndSpanTime)
            waves <- round(CyclesLCM * modelLen)
            for (C in 1:cycleCnt) {
              tempModel <- plotModel
              if (Components == 1) {
                tempModel <- plotModel[C, ]
              }
            }
            label.x <- seq(from = StartSpanTime, to = EndSpanTime, by = by.x)
            at.x <- (1/dTime) * (label.x - StartSpanTime)
          }
          else {
            tempModel <- plotModel
            x_range <- c(0, modelLen)
            for (C in 1:cycleCnt) {
              if (Components == 1) {
                tempModel <- plotModel[C, ]
              }
            }
            if (LCM < 48) {
              by.xx <- 2
            }
            else if (LCM < 200) {
              by.xx <- 4
            }
            else if (LCM < 400) {
              by.xx <- 8
            }
            else if (LCM < 800) {
              by.xx <- 12
            }
            else if (LCM < 1000) {
              by.xx <- 16
            }
            else if (LCM < 1200) {
              by.xx <- 20
            }
            else by.xx <- 40
            label.x <- seq(from = 0, to = LCM, by = by.xx) + StartSpanTime
            at.x <- (1/dTime) * (label.x - StartSpanTime)
          }
          ht <- 6 - (j * htVar) + jht
        }
      }
    }
    startDateP <- as.POSIXct(strptime(x = StartDate, format = "%Y%m%d%H%M", tz = tz))
    endDateP <- as.POSIXct(strptime(x = EndDate, format = "%Y%m%d%H%M", tz = tz))
    yTitle <- Y[y]
    if (header == TRUE) {
      if (length(names(myHeader)) == 0) {
        yTitle <- names(myHeader)[Y[y]]
      }
    }
    m <- Components
    if ((nrow(A) > 1 || m > 1) & ncol(A) > 1 & GraphSet$HeatMap & suppressWarnings(any(GraphSet))) {
      if (m == 1) {
        mapx <- 1/Cycle[, 1, 1:m]
        mapx2 <- c(1:RowCnt)
        ampz <- A[1:i, , m]
        ampz[1, ] <- 1
        ampz[i, ] <- 1
      } else {
        mapx <- 1/Cycle[, 1, 1:m]
        mapx2 <- c(1:m)
        ampz <- t(A[i, , 1:m])
      }
      mapy <- StartSpans + StartTime + (Interval/2)
      tickCnt <- format(seq(from = StartSpans[1] + StartTime + (Interval/2), to = MyData_hours + StartTime - (Interval/2), by = 24), digits = 4)
      if (Period$Set[1] == 0) {
        HeatYlab <- paste("Period (hours): ", Period$Start, "/1, ", Period$Start, "/", 1 + FreqInc, " . . . to ", Period$Start, "/", format((1 + ((i - 1) * FreqInc)), digits = 4, nsmall = 2), "=", format(cycle, digits = 4, nsmall = 2), ", ", "Harmonic increment:", FreqInc)
        HeatYlab2 <- paste("Frequency (cycles/", Period$Start, "hour):  1, ", FreqInc + 1, ", . . . to ", format(RowCnt, digits = 4, nsmall = 1), ", ", "Harmonic increment:", FreqInc)
      } else {
        HeatYlab <- paste("Period (hours):  (At", Period["Set"], "periods)")
        HeatYlab2 <- paste("Frequency (cycles/", Period$Start, "hour):  (At", Period["Set"], "periods)")
        mapx <- sort(mapx, decreasing = FALSE)
      }
      lvlCnt <- 10
      colCount <- 100
      Nscale <- ((0:colCount)/colCount)
      Nbw <- gray(Nscale)
      HeatMain <- titleMain
      HeatXlab <- paste("Time (hours from reference time, ", RefTime, ")\n", "Interval length: ", Interval, Units, "   Increment: ", Increment, Units)
      HeatLegend <- Nscale * max(ampz, na.rm = TRUE)
      HeatLegend2 <- c(HeatLegend, max(ampz, na.rm = TRUE) + max(A[2:(i - 1), 1, m], na.rm = TRUE)/colCount)
      HeatIndex <- rep(1:(colCount/lvlCnt), lvlCnt)
      HeatLegend3 <- (HeatLegend2[HeatIndex == 1])
      NbwFill <- (Nbw[HeatIndex == 1])
      if (Colors == "BW") {
        colorVar <- rev(Nbw)
        fillVar <- rev(NbwFill)
      } else {
        if (Colors == "Heat") {
          colorVar <- rev(heat.colors(colCount))
          fillVar <- rev(heat.colors(lvlCnt + 1))
        } else {
          colorVar <- rev(terrain.colors(colCount))
          fillVar <- rev(terrain.colors(lvlCnt + 1))
        }
      }
      lambda <- 0.025
      rightAxis <- c(1:RowCnt)
    }
    if (length(which(GraphSet == T)) > 0) {
      if (suppressWarnings(any(GraphSet))) {
        if (j > 1) {
          layoutSize <- 8
        } else {layoutSize <- 8}
        if (FreqInc != 1) {
          Ylable_inc <- paste("; harmonic increment:", FreqInc, "cycles")
        } else {
          Ylable_inc <- " "
        }
        if (j > 1) {
          for (l in m) {
            for (k in 1:Loops) {
              cat(k, j, l, "\n")
              lineX <- StartSpans + StartTime + (Interval/2)
              if ((length(which(GraphSet == T)) == 1 && GraphSet$Data && l == 1 && k == 1) || length(which(GraphSet == T)) > 2) {
                if (m == 1) {
                  mainX <- paste("Column ", Ycol, ";  Period", Cycle[k, j, l], ";   Time (", Units, ") from reference date: ", RefTime)
                }  else {
                  mainX <- paste("Column ", Ycol, ";  Components", list(Cycle[, j, ]), ":   Time (", Units, ") from reference date: ", RefTime)
                }
                if (GraphSet$Data && (l == 1 || l == m) && k == 1) {
                  g_range <- base::range(MyData$time.hour[1:EndIdx])
                  at.x <- seq(from = round(g_range[1]), to = round(g_range[2]), by = by.x)
                }
                g_range <- base::range(lineX)
                if (m == 1) {
                  if (GraphSet$MESOR == T && !all(is.na(M[k, ])) && l == 1) {
                  }
                  if (GraphSet$Amp && !all(is.na(A[k, , l]))) {
                    ampMax <- which.max(A[k, , l])
                    ampMin <- which.min(A[k, , l])
                    seMax <- which.max(amp_se[k, , l])
                    seMin <- which.min(amp_se[k, , l])
                  }
                  if (GraphSet$Phi && !all(is.na(PHI[k, , l]))) {
                  }
                }
                else {
                  if (GraphSet$Amp && !all(is.na(Magnitude[k, ]))) {
                  }
                }
              }
            }
          }
        } else {
          CycleLen <- length(Cycle)
          cat("cycle", CycleLen)
          if (CycleLen > 6) {
            titleLable2 <- paste("Periods:", format(Cycle[i, 1, ], digits = 4, nsmall = 2), " to ", format(Cycle[1, 1, ], digits = 4, nsmall = 2), Units, Ylable_inc)
          } else {
            titleLable2 <- paste("Periods:", list(Cycle[, 1, ]), Units, Ylable_inc)
          }
          if (GraphSet$Data) {
            g_range <- base::range(MyData$time.hour[1:EndIdx])
            at.x <- seq(from = round(g_range[1]), to = round(g_range[2]), by = by.x)
          }
          if (m > 1 || i > 1) {
            if (GraphSet$Amp && !all(is.na(A[CycleLen:1]))) {
              AmpRange <- c((A[CycleLen:1] - amp_se[CycleLen:1]), (A[CycleLen:1] + amp_se[CycleLen:1]))
            }
          }
        }
      }
    }
  }
  PHI <<- as.vector(PHI)
  PHI.SE <<- as.vector(phi_se)
  PVAL <<- as.vector(P)
  AMP <<- as.vector(A)
  AMP.SE <<- as.vector(amp_se)
  waves <<- as.vector(waves)
  PR <<- as.vector(PR)
  tempModel <<- tempModel
  y_range <<- y_range
  mesor <<- as.vector(M)
  rm(GraphSet)
}

