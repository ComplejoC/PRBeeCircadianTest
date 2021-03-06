period_analysis <- function(df = NULL, sampling= 60, newsampling=60, TimeCol=1, p.alpha=0.05, multipeak_period = F, 
                            phase_ref=0, datacolumn=2, days.movingwindow=3, hoursgap= 30, slide.movingwindow=1, 
                            smoothinghours=4,lomb.period=c(18,30), correlation_method = "lombout", oversampling_factor = 60,
                            peak_of_interest = 2, detrend = TRUE){
  
  
  
  library(rgr)
  library(CircadianDynamics)
  library(tidyverse)
  library(lubridate)
  library(rstudioapi)
  
  print("Select the folder where you want to run the analysis")
  directory <- selectDirectory()
  
  pertest = list()
  
  for ( i in 2:ncol(df)) {
    
    data <- df[, c(1,i)]
    new_dir1 <- paste0(directory,"/analysis")
    new_dir2 <- paste0(directory,"/analysis/",names(df)[i])
    filename <- names(df)[i]
    filePath <- new_dir2
    dir.create(new_dir1)
    dir.create(new_dir2)
    setwd(new_dir2)
    
    
    
    
    
    
    
    # Loading required libraries   
    # packages = c('rstudioapi', 'rgr', 'CicadianDynamics')
    # for (package in packages) {
    #   print(package)
    #   if (require(package)) {
    #     library(package)
    #   }else{install.packages(package)}}
    
    
    
    # analysis.start is the time when this function started running
    analysis.start <<- as.character(Sys.time())
    analysis.start <<- gsub(" ", "___", analysis.start)
    analysis.start <<- gsub(":", "-", analysis.start)
    
    
    # Multipeak peariod: Determines whether to identify period in the autocorrelation using one peak or
    # all peaks of the data. If false, one peak around 24 hours will be looked for. The function will
    # stop working if a value other than TRUE or FALSE is given.
    
    if(multipeak_period==F){
      peakmethod <- "Onepeak at 24 hours"
    } else if (multipeak_period== T ) {
      peakmethod <- "All peaks and average of differences"
    } else {
      peakmethod <- "Onepeak at 24 hours"
      warning("multipeak_period: Input is not defined, it must be True or False. Using default, False.")
    }
    
    
    # Time of day to use as reference when calculating the phase. Currently, this part of the code
    #  only works if phase_ref = 0.
    
    if(phase_ref==0){
      
      ref.phase <- "Midnight of first day"
      parameters <- c("sampling","newsampling","TimeCol","p.alpha",
                      "datacolumn","days.movingwindow","hoursgap","slide.movingwindow",
                      "smoothinghours","lomb.period start","lomb.period end","autocorrelation period id method",
                      "phase_ref")
      param_values <- c(sampling,newsampling,TimeCol,p.alpha,datacolumn,days.movingwindow,hoursgap,slide.movingwindow,smoothinghours,lomb.period[1],lomb.period[2],peakmethod,ref.phase)
      inputparam <- data.frame(parameters,param_values)
      rm(ref.phase)
    }
    
    
    
    # Installs and loads packages required to run the analysis
    aa_install_packages()
    aaa_load_required_packages()
    
    
    # Column where the desired parameter for analysis is found.    
    Valcols <- datacolumn
    
    # Transform the data to a new desired sampling rate. newsampling will be a number
    # for the new rate. hoursgap is the maxamum hours of missing values allowed.
    zerobins <- hoursgap*(60/newsampling)
    
    # for the moving average, smoothinghours will determine how many data points to average together.
    n <- smoothinghours*(60/newsampling)
    
    # By calling scipen = 15 on the options function, we are disabling scientific notation for numbers with less
    # than 15 digits. This will make it easier to do math.
    
    options(scipen=15)
    
    
    
    
    
    # Import the file, the first row must be variable names.
    sensor <- data
    
    
    # This chunk of code should be together with the cunck that also manages the values of 
    # phase_ref.
    if(phase_ref != 0){
      if(is.na(phase_ref)==T){
        ref.phase <- sensor[1,1]
      }
      ref.phase <- phase_ref
      parameters <- c("sampling","newsampling","TimeCol","p.alpha","datacolumn",
                      "days.movingwindow","hoursgap","slide.movingwindow","smoothinghours",
                      "lomb.period start","lomb.period end","autocorrelation period id method","phase_ref")
      param_values <- c(sampling,newsampling,TimeCol,p.alpha,datacolumn,days.movingwindow,hoursgap,
                        slide.movingwindow,smoothinghours,lomb.period[1],lomb.period[2],peakmethod,ref.phase)
      inputparam <- data.frame(parameters,param_values)
      rm(ref.phase)
    }
    
    
    # Modify the datetime in the date column in a readable and consistent format. 
    date.time <- str_split_fixed(pull(sensor[,1])," ",n=2)
    
    
    
    gooddates <- ymd(date.time[,1])
    gooddates <- as.character(gooddates)
    date.time[,1] <- gooddates
    dates <- str_split_fixed(date.time[,1],"-",n=3)
    dates[,3] <- formatC(as.numeric(as.character(dates[,3])),width=2,format="d",flag="0")
    dates[,2] <- formatC(as.numeric(as.character(dates[,2])),width=2,format="d",flag="0")
    dates[,1] <- as.character(dates[,1])
    
    
    
    times <- str_split_fixed(date.time[,2],":",n=3)
    times[,1] <- formatC(as.numeric(as.character(times[,1])),width=2,format="d",flag="0")
    times[,2] <- formatC(as.numeric(as.character(times[,2])),width=2,format="d",flag="0")
    times[,3] <- formatC(as.numeric(as.character(times[,3])),width=2,format="d",flag="0")
    test1 <- "NA"
    if(unique(times[,3])==test1){
      datetime <- paste(dates[,1],dates[,2],dates[,3],times[,1],times[,2],sep="")
    }else{
      #times[,3] <- formatC(as.numeric(as.character(times[,3])),width=2,format="d",flag="0")
      
      datetime <- paste(dates[,1],dates[,2],dates[,3],times[,1],times[,2],times[,3],sep="")
    }
    
    #Empty the datetime column in sensor
    sensor[,1] <- NULL
    
    # Add the new dates
    sensor <- cbind(datetime,sensor)
    
    # Remove from the environment variables that will no longer be used.   
    rm(date.time)
    rm(dates)
    rm(times)
    rm(datetime)
    rm(test1)
    
    # Create a file to document all of the input parameters used in this analysis.
    tempfilename <- gsub(".csv",paste("__inputparameters__",analysis.start,".csv",sep=""),filename)
    write.csv(inputparam,tempfilename,row.names=FALSE)
    rm(tempfilename)
    rm(inputparam)
    rm(parameters)
    rm(param_values)
    
    
    # Create a file using the modified data with the new dates.
    
    filename <- gsub(".csv",paste("__formatdate__",analysis.start,".csv",sep=""),filename)
    
    
    write.csv(x=sensor,file=filename,row.names=FALSE)
    
    
    
    # Create an empty matrix
    databin30 <- matrix()
    
    # Create multiple empty lists   
    pvalues <- list()
    periods <- list()
    powers <- list()
    cospvals <- list()
    Mesors <- list()
    cosphis <- list()
    cosphises <- list()
    cosamps <- list()
    cosampses <- list()
    cosprs <- list()
    autoperiodsall <- list()
    autopowersall <- list()
    windowstarts <- list()
    windowends <- list()
    
    # Valcols <- 2
    
    
    
    #seq(from=2,to=ncol(sensor),by=1)
    # TimeCol=
    # It is unclear to me what is  endX and why it is 2.
    endX <- 2
    # p.alpha <- 0.05
    
    # We now begin a giant for loop that will run over every element of what we
    # considered to be the datacolumns or column.
    for(j in Valcols){
      
      
      
      # Import the first row of the csv file we wrote with the sensor data.      
      myHeader <- read.table(filename, header = TRUE, nrows = 1,
                             sep = ",", skip = 0)
      
      # Import the entirety of the csv file we wrote with the sensor data.
      myData1A <- read.table(filename, header = TRUE, sep = ",", 
                             skip = 0, stringsAsFactors = FALSE, comment.char = "", 
                             col.names = names(myHeader))
      
      # Select the Time column and the column we chose as the datacolumn from the sensor data.
      myData1 <- myData1A[, c(TimeCol = TimeCol, valCols = j)]
      
      # Change Datetime column from a char into a Datatime data type.     
      myData1[, TimeCol[1]] <- as.POSIXct(strptime(x = paste(myData1[,TimeCol[1]], myData1[, TimeCol[1]]), format = "%Y%m%d%H%M%S" , tz = "GMT"))      #Changes date format.
      
      
      # Modify the data to be analyzed, check for missing values and inpute where possible; save this output.
      myList <- do.call(Import, list(x = myData1, tsCol = 1, valCols = j, sumCols = 1,
                                     rowDurationSeconds = sampling * 60, maxGap = nrow(myData1), 
                                     verbose = 0, tz = "GMT"))
      
      # Check if myList$df[,1] == myData1[,1], it should be the same.
      test <- myList$df[,1] %in% myData1[,1]
      
      # dave the sensor data in another variable called rawdata.
      rawdata <- myList$df
      
      # Go through every element in test and if that element = FALSE, change it for an NA.
      
      for(i in 1:length(test)){
        if(test[i]==FALSE){
          rawdata[i,2] = NA
        }
      }
      # endtrial <- nrow(data1min)-30
      
      # rm(test)
      # 
      # bin30 <- data.frame()
      # for(i in seq(from=1,to=endtrial, by=30)){
      #   
      #   bin30 <- rbind(bin30,data1min[i,])
      # }
      # rm(endtrial)
      # 
      # databin30 <<- cbind(databin30,bin30)
      
      
      # Take the first row of the sensor data.
      bin30 <- rawdata[1,]
      
      # define bin movement as the ratio of the newsampling/sampling. (I don't understand this well)
      
      binmov <- newsampling/sampling
      
      # Create a sequence of number that goes from 1 + binmov to the end of however many rows
      # there are in the sensor data by making steps of numbers equal to binmov.
      bins30min <- seq(from = 1+binmov, to = nrow(myList$df), by = binmov)
      
      # For every number in bins30min, take that row in the sensor data and bind it to the first row
      # of the sensor data. After this loop you end up with an exact copy of the rawdata unless you change
      # the sampling rate.
      for(w in bins30min){
        bin30 <- rbind(bin30,rawdata[w,])
      }
      
      # Remove missing values from the data.
      bin30 <- remove.na(bin30)
      
      # Only keep the data with the dropped NA's
      bin30 <- bin30$x
      
      # Create an empty list called CatBins. At every iteration, this variable will be
      # Cleared out of all values.
      CatBins <- list()
      # Create a level called binNames with the names of all the columns in bin30 (the data)
      CatBins$binNames <- names(bin30)
      # There are 1440 minutes in a day, by dividing it by the newsampling, we can get
      # the total number of samples we get after the resampling.
      CatBins$binsPerDay <- 1440/newsampling
      # The amount of observations/datapoints in bin30
      CatBins$dataPts <- nrow(bin30)
      # This will be exactly the same as binsPerDay
      CatBins$dayPts <- 1440/newsampling
      # The value of newsampling
      CatBins$newMinPerBin <- newsampling
      # The quatity of columns in the data minus 1.
      CatBins$plotCnt <- ncol(bin30)-1
      
      
      # I still don't know why this works or how it will be used.
      movingwindow <- days.movingwindow*(((1440)*slide.movingwindow)/CatBins$newMinPerBin)
      
      #   
      cntwindows <- floor((CatBins$dataPts)/(1440/CatBins$newMinPerBin))*(1440/CatBins$newMinPerBin)-movingwindow
      startwindows <- seq(from = 0, to = cntwindows, by = movingwindow/days.movingwindow)
      
      auto <- list()
      
      period <- c()
      power <- c()
      p.value <- c()
      lombout <- list()
      lomboutdata <- list()
      cospval <- c()
      cosphi <- c()
      cosphise <- c()
      cosamp <- c()
      cosampse <- c()
      cospr <- c()
      autopowers <- c()
      autoperiods <- c()
      Mesor <- c()
      times <- seq(from=newsampling/60,to=days.movingwindow*24,by=newsampling/60)
      
      
      cnter <- 0
      
      pdf(paste("autocorrelationgraphs__",analysis.start,".pdf",sep=""))
      
      for(i in startwindows){
        
        cnter <- cnter+1
        
        lay <- layout(rbind(c(1),c(2),c(3),c(4),c(5)))
        par(mar=c(2,4,1.75,2))
        
        
        datacos <- bin30[1:movingwindow+i,]
        
        windowstarts[[length(windowstarts)+1]] <- as.character(datacos[1,1])
        windowends[[length(windowends)+1]] <- as.character(datacos[nrow(datacos),1])
        
        
        datacosraw <- datacos
        
        
        
        zerotest <- rle(datacos[,2])
        zerotest2 <- cbind(zerotest$values,zerotest$lengths)
        zerotest <- subset(zerotest2,zerotest2[,1]==0)
        if(any(zerotest[,2] >= zerobins)){
          days <- ((movingwindow)/(1440/CatBins$newMinPerBin))/(days.movingwindow)
          periodNA <- rep(0,days)
          period <- append(period,periodNA,after=length(period))
          powerNA <- rep(0,days)
          power <- append(power,powerNA,after=length(power))
          p.valueNA <- rep(0,days)
          p.value <- append(p.value,p.valueNA,after=length(p.value))
          cospvalNA <- rep(0,days)
          cospval <- append(cospval,cospvalNA,after=length(cospval))
          MesorNA <- rep(0,days)
          Mesor <- append(Mesor,MesorNA,after=length(Mesor))
          cosphiNA <- rep(0,days)
          cosphi <- append(cosphi,cosphiNA,after=length(cosphi))
          cosphiseNA <- rep(0,days)
          cosphise <- append(cosphise,cosphiseNA,after=length(cosphise))
          cosampNA <- rep(0,days)
          cosamp <- append(cosamp,cosampNA,after=length(cosamp))
          cosampseNA <- rep(0,days)
          cosampse <- append(cosampse,cosampseNA,after=length(cosampse))
          cosprNA <- rep(0,days)
          cospr <- append(cospr,cosprNA,after=length(cospr))
          autopowersNA <- rep(0,days)
          autopowers <- append(autopowers,autopowersNA,after=length(autopowers))
          autoperiodsNA <- rep(0,days)
          autoperiods <- append(autoperiods,autoperiodsNA,after=length(autoperiods))
          
          
          next
        }
        
        require(pracma)
        datacos[,2] <- movavg(x=datacos[,2],n=n,type="s")
        detach("package:pracma",unload = TRUE)
        
        plot(datacosraw,type="l",xlab="")
        mtext(text=paste("Window number ",cnter,sep = ""),side=3, adj = 1,line = .5,cex = .5)
        
        plot(datacos,type="l",xlab="")
        
        detrend_data <- as.data.frame(datacos[,1])
        detrend_data$detrend <- pracma::detrend(datacos[,2])
        
        plot(detrend_data, type= "l", xlab="" )
        
        datacos2 <- datacos
        
        
        
        datacos[,1] <- gsub("[[:punct:]]","",datacos[,1])
        datacos[,1] <- gsub(" ","",datacos[,1])
        datacos[,1] = substr(datacos[,1],1,nchar(datacos[,1])-2)
        
        write.csv(datacos,"datacos.csv",row.names=FALSE)
        
        filecos <- file.path(filePath,"datacos.csv")
        
        require(pracma)
        smootheddata <- movavg(x=bin30[1:movingwindow+i,2],n=n,type="s")
        detrend_data <- detrend(smootheddata)
        detach("package:pracma",unload=TRUE)
        
        if (detrend) {
          CatBins$DataList <- matrix(detrend_data,nrow = 1)
          CatBins$smootheddata <- matrix(smootheddata,nrow=1)
        } else {CatBins$DataList <-  matrix(smootheddata,nrow=1)}
        #CatBins$DataList <- matrix(bin30[1:movingwindow+i,2],nrow=1)
        
        
        
        CatBins$TimeBins <- bin30[1:movingwindow+i,1]
        DataList <- CatBins$DataList
        
        
        
        LagPcnt <- 1
        Avg <- FALSE
        modulo <- 1440
        fileNum <- 1
        
        startData <-1
        TimeBins<-CatBins$TimeBins
        startX<-2    #  time is in column 1
        startDark <-as.character(format(as.POSIXct(TimeBins[startData], origin="1970-01-01",tz="GMT"),"%H:%M:%S"))    #  get index of first darkness value
        thisTime <- format(Sys.time(), "%b %d, %Y - %H:%M:%S");
        nameVector = CatBins$binNames[-1]     # exclude the DateTime header name
        endXrel<-endX-startX+1
        for (cnt in 1:CatBins$plotCnt) {      #  1 plot per animal, for all animals 
          curX<-cnt+startX-1
          MaxLag <- round((CatBins$dataPts/CatBins$dayPts)*LagPcnt)*CatBins$dayPts        #dayPts is really pts in a modulus (chosen period)
          if (MaxLag<CatBins$dayPts){     #  cannot be less than one period
            MaxLag<-CatBins$dayPts
          }
          
          #The function acf computes (and by default plots) estimates of the autocovariance or autocorrelation function. 
          getACF <<- acf(DataList[cnt,],main="",type="correlation",ylab="Correlation coefficient (r)",plot = FALSE, lag.max = MaxLag,col = "white")
          yACF <- as.numeric(getACF$acf)
          
          for(q in seq(from=1, to=length(yACF))){
            nantest <- is.nan(yACF[q])
            if(nantest==TRUE){
              yACF[q]=0
            }
          }
          
          auto[[length(auto)+1]] <- yACF
          
          #      #lowend <- ((1440/newsampling)*2)-((60/newsampling)*6)
          #      #topend <- ((1440/newsampling)*2)+((60/newsampling)*6)
          
          peaks <- findallpeaks(yACF)
          
          lags <- c()
          
          for(i in peaks){
            lagpoint <- which(yACF==i)
            lags <- append(lags,lagpoint,after=length(lags))
          }
          
          diffs <- diff(lags)
          
          autopower <- mean(peaks)
          #      #autopower <- findpeaks(yACF[lowend:topend])
          
          
          autopowers <- append(autopowers,autopower,after=length(autopowers))
          
          
          
          
          
          
          
          
          #Plotting Autocorrelations    
          if(identical(autopower,-Inf) | length(peaks) == 0){
            xACF <- as.numeric(seq(from = 0, to = length(yACF)-1, by = 1))
            plot(xACF,yACF,type="l",col="blue",ylim=c(-1,1),ylab="Correlation Coefficient (r)",xlab="Lag")
            
            abline(v=seq(from=0, to=MaxLag, by=(CatBins$binsPerDay)),xaxt="n",col="darkslategray",lty="dotted")
            abline(h=0,col="black")
            
            #points(x=autoperiodlag,y=autoperiod,pch=8,cex=3)
            legend("bottomright",legend = paste("No period or peak identified"),bty = "n",cex = 1)
            
            #mtext(text=paste(CatBins$binNames[2]),side=3,outer=TRUE,padj=1.5)
            mtext(text=paste(CatBins$TimeBins[1],CatBins$TimeBins[length(CatBins$TimeBins)],sep=" - "),side=3,outer=TRUE,padj=1.5)
            title(sub=paste(cnter),adj=1,line=4,cex.sub=.75)
            
            autoperiod <- NA
            autoperiods <- append(autoperiods,autoperiod,after=length(autoperiods))
            
            
          }else if (multipeak_period == T) {
            
            #This is one of the values that is used for the cosine fitting. We can adjust it to use all peaks or the second peak, when possible.
            autoperiod <- mean(diffs)
            
            #        #autoperiod <- which(yACF == autopower)
            
            xACF <- as.numeric(seq(from = 0, to = length(yACF)-1, by = 1))
            plot(xACF,yACF,type="l",col="blue",ylim=c(-1,1),ylab="Correlation Coefficient (r)",xlab="Lag")
            # 
            abline(v=seq(from=0, to=MaxLag, by=(CatBins$binsPerDay)),xaxt="n",col="darkslategray",lty="dotted")
            abline(h=0,col="black")
            
            
            points(x=lags,y=peaks,pch=8,cex=3)
            #        #points(x=autoperiod,y=autopower,pch=8,cex=3)
            #        #legend("bottomright",legend = c(paste("Period = ",(autoperiod/(60/newsampling))/2,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
            legend("bottomright",legend = c(paste("Period = ",autoperiod,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
            
            
            # mtext(text=paste(CatBins$binNames[2]),side=3,outer=TRUE,padj=1.5)
            mtext(text=paste(CatBins$TimeBins[1],CatBins$TimeBins[length(CatBins$TimeBins)],sep=" --- "),side=3,outer=TRUE,padj=1.5)
            
            
            #        #autoperiods <- append(autoperiods,(autoperiod/(60/newsampling))/2,after=length(autoperiods))
            autoperiods <- append(autoperiods,autoperiod,after=length(autoperiods))
          } else if (multipeak_period == F) {
            #This is one of the values that is used for the cosine fitting. We can adjust it to use all peaks or the second peak, when possible.
            if (length(lags) == 1) {
              autoperiod <- lags
              
              #Creating plot
              xACF <- as.numeric(seq(from = 0, to = length(yACF)-1, by = 1))
              plot(xACF,yACF,type="l",col="blue",ylim=c(-1,1),ylab="Correlation Coefficient (r)",xlab="Lag")
              # 
              abline(v=seq(from=0, to=MaxLag, by=(CatBins$binsPerDay)),xaxt="n",col="darkslategray",lty="dotted")
              abline(h=0,col="black")
              
              #Stars for the peaks
              points(x=lags,y=peaks,pch=8,cex=3)
              #        #points(x=autoperiod,y=autopower,pch=8,cex=3)
              #        #legend("bottomright",legend = c(paste("Period = ",(autoperiod/(60/newsampling))/2,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
              legend("bottomright",legend = c(paste("Period = ",autoperiod,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
              
              
              # mtext(text=paste(CatBins$binNames[2]),side=3,outer=TRUE,padj=1.5)
              mtext(text=paste(CatBins$TimeBins[1],CatBins$TimeBins[length(CatBins$TimeBins)],sep=" --- "),side=3,outer=TRUE,padj=1.5)
              
              
              #        #autoperiods <- append(autoperiods,(autoperiod/(60/newsampling))/2,after=length(autoperiods))
              autoperiods <- append(autoperiods,autoperiod,after=length(autoperiods)) 
              
              
              
            } else if (length(diffs) >= peak_of_interest-1) {
              autoperiod <- diffs[peak_of_interest-1]
              
              xACF <- as.numeric(seq(from = 0, to = length(yACF)-1, by = 1))
              plot(xACF,yACF,type="l",col="blue",ylim=c(-1,1),ylab="Correlation Coefficient (r)",xlab="Lag")
              # 
              abline(v=seq(from=0, to=MaxLag, by=(CatBins$binsPerDay)),xaxt="n",col="darkslategray",lty="dotted")
              abline(h=0,col="black")
              
              
              points(x=lags[-peak_of_interest],y=peaks[-peak_of_interest],pch=8,cex=3)
              points(x=lags[peak_of_interest],y=peaks[peak_of_interest],pch=8,cex=3, col = 'red')
              #        #points(x=autoperiod,y=autopower,pch=8,cex=3)
              #        #legend("bottomright",legend = c(paste("Period = ",(autoperiod/(60/newsampling))/2,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
              legend("bottomright",legend = c(paste("Period = ",autoperiod,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
              
              
              # mtext(text=paste(CatBins$binNames[2]),side=3,outer=TRUE,padj=1.5)
              mtext(text=paste(CatBins$TimeBins[1],CatBins$TimeBins[length(CatBins$TimeBins)],sep=" --- "),side=3,outer=TRUE,padj=1.5)
              
              
              #        #autoperiods <- append(autoperiods,(autoperiod/(60/newsampling))/2,after=length(autoperiods))
              autoperiods <- append(autoperiods,autoperiod,after=length(autoperiods))
            } else {
              autoperiod <- diffs[length(diffs)]
              
              xACF <- as.numeric(seq(from = 0, to = length(yACF)-1, by = 1))
              plot(xACF,yACF,type="l",col="blue",ylim=c(-1,1),ylab="Correlation Coefficient (r)",xlab="Lag")
              # 
              abline(v=seq(from=0, to=MaxLag, by=(CatBins$binsPerDay)),xaxt="n",col="darkslategray",lty="dotted")
              abline(h=0,col="black")
              
              
              points(x=lags,y=peaks,pch=8,cex=3)
              points(x=lags[length(lags)],y=peaks[length(peaks)],pch=8,cex=3, col = 'red')
              #        #points(x=autoperiod,y=autopower,pch=8,cex=3)
              #        #legend("bottomright",legend = c(paste("Period = ",(autoperiod/(60/newsampling))/2,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
              legend("bottomright",legend = c(paste("Period = ",autoperiod,"hours"),paste("Correlation Coefficient =",autopower)),bty = "n",cex = 1)
              
              
              # mtext(text=paste(CatBins$binNames[2]),side=3,outer=TRUE,padj=1.5)
              mtext(text=paste(CatBins$TimeBins[1],CatBins$TimeBins[length(CatBins$TimeBins)],sep=" --- "),side=3,outer=TRUE,padj=1.5)
              
              
              #        #autoperiods <- append(autoperiods,(autoperiod/(60/newsampling))/2,after=length(autoperiods))
              autoperiods <- append(autoperiods,autoperiod,after=length(autoperiods))
              
              
              
              
              
            } }
          
          
          
          
          
          
          
          
          
          
          
          
          if (cnt==1){
            fileMatrix<-getACF$lag
          }
        }
        
        
        
        
        #########
        #########lomb-Scargle of data
        dataraw <- as.vector(DataList)
        lomboutraw <- lombscargle_mod(dataraw,times=times,from=lomb.period[1],to=lomb.period[2],ofac=oversampling_factor,alpha=p.alpha,main="",type="period",plot=TRUE)
        points(x=lomboutraw$peak.at[1],y=lomboutraw$peak,pch=8,cex=3)
        legend("bottomright",legend = c(paste("Period = ",lomboutraw$peak.at[1],"hours"),paste("Power =",lomboutraw$peak)),bty = "n",cex = 1)
        
        period <- append(period,lomboutraw$peak.at[1],after=length(period))
        power <- append(power,lomboutraw$peak,after=length(power))
        p.value <- append(p.value,lomboutraw$p.value,after=length(p.value))
        #,from=18,to=30
        #assign(x=paste("lomboutdata",i,sep=""),value=lomboutraw)
        #########lomb-Scargle of data
        
        #Here we tell the function which period value to use, the lombout or the autocorrelation
        if (correlation_method == "autocorrelation"){
          per <-   autoperiods[length(autoperiods)]
        } else if (correlation_method == "lombout") {
          per <- lomboutraw$peak.at[1]
        } else {
          stop("correlation_method is not spelled correctly: must be either autocorrelation or lombout")
        }
        
        #
        per <- ifelse(is.na(per), 24, per)
        if(is.na(per)==TRUE){
          Mesor <- append(Mesor,NA,after=length(Mesor))
          cospval <- append(cospval,NA,after=length(cospval))
          cosphi <- append(cosphi,NA,after=length(cosphi))
          cosphise <- append(cosphise,NA,after=length(cosphise))
          cosamp <- append(cosamp,NA,after=length(cosamp))
          cosampse <- append(cosampse,NA,after=length(cosampse))
          cospr <- append(cospr,NA,after=length(cospr))
        }else{
          # pdf("test.pdf")
          pertest <- append(pertest, per, after = length(pertest))
          Cosinor_2(TimeCol=1,Y=2, Components=1, window="noTaper", RefDateTime=phase_ref,
                    timeFormat="%Y%m%d%H%M", RangeDateTime=list(Start=NA,End=NA),
                    Units="Hours", Progressive=list(Interval=0, Increment=0),
                    Period=list(Set=per,Start=0,Increment=1,End=0),header=TRUE,
                    Skip=0, Colors="BW",Graphics="pdf",Output=list(Txt=FALSE,Dat=FALSE,Doc=FALSE,Graphs=FALSE),
                    yLabel="",Console=TRUE,Debug=FALSE,IDcol="filename",fileName=filecos,functionName="")
          file.remove(filecos)
          Mesor <- append(Mesor,mesor,after=length(Mesor))
          cospval <- append(cospval,PVAL,after=length(cospval))
          cosphi <- append(cosphi,PHI,after=length(cosphi))
          cosphise <- append(cosphise,PHI.SE,after=length(cosphise))
          cosamp <- append(cosamp,AMP,after=length(cosamp))
          cosampse <- append(cosampse,AMP.SE,after=length(cosampse))
          cospr <- append(cospr,PR,after=length(cospr))
          
          plot(datacos2,type="l",xlab="",ylim=y_range)
          legend("bottomright",legend = c(paste("Amplitude = ",AMP),paste("Phase =",PHI),paste("PR =",PR)),bty = "n",cex = .75)
          
          par(new=TRUE)
          plot(x=c(1:waves),y=rep(tempModel,length.out=waves),ylab="",xlab="",axes=FALSE,col="blue",ylim=y_range,type="l")
        }
        
        
      }
      
      
      Mesors[[length(Mesors)+1]] <- Mesor
      pvalues[[length(pvalues)+1]] <- p.value
      powers[[length(powers)+1]] <- power
      periods[[length(periods)+1]] <- period
      cospvals[[length(cospvals)+1]] <- cospval
      cosphis[[length(cosphis)+1]] <- cosphi
      cosphises[[length(cosphises)+1]] <- cosphise
      cosamps[[length(cosamps)+1]] <- cosamp
      cosampses[[length(cosampses)+1]] <- cosampse
      cosprs[[length(cosprs)+1]] <- cospr
      autoperiodsall[[length(autoperiodsall)+1]] <- autoperiods
      autopowersall[[length(autopowersall)+1]] <- autopowers
    }
    
    
    dev.off()
    
    pdf(paste("graphsparam__",analysis.start,".pdf",sep=""))
    
    columns <- names(sensor[,2:length(sensor)])
    
    windowstart <- unlist(windowstarts)
    windowend <- unlist(windowends)
    
    #Here we are making a data frame with the results of the cosine function fitting on specific time windows.
    #We are doing this for each IND which should be 1
    # You will need dplyr and tidyr to run this portion of the script.
    cosine_results <- dplyr::tibble(Mesors, pvalues, powers, periods, cospvals, cosphis, cosphises, cosamps,
                                    cosampses, cosprs, autoperiodsall, autopowersall)
    # The way we made the data frame, results in each column being a list. Here we unnest which is like 
    # unlist but for data frame columns.
    cosine_results<- tidyr::unnest(cosine_results, cols = c(Mesors, pvalues, powers, periods, cospvals, cosphis, cosphises, 
                                                            cosamps, cosampses, cosprs, autoperiodsall, autopowersall))
    # Add the start of each window
    cosine_results$windowstart <- windowstart
    # Add the end of each window
    cosine_results$windowend <- windowend
    # Renaming so it makes sense
    names(cosine_results) <- c("mesor", "pval_lombout", "pwr_lombout", "period_lombout", "pval_cos",
                               "phi_cos", "phi_SE_cos", "amp_cos", "amp_SE_cos", "PR_cos","period_auto", "pwr_auto",
                               "window_start", "window_end")
    # Save the output into a CSV
    write.csv(cosine_results, 'cosine_results.csv', row.names = FALSE)
    
    
    #handling cases where some variables are NA:
    autoperiodsalls <- ifelse(is.na(autoperiodsall), 0, autoperiodsall)
    
    
    pvaluesall <- data.frame(windowstart,windowend,matrix(unlist(pvalues),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE)) 
    #colnames(pvaluesall) <- columns
    write.csv(pvaluesall,paste(filename,"__pvalues__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    powersall <- data.frame(windowstart,windowend,matrix(unlist(powers),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(powersall) <- columns
    write.csv(powersall,paste(filename,"__powers__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    periodsall <- data.frame(windowstart,windowend,matrix(unlist(periods),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(periodsall) <- columns
    write.csv(periodsall,paste(filename,"__periods__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    cospvalsall <- data.frame(windowstart,windowend,matrix(unlist(cospvals),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(cospvalsall) <- columns
    write.csv(cospvalsall,paste(filename,"__cospvals__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    cosphisall <- data.frame(windowstart,windowend,matrix(unlist(cosphis),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    cosphisall$phasehrs <- (12*cosphisall[,3])/(180)
    #colnames(cosphisall) <- columns
    write.csv(cosphisall,paste(filename,"__cosphis__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    cosphisesall <- data.frame(windowstart,windowend,matrix(unlist(cosphises),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(cosphisesall) <- columns
    write.csv(cosphisesall,paste(filename,"__cosphises__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    cosampsall <- data.frame(windowstart,windowend,matrix(unlist(cosamps),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(cosampsall) <- columns
    write.csv(cosampsall,paste(filename,"__cosamps__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    cosampsesall <- data.frame(windowstart,windowend,matrix(unlist(cosampses),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(cosampsesall) <- columns
    write.csv(cosampsesall,paste(filename,"__cosampses__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    cosprsall <- data.frame(windowstart,windowend,matrix(unlist(cosprs),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(cosprsall) <- columns
    write.csv(cosprsall,paste(filename,"__cosprs__",analysis.start,".csv",sep=""),row.names=TRUE)
    
    autopowersalls <- data.frame(windowstart,windowend,matrix(unlist(autopowersall),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(autopowersalls) <- columns
    write.csv(autopowersalls,paste(filename,"__autocorrelationcoeff__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    autoperiodsalls <- data.frame(windowstart,windowend,matrix(unlist(autoperiodsall),ncol=length(names(sensor))-1,nrow=length(startwindows),byrow=FALSE))
    #colnames(autoperiodsalls) <- columns
    write.csv(autoperiodsalls,paste(filename,"__autoperiods__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    
    
    lay <- layout(rbind(c(1,1,1,1),
                        c(2,3,4,5),
                        c(6,7,8,9)))
    
    timesgraphs <- seq(from=1, to=cnter,by=1)
    
    
    plot(rawdata,type="l")
    plot(x=timesgraphs,y=autoperiodsalls[1:nrow(autoperiodsalls),3],type="l",ylab="Periods Autocorrelation")
    plot(x=timesgraphs,y=cospvalsall[1:nrow(cospvalsall),3],type="l",ylab="Cosinor P-values")
    plot(x=timesgraphs,y=cosampsall[1:nrow(cosampsall),3],type="l",ylab="Cosinor Amplitudes")
    plot(x=timesgraphs,y=cosphisall[1:nrow(cosphisall),3],type="l",ylab="Cosinor Phases")
    plot(x=timesgraphs,y=autopowersalls[1:nrow(autopowersalls),3],type="l",ylab="Autocorrelation Coefficients")
    plot(x=timesgraphs,y=periodsall[1:nrow(periodsall),3],type="l",ylab="Lomb-Scargle Periods")
    plot(x=timesgraphs,y=powersall[1:nrow(powersall),3],type="l",ylab="Lomb-Scargle Powers")
    plot(x=timesgraphs,y=pvaluesall[1:nrow(pvaluesall),3],type="l",ylab="Lomb-Scargle P-values")
    
    
    dev.off()
    
    date.timeraw <- str_split_fixed(rawdata[,1]," ",n=2)
    date.timeraw[,1] <- gsub("-","/",date.timeraw[,1])
    outputraw <- cbind(date.timeraw,rawdata[,2])
    colnames(outputraw) <- c("Date","Time","Values")
    
    write.csv(outputraw,paste("output_rawdata__",analysis.start,".csv",sep=""),row.names = TRUE)
    
    
    
    
    
  }
  
}

