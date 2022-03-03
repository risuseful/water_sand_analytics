# wgr_v1.R
# Akmal, 16JAN2019

# remove data
rm(list = ls())

# setting seed
set.seed(123)

# call library
library(parallel)
library(doParallel)
library(randomForest)
library(stats)
library(RODBC)
library(stringi)
library(stringr)
library(data.table)
library(rlang)
library(GA)


# record time lapse
start.time <- Sys.time()


# specify delSet (max_wgr = min_wgr + delSet)
delSet <- 0 # if delSet=0, max_wgr is set to maxVal_wgr
maxVal_wgr <- 350

# ------------- set working directory and build all functions -------------------------
setwd("C:/R Studio") 

# debug
# setwd("U:/Integrated Field Development Planning/2018-2019 Data Mining Project/R scripts/Water/wgr/")

# assigning directory for saving images
saveDir <- c("****") 

# debug
# saveDir <- c("U:/Integrated Field Development Planning/2018-2019 Data Mining Project/R scripts/Water/wgr/")

# build all functions
source('wgr_func.R')

# -------------------------------------------------------------


# ------------------- grab data from server ------------------------

# set up connection via ODBC to the SQL server
dbconnection <- odbcDriverConnect("Driver={****};Server=****; Database=****;Uid=****; Pwd=****;")

# extract user's input
# rangeTab <- sqlQuery(dbconnection,paste("select top 1 * from Range where HasResult = 0")) -- *RE-ACTIVATE AFTER Debug*

# debug
# rangeTab <- sqlQuery(dbconnection,paste("
# 
#                                         select Range.ID, Range.StartDate, Range.EndDate, Range.D1, Range.D2,Range.Hist, Range.Stat 
#                                         from 
#                                         range 
#                                         inner join range_single 
#                                         on range.id = range_single.ID
#                               
#                                         
#                                         
#                                         
#                                         
#                                         "))


rangeTab <- sqlQuery(dbconnection,paste("spRange"))


# exit program if rangeTab$HasResult=1, since results are already available
#if (rDat$HasResult>0)
if (nrow(rangeTab)==0)
{

  # close the localhost-SQLserver connection
  odbcClose(dbconnection )

} else {
  
  rDat <- rangeTab[1,] # take the first row

  # grab start and end time
  tInit <- rDat$StartDate
  tEnd <- rDat$EndDate
  
  # debug
  # tInit <- "2018-01-01"
  # tEnd <- "2018-12-31"

  
  # grab input data
  # d <- grabDat(tInit, tEnd) -- grabDat() is obsolete
  d <- grabDatNew(tInit, tEnd)
  
  # close the localhost-SQLserver connection
  odbcClose(dbconnection )

  # ------------------------------------------------------------------
  
  
  
  
  # --------- preprocessing --------------
  
  # resample: 1st hour in a 24 hours period
  m <- d
  # m <- m[ str_sub(stri_sub(m$"TimeStamp", 12),1,2) %in% c("01"), ]
  
  
  # create "mat" matrix, i.e. without TimeStamp
  mat <- m
  m <- NULL
  td <- as.Date(substr(mat$TimeStamp, start=1, stop=10))
  mat[c("TimeStamp")] <- list(NULL) # remove "TimeStamp" column
  
  # remove columns with NAs
  mat <- mat[, colSums(is.na(mat)) == 0]
  
  # remove columns that have mostly zero values
  # mat <- mat[, colMeans(mat)>1]
  # note: deactivated since you want 128 values for the WGR for each well
  
  # --------------------------------------------
  
  
  
  
  
  # --------- determining total water from CKX separator: watTotal ------------------
  
  # specify date when measurement of total water becomes available at CKX
  tCKX <- as.Date(c("2017-07-01"))
  
  # re-format tInit
  # tStart <- as.Date(rDat$StartDate)
  
  # initiate watTotal
  ntd <- nrow(as.matrix(td))
  watTotal <- matrix(0,ntd,1)
  
  
  # compute watTotal
  # for (i in (1:ntd))
  # {
  #   if (td[i] < tCKX)
  #   {
  #     
  #     watTotal[i] <- mat$TotalWater[i]
  #     
  #   } else {
  #     
  #     # getting input vectors from CKX reading
  #     Bc <- mat$"FICA-2006"[i] # CKX PW Pump reading
  #     Op <- mat$"FICA-2009-MV"[i] # PW Pump recycle opening (in percentage)
  #     P1 <- mat$"PI-2026"[i] # inlet pressure
  #     P2 <- mat$"PICA-2010"[i] # outlet pressure, i.e. separator pressure
  #     
  #     # compute water rate at the outlet
  #     Bd <- Bc*(24/0.159) # note: 24 and 0.159 are conversion factors
  #     
  #     # compute recycled water rate (recycling to the CKX separator)
  #     Cv <- ifelse( Op==0, 0, 0.2607*exp(0.0391*Op) )
  #     DP <- 100*(P1-P2) # compute pressure difference
  #     DP <- ifelse( DP>=0, DP, 0 )
  #     Qr <- 0.0865*Cv*sqrt(DP)
  #     
  #     # compute total water
  #     watTotal[i] <- Bd - (24/0.159)*Qr
  #     
  #     # make adjustments on negative values
  #     if (watTotal[i] < 0)
  #     {
  #       watTotal[i] <- 0
  #     }
  #     
  #   }
  # }
  
  
  # compute watTotal
  for (i in (1:ntd))
  {
      watTotal[i] <- mat$TotalWater[i]
  }
  
  
  # remove unnecessary columns
  mat <- as.data.frame(mat) # convert to data frame, just to be save
  to.remove <- c("FICA-2006", "FICA-2009-MV", "PI-2026", "PICA-2010", "TotalWater", "SalesGas")
  mat <- mat[, -which(names(mat) %in% to.remove)]
  
  # attach total water vector to matrix "newmat"
  mat <<- cbind(mat, watTotal) # "<<-" makes this matrix a Global object
  
  nw <<- ncol(mat) - 1 # total number of wells
  
  
  # --------------------------------------------------------------------
  
  
  
  
  
  # --------------- specify range of values for wgr -------------------
  
  # set up connection via ODBC to the SQL server
  dbconnection <- odbcDriverConnect("Driver={****};Server=****; Database=****;Uid=****; Pwd=****;")
  
  wgrRange <- sqlQuery(dbconnection,paste("select Well, WGR_min, WGR_Max from PRISM_WGR")) # grab min, max values from database
  
  #min_wgr <- 1 # minimum WGR per well
  # max_wgr <- 300 # maximum WGR per well
  max_wgr <- c(rep(0,nw)) # create zero matrix of maximum values; to be filled up
  min_wgr <- c(rep(0,nw)) # create zero matrix of minimum values; to be filled up
  
  # close the localhost-SQLserver connection
  odbcClose(dbconnection )
  
  # debug
  ns <- 0
  
  # assign min_wgr vector
  for ( i in 1:(ncol(mat)-1) )
  {
    
    # name of well from "mat"
    wnam <- colnames(mat)[i]
    wnam <- str_replace_all(string=wnam, pattern=".Gas", repl="")
    
    
    for (j in 1:nrow(wgrRange))
    {
      # if well name is matching, copy and paste minimum wgr value
      if (wnam == wgrRange$Well[j])
      {
        
        max_wgr[i] <- wgrRange[j,3] # assign maximum wgr value for that well
        min_wgr[i] <- wgrRange[j,2] # assign minimum wgr value for that well
        
        # debug
        ns <- ns + 1
        
        # quality check; ensure max>=min
        if (max_wgr[i] <= min_wgr[i])
        {
            max_wgr[i] <- min_wgr[i] + delSet
        }
        
      }
    }
    
    # override max_wgr if delSet=0
    if (delSet == 0)
    {
      max_wgr[i] <- maxVal_wgr
    }
    

    
  }
  
  # debug
  ns
  min_wgr
  
  # ----------------------------------------------------
  
  
  
  
  # ------------ refine min_wgr and max_wgr ---------------
  # note: force WGR to 0 if the well is mostly shut throughout time
  for (i in 1:nw)
  {
    if (colMeans(mat)[i]<1)
    {
      min_wgr[i] <- 0.0
      max_wgr[i] <- 0.0001
    }
  }
  # ------------------------------------------------------
  
  
  
  # ------------------- solve WGR using optim ----------------------
  

  
  # GA <- ga(type = "real-valued", fitness =  function(x) fitGA(x), 
  #          lower = c(rep(min_wgr,nw)), upper = c(rep(max_wgr,nw)), 
  #          maxiter=10000, monitor=TRUE, parallel=TRUE, popSize=128)
  
  
  # use optim() ? -> adjust fitGA's line that says "res = -res" to "res = res"
  # optimRes <- optim(min_wgr,fitOp, method="L-BFGS-B", lower = min_wgr, upper = max_wgr)
  #optimRes <- optim(c(rep(100,nw)),fitOp, method="L-BFGS-B", lower = c(rep(1,nw)), upper = c(rep(300,nw)))
  
  #gaRes <- ga(type = "real-valued", fitness=fitGA, lower = min_wgr, upper = max_wgr, popSize = 200, maxiter = 80000, parallel=TRUE)
  # gaRes <- de( fitness=fitGA, lower = min_wgr, upper = max_wgr, maxiter = 80000)
  gaRes <- gaisl(type = "real-valued", 
                 fitness=fitGA, 
                 lower = min_wgr, 
                 upper = max_wgr, 
                 popSize = 300, 
                 maxiter = 100, 
                 parallel=TRUE, 
                 numIslands=10,
                 optim=TRUE,
                 optimArgs = list(method="L-BFGS-B", poptim=0.05, pressel=0.5))
  
  # GA report
  # sol <- slot(GA,"solution")
  # erra <- slot(GA,"fitnessValue")
  
  # optim report
  # sol <- optimRes$par -- Activate
  # erra <- optimRes$value -- Activate
  sol <- slot(gaRes,"solution")[1,]
  erra <- slot(gaRes,"fitnessValue")
  # -----------------------------------------------------------------------------
  
  
  
  
  
  # report end time
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time, units=c("secs"))
  
  
  
  
  
  # -------------- results reporting ---------------
  
  G <- as.matrix(mat[,1:nw])
  w <- as.matrix(sol)
  b_obs <- mat[,nw+1]
  b_pred <- G%*%w # use G%*%w when using optim(), use G%*%t(w) when using ga()
 
  fileNam <- c("_wgrCor.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/", repl="/") # remove blank spaces
  predCor <- cor(b_obs,b_pred)
  mainStr <- paste("Correlation = ", round(predCor,2)," | ", "Runtime = ", round(time.taken,0),"seconds")
  
  png(filename=filePath)
  plot(b_obs,b_pred,xlab=c("Observed Water Rate"), ylab=c("Predicted Water Rate"),xlim=c(0,max(mat[,nw+1])), ylim=c(0,max(mat[,nw+1])), main=mainStr, type="p", pch=15)
  # plot(b_obs,b_pred,xlab=c("Observed Water Rate"), ylab=c("Predicted Water Rate"),main=mainStr, type="p", pch=15)
  lines(x=c(0,max(mat[,nw+1])), y=c(0,max(mat[,nw+1])), col="red" )
  dev.off()
  
  fileNam <- c("_totalWat.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  mainStr <- paste("Pred (Line) vs Obs (Dot) | RMSE Error = ", round(-erra,2))
  
  png(filename=filePath)
  plot(b_pred,type='l',col="blue", xlab=c("Days"), ylab=c("Water Rate"), main=mainStr)
  lines(b_obs, type="p", pch=15, col="black", cex=0.6)
  dev.off()
  
  # fileNam <- c("_wgrProf.png") # specify file name
  # fileNam <- paste(rDat$ID,fileNam, sep="")
  # filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  # mainStr <- paste("Water Gas Ratio:",tInit,"to",tEnd)
  # 
  # png(filename=filePath,width=10.25,height=3.25,res=2200,units="in")
  # png(filename=filePath,width=800,height=600,res=1200)
  # barplot(solFinal, las=2, ylab="WGR", main=mainStr)
  # dev.off()
  
  
  
  # ------------------------------------------------
  
  
  # write solution to csv
  solFinal <- as.matrix(t(sol))
  colnames(solFinal) <- colnames(mat)[1:nw]
  write.csv(file="FinalSolution.csv", solFinal)
  
  
  # ----------------- insert WGR solutions to database -------------------

  dbconnection <- odbcDriverConnect("Driver={****};Server=****; Database=****;Uid=****; Pwd=****;")

  # upQ <- sqlQuery(dbconnection, paste("UPDATE WGR SET ID=", rDat$ID, ", Well=", colnames(mat[i]), ", WGR=", round(sol[i],2)))
  #upQ <- sqlQuery(dbconnection, paste("Insert into WGR (ID, Well, WGR) Values (1, 'namew', 30)"))
  #loadQ <- sqlQuery(dbconnection, paste("select * from wGR"))
  
  sID <- rDat$ID
  
  sqldelete <- c("delete WGR where ID =" )
  sqldelete <- paste(sqldelete, sID)
  sqlQuery(dbconnection, sqldelete)
  
  s1 <- c("Insert into WGR (ID, Well, WGR) Values (")
  
  # debug
  # sID <- 92

  for (i in 1:nw)
  {
    swnam <- paste("'",colnames(mat[i]),"'")
    swnam <- str_replace_all(string=swnam, pattern=" ", repl="") # remove blank spaces

    strComp <- paste(s1,sID,",",swnam,",",sol[i],")")

    upQ <- sqlQuery(dbconnection,strComp)
  }

  #debug
  #loadQ <- sqlQuery(dbconnection, paste("select * from WGR"))
  #loadQ


  odbcClose(dbconnection )

  # ----------------------------------------------------------------------

  # debug
  predCor


}
