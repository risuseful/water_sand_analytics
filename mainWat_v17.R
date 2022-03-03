# ***********************************************************
#                     mainWat_v17.R
#                    Akmal, 23JAN2019
# ***********************************************************




# --------------------- prelims ----------------------
rm(list = ls()) # remove data
set.seed(123) # setting seed
start.time <- Sys.time() # record time lapse
# -----------------------------------------------------





# ------------------ call library ---------------------
library(randomForest)
library(stats)
library(RODBC)
library(stringi)
library(stringr)
library(data.table)
library(rlang)
# ------------------------------------------------------






# ------------------ set directories and create functions --------------------
setwd("U:/Integrated Field Development Planning/2018-2019 Data Mining Project/R scripts/Water/attempt2/") # debug
saveDir <- c("U:/Integrated Field Development Planning/2018-2019 Data Mining Project/R scripts/Water/attempt2/") # debug
source('funcWat.R')# create all functions
# ----------------------------------------------------------------------------






# --------------- grab data from server --------------------

# set up connection via ODBC to the SQL server
dbconnection <- odbcDriverConnect("Driver={****};Server=****; Database=****;Uid=****; Pwd=****;")

# extract user's input
# rangeTab <- sqlQuery(dbconnection,paste("select top 1 * from Range where HasResult = 0")) -- Activate

rangeTab <- sqlQuery(dbconnection,paste("select top 1 * from Range")) # debug

# exit program if rangeTab$HasResult=1, since results are already available
if (nrow(rangeTab)==0)
{
  
  # close the localhost-SQLserver connection
  odbcClose(dbconnection )
  
} else {
  
  rDat <- rangeTab[1,] # take the first row
  
  # extract input data based on user's input
  s1 <- c("select * from vMatrix where timestamp between")
  s2 <- c("order by timestamp;")
  tInit <- rDat$StartDate
  tInit_q <- paste("'",tInit,"'") # add single quote
  tInit <- str_replace_all(string=tInit_q, pattern=" ", repl="") # remove blank spaces
  tEnd <- rDat$EndDate
  tEnd_q <- paste("'",tEnd,"'")
  tEnd <- str_replace_all(string=tEnd_q, pattern=" ", repl="") # remove blank spaces
  
  # debug
  tInit <- c("'2019-01-01'")
  tEnd  <- c("'2019-10-01'")
  
  # grab input data
  d <- sqlQuery(dbconnection, paste(s1,tInit,"and",tEnd,s2)) # concatenate strings and perform sql query
  
  # user's flag for expansion of Random Forests' input data
  # Note: to update, use the following:
  flag_D1   <- rDat$D1 # flag value: generate 1st-order derivative of raw matrix
  flag_D2   <- rDat$D2 # flag value: generate 2nd-order derivative of raw matrix
  flag_Hist <- rDat$Hist # flag value: generate historical matrix
  flag_Desc <- rDat$Stat # flag value: generate descriptive stats of historical matrix
  
  # close the localhost-SQLserver connection
  odbcClose(dbconnection )
  
  # --------------------------------------------------------- 
  
  
} 
  
  
  
  
  # --------- preprocessing -------------------
  
  # resample: 1st hour in a 24 hours period
  m <- d # copy
  m <- m[ str_sub(stri_sub(m$"TimeStamp", 12),1,2) %in% c("01"), ]
  
  # create "mat" matrix, i.e. without TimeStamp
  mat <- m
  m <- NULL
  td <- as.Date(substr(mat$TimeStamp, start=1, stop=10))
  mat[c("TimeStamp")] <- list(NULL) # remove "TimeStamp" column
  
  # remove columns with NAs
  mat <- mat[, colSums(is.na(mat)) == 0]
  
  # remove unnecessary columns
  mat <- as.data.frame(mat) # convert to data frame, just to be save
  to.remove <- c("****", "****", "****", "****", "****")
  mat <- mat[, -which(names(mat) %in% to.remove)]
  
  # --------------------------------------------
  
  
  
  
  
  
  
  
  # ------------ split data ------------------
  
  # number of columns in mat
  nc <- ncol(mat)
  
  # gross gas only
  matGas <- cbind(mat[,grep("Gas", colnames(mat))], mat$TotalWater)
  colnames(matGas)[ncol(matGas)] <- c("TotalWater")
  
  # WHP only
  matP <- cbind(mat[,grep("WHP", colnames(mat))], mat$TotalWater)
  colnames(matP)[ncol(matP)] <- c("TotalWater")
  
  # WHT only
  matT <- cbind(mat[,grep("WHT", colnames(mat))], mat$TotalWater)
  colnames(matT)[ncol(matT)] <- c("TotalWater")
  
  # SandRate only
  matS <- cbind(mat[,grep("SandRate", colnames(mat))], mat$TotalWater)
  colnames(matS)[ncol(matS)] <- c("TotalWater")
  
  # ------------------------------------------
  
  
  
  
  
  # ---------------- RF ranking: mat ------------------
  
  # set seed
  set.seed(123)
  
  # matRF
  matRF <- mat
  
  # split training and test sets
  names(matRF) <- make.names(names(matRF))
  fracTr <- 0.66
  trSamp <- floor(0.66*nrow(matRF))
  tr_idx <- sample(1:nrow(matRF), trSamp, replace=FALSE)
  train <- matRF[tr_idx,]
  test <- matRF[-tr_idx,]
  
  # execute Random Forests
  output.forest_all <- randomForest(TotalWater ~., data=train, importance=TRUE)
  watpred <- predict(output.forest_all,test)
  predCor <- cor(watpred, test$TotalWater)
  
  # debug
  # plot(watpred, test$TotalWater, main=round(predCor,2))
  
  # report end time
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time, units=c("secs"))
  time.taken
  
  # save water crossplot image
  # jpeg(file="watplot.jpeg")
  fileNam <- c("_watplot.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam) # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/ ", repl="/") # remove blank spaces
  png(filename=filePath)
  mainStr <- paste("Correlation = ", round(predCor,2))
  plot(test$TotalWater, watpred, type="p", pch=15, main=mainStr, xlab="Measured Water", ylab="Predicted Water", xlim=c(0,max(max(watpred),max(test$TotalWater))), ylim=c(0,max(max(watpred),max(test$TotalWater))))
  lines(x=c(0,max(max(watpred),max(test$TotalWater))), y=c(0,max(max(watpred),max(test$TotalWater))), col="red" )
  dev.off()
  
  # save well ranking image
  # jpeg(file="ranking.jpeg")
  mainStr <- paste("Time taken for model construction = ", round(time.taken,0))
  fileNam <- c("_ranking.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/", repl="/") # remove blank spaces
  png(filename=filePath)
  varImpPlot(output.forest_all, sort=TRUE, main=paste(mainStr, "seconds"))
  dev.off()
  
  
  # create importance table
  imp_table <- c()
  
  # report table
  val_input <- "All"
  no_id <- rDat$ID
  
  for (j in 1:2)
  {
    
    if (j==1)
    {
      val_type <- "MSE"
    } else {
      val_type <- "Purity"
    }
    
    newTab <- importance(output.forest_all, type=j)
    
    for (i in 1:nrow(newTab))
    {
      nam <- rownames(newTab)[i]
      well_name <- nam
      well_score <- newTab[i]
      # well_name <- gsub(".Gas", "", nam)
      # well_name <- gsub(".WHP", "", nam)
      # well_name <- gsub(".WHT", "", nam)
      # well_name <- gsub(".SandRate", "", nam)
      
      imp_table <- rbind(c(no_id, well_name, val_input, as.numeric(well_score), val_type), imp_table)
    }
  }
  
  
  # ---------------------------------------------------
  
  
  
  
  
  
  
  # ---------------- RF ranking: matGas ------------------
  
  # matRF
  matRF <- matGas
  
  # split training and test sets
  names(matRF) <- make.names(names(matRF))
  fracTr <- 0.66
  trSamp <- floor(0.66*nrow(matRF))
  tr_idx <- sample(1:nrow(matRF), trSamp, replace=FALSE)
  train <- matRF[tr_idx,]
  test <- matRF[-tr_idx,]
  
  # execute Random Forests
  output.forest_Gas <- randomForest(TotalWater ~., data=train, importance=TRUE)
  watpred <- predict(output.forest_Gas,test)
  predCor <- cor(watpred, test$TotalWater)
  
  # debug
  # plot(watpred, test$TotalWater, main=round(predCor,2))
  
  # report end time
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time, units=c("secs"))
  time.taken
  
  # save water crossplot image
  # jpeg(file="watplot.jpeg")
  fileNam <- c("_watplotGas.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam) # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/ ", repl="/") # remove blank spaces
  png(filename=filePath)
  mainStr <- paste("Correlation = ", round(predCor,2))
  plot(test$TotalWater, watpred, type="p", pch=15, main=mainStr, xlab="Measured Water", ylab="Predicted Water", xlim=c(0,max(max(watpred),max(test$TotalWater))), ylim=c(0,max(max(watpred),max(test$TotalWater))))
  lines(x=c(0,max(max(watpred),max(test$TotalWater))), y=c(0,max(max(watpred),max(test$TotalWater))), col="red" )
  dev.off()
  
  # save well ranking image
  # jpeg(file="ranking.jpeg")
  mainStr <- paste("Time taken for model construction = ", round(time.taken,0))
  fileNam <- c("_rankingGas.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/", repl="/") # remove blank spaces
  png(filename=filePath)
  varImpPlot(output.forest_Gas, sort=TRUE, main=paste(mainStr, "seconds"))
  dev.off()
  
  
  # report table
  val_input <- "G"
  no_id <- rDat$ID
  
  for (j in 1:2)
  {
    
    if (j==1)
    {
      val_type <- "MSE"
    } else {
      val_type <- "Purity"
    }
    
    newTab <- importance(output.forest_Gas, type=j)
    
    for (i in 1:nrow(newTab))
    {
      nam <- rownames(newTab)[i]
      well_name <- nam
      well_score <- newTab[i]
      well_name <- gsub(".Gas", "", nam)
      # well_name <- gsub(".WHP", "", nam)
      # well_name <- gsub(".WHT", "", nam)
      # well_name <- gsub(".SandRate", "", nam)
      
      imp_table <- rbind(c(no_id, well_name, val_input, as.numeric(well_score), val_type), imp_table)
    }
  }
  
  
  # ------------------------------------------------------
  
  
  
  
  
  
  # ---------------- RF ranking: matP ------------------
  
  # matRF
  matRF <- matP
  
  # split training and test sets
  names(matRF) <- make.names(names(matRF))
  fracTr <- 0.66
  trSamp <- floor(0.66*nrow(matRF))
  tr_idx <- sample(1:nrow(matRF), trSamp, replace=FALSE)
  train <- matRF[tr_idx,]
  test <- matRF[-tr_idx,]
  
  # execute Random Forests
  output.forest_WHP <- randomForest(TotalWater ~., data=train, importance=TRUE)
  watpred <- predict(output.forest_WHP,test)
  predCor <- cor(watpred, test$TotalWater)
  
  # debug
  # plot(watpred, test$TotalWater, main=round(predCor,2))
  
  # report end time
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time, units=c("secs"))
  time.taken
  
  # save water crossplot image
  # jpeg(file="watplot.jpeg")
  fileNam <- c("_watplotP.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam) # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/ ", repl="/") # remove blank spaces
  png(filename=filePath)
  mainStr <- paste("Correlation = ", round(predCor,2))
  plot(test$TotalWater, watpred, type="p", pch=15, main=mainStr, xlab="Measured Water", ylab="Predicted Water", xlim=c(0,max(max(watpred),max(test$TotalWater))), ylim=c(0,max(max(watpred),max(test$TotalWater))))
  lines(x=c(0,max(max(watpred),max(test$TotalWater))), y=c(0,max(max(watpred),max(test$TotalWater))), col="red" )
  dev.off()
  
  # save well ranking image
  # jpeg(file="ranking.jpeg")
  mainStr <- paste("Time taken for model construction = ", round(time.taken,0))
  fileNam <- c("_rankingP.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/", repl="/") # remove blank spaces
  png(filename=filePath)
  varImpPlot(output.forest_WHP, sort=TRUE, main=paste(mainStr, "seconds"))
  dev.off()
  
  
  # report table
  val_input <- "P"
  no_id <- rDat$ID
  
  for (j in 1:2)
  {
    
    if (j==1)
    {
      val_type <- "MSE"
    } else {
      val_type <- "Purity"
    }
    
    newTab <- importance(output.forest_WHP, type=j)
    
    for (i in 1:nrow(newTab))
    {
      nam <- rownames(newTab)[i]
      well_name <- nam
      well_score <- newTab[i]
      # well_name <- gsub(".Gas", "", nam)
      well_name <- gsub(".WHP", "", nam)
      # well_name <- gsub(".WHT", "", nam)
      # well_name <- gsub(".SandRate", "", nam)
      
      imp_table <- rbind(c(no_id, well_name, val_input, as.numeric(well_score), val_type), imp_table)
    }
  }
  
  # ------------------------------------------------------
  
  
  
  
  
  
  # ---------------- RF ranking: matT ------------------
  
  # matRF
  matRF <- matT
  
  # split training and test sets
  names(matRF) <- make.names(names(matRF))
  fracTr <- 0.66
  trSamp <- floor(0.66*nrow(matRF))
  tr_idx <- sample(1:nrow(matRF), trSamp, replace=FALSE)
  train <- matRF[tr_idx,]
  test <- matRF[-tr_idx,]
  
  # execute Random Forests
  output.forest_WHT <- randomForest(TotalWater ~., data=train, importance=TRUE)
  watpred <- predict(output.forest_WHT,test)
  predCor <- cor(watpred, test$TotalWater)
  
  # 
  
  # debug
  # plot(watpred, test$TotalWater, main=round(predCor,2))
  
  # report end time
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time, units=c("secs"))
  time.taken
  
  # save water crossplot image
  # jpeg(file="watplot.jpeg")
  fileNam <- c("_watplotT.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam) # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/ ", repl="/") # remove blank spaces
  png(filename=filePath)
  mainStr <- paste("Correlation = ", round(predCor,2))
  plot(test$TotalWater, watpred, type="p", pch=15, main=mainStr, xlab="Measured Water", ylab="Predicted Water", xlim=c(0,max(max(watpred),max(test$TotalWater))), ylim=c(0,max(max(watpred),max(test$TotalWater))))
  lines(x=c(0,max(max(watpred),max(test$TotalWater))), y=c(0,max(max(watpred),max(test$TotalWater))), col="red" )
  dev.off()
  
  # save well ranking image
  # jpeg(file="ranking.jpeg")
  mainStr <- paste("Time taken for model construction = ", round(time.taken,0))
  fileNam <- c("_rankingT.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/", repl="/") # remove blank spaces
  png(filename=filePath)
  varImpPlot(output.forest_WHT, sort=TRUE, main=paste(mainStr, "seconds"))
  dev.off()
  
  
  # report table
  val_input <- "T"
  no_id <- rDat$ID
  
  for (j in 1:2)
  {
    
    if (j==1)
    {
      val_type <- "MSE"
    } else {
      val_type <- "Purity"
    }
    
    newTab <- importance(output.forest_WHT, type=j)
    
    for (i in 1:nrow(newTab))
    {
      nam <- rownames(newTab)[i]
      well_name <- nam
      well_score <- newTab[i]
      # well_name <- gsub(".Gas", "", nam)
      # well_name <- gsub(".WHP", "", nam)
      well_name <- gsub(".WHT", "", nam)
      # well_name <- gsub(".SandRate", "", nam)
      
      imp_table <- rbind(c(no_id, well_name, val_input, as.numeric(well_score), val_type), imp_table)
    }
  }
  
  
  # ------------------------------------------------------
  
  
  
  
  
  # ---------------- RF ranking: matS ------------------
  
  # matRF
  matRF <- matS
  
  # split training and test sets
  names(matRF) <- make.names(names(matRF))
  fracTr <- 0.66
  trSamp <- floor(0.66*nrow(matRF))
  tr_idx <- sample(1:nrow(matRF), trSamp, replace=FALSE)
  train <- matRF[tr_idx,]
  test <- matRF[-tr_idx,]
  
  # execute Random Forests
  output.forest_Sand <- randomForest(TotalWater ~., data=train, importance=TRUE)
  watpred <- predict(output.forest_Sand,test)
  predCor <- cor(watpred, test$TotalWater)
  
  # debug
  # plot(watpred, test$TotalWater, main=round(predCor,2))
  
  # report end time
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time, units=c("secs"))
  time.taken
  
  # save water crossplot image
  # jpeg(file="watplot.jpeg")
  fileNam <- c("_watplotS.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam) # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/ ", repl="/") # remove blank spaces
  png(filename=filePath)
  mainStr <- paste("Correlation = ", round(predCor,2))
  plot(test$TotalWater, watpred, type="p", pch=15, main=mainStr, xlab="Measured Water", ylab="Predicted Water", xlim=c(0,max(max(watpred),max(test$TotalWater))), ylim=c(0,max(max(watpred),max(test$TotalWater))))
  lines(x=c(0,max(max(watpred),max(test$TotalWater))), y=c(0,max(max(watpred),max(test$TotalWater))), col="red" )
  dev.off()
  
  # save well ranking image
  # jpeg(file="ranking.jpeg")
  mainStr <- paste("Time taken for model construction = ", round(time.taken,0))
  fileNam <- c("_rankingS.png") # specify file name
  fileNam <- paste(rDat$ID,fileNam, sep="")
  filePath <- paste(saveDir, fileNam, sep="") # concatenate file name with file directory
  filePath <- str_replace_all(string=filePath, pattern="/", repl="/") # remove blank spaces
  png(filename=filePath)
  varImpPlot(output.forest_Sand, sort=TRUE, main=paste(mainStr, "seconds"))
  dev.off()
  
  
  # report table
  val_input <- "S"
  no_id <- rDat$ID
  
  for (j in 1:2)
  {
    
    if (j==1)
    {
      val_type <- "MSE"
    } else {
      val_type <- "Purity"
    }
    
    newTab <- importance(output.forest_Sand, type=j)
    
    for (i in 1:nrow(newTab))
    {
      nam <- rownames(newTab)[i]
      well_name <- nam
      well_score <- newTab[i]
      # well_name <- gsub(".Gas", "", nam)
      # well_name <- gsub(".WHP", "", nam)
      # well_name <- gsub(".WHT", "", nam)
      well_name <- gsub(".SandRate", "", nam)
      
      imp_table <- rbind(c(no_id, well_name, val_input, as.numeric(well_score), val_type), imp_table)
    }
  }
  
  
  # ------------------------------------------------------
  
  
  
  
  # ------------ insert values from imp_table to database view -------------
  
  # giva column names to imp_table
  imp_table <- as.data.frame(imp_table)
  colnames(imp_table) <- c("ID", "well", "input", "score", "type")
  
  # establish database connection
  dbconnection <- odbcDriverConnect("Driver={****};Server=****; Database=****;Uid=****; Pwd=****;")
  
  # clear
  delQ <- sqlQuery(dbconnection, paste("delete from RF where ID = 1"))
  
  s1 <- c("Insert into RF (ID, well, Input, Score, Type) Values (")
  for (i in 1:nrow(imp_table))
  {
    strVal <- paste(",", "'", imp_table$well[i], "'", ",", "'", imp_table$input[i],  "'", ",", "'", imp_table$score[i],  "'", ",", "'", imp_table$type[i], "'", ")")
    strVal <- str_replace_all(string=strVal, pattern=" ", "")
    strComp <- paste(s1, imp_table$ID[i], strVal)
    
    upQ <- sqlQuery(dbconnection, strComp)
  }
  
  # debug
  loadQ <- sqlQuery(dbconnection, paste("select * from RF"))
  loadQ
  
  odbcClose(dbconnection)
  
  # ------------------------------------------------------------------------
  
  
  
}


