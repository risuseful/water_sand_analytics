# ********************************** #
# funcSand_4.R 
# 
# ********************************** #









# ------------------------------------------- grab ckx sand -------------------------------------------------------

grab_ckx_sand <- function(tInit, tEnd)
{
  # Description:
  # This function grabs amount of collected sand at the CKX platform
  # Note: any report from FSO is not included
  
  # define sql statement (date + hours)
  s1    <- c("select cast(samplingdatetime as date) as tDate, facilityID, sand_kg from vSand ")
  s2    <- c("where (platform not like 'FSO%') and (cast(samplingdatetime as date) between ")
  s3    <- paste("'", tInit, "'")
  s3    <- str_replace_all(string=s3, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s4    <- c(" and ")
  s5    <- paste("'", tEnd, "'")
  s5    <- str_replace_all(string=s5, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s6    <- c(") order by cast(samplingdatetime as date) asc ")
  s_all <- paste(s1, s2, s3, s4, s5, s6)
  
  # grab input data from sql server
  dbconnection <- odbcDriverConnect("Driver={*****};Server=*****; Database=*****;Uid=*****; Pwd=*****;") # set up connection via ODBC to the SQL server
  d  <- sqlQuery(dbconnection, s_all) # grab timestamp, hours included
  odbcClose(dbconnection ) # close the localhost-SQLserver connection
  
  return(d)
}

# -----------------------------------------------------------------------------------------------------------------










# ----------------------------------------- grab timestamp sql server ----------------------------------------------
grab_timestamp <- function(tInit, tEnd)
{
  
  # Description:
  # This function grabs distinct well names from well data table
  
  
  
  
  # define sql statement (date + hours)
  s1    <- c("select distinct timestamp from exatrend ")
  s2    <- c("where cast(timestamp as date) between ")
  s3    <- paste("'", tInit, "'")
  s3    <- str_replace_all(string=s3, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s4    <- c(" and ")
  s5    <- paste("'", tEnd, "'")
  s5    <- str_replace_all(string=s5, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s6    <- c("order by timestamp asc ")
  s_all <- paste(s1, s2, s3, s4, s5, s6)
  
  # define sql statement (date only)
  t1    <- c("select distinct cast(timestamp as date) from exatrend")
  t2    <- c("order by cast(timestamp as date) asc")
  t_all <- paste(t1, s2, s3, s4, s5, t2)
  
  # grab input data from sql server
  dbconnection <- odbcDriverConnect("Driver={*****};Server=*****; Database=*****;Uid=*****; Pwd=*****;") # set up connection via ODBC to the SQL server
  t_hr  <- sqlQuery(dbconnection, s_all) # grab timestamp, hours included
  t_day  <- sqlQuery(dbconnection, t_all) # grab timestamp, but hours NOT included
  odbcClose(dbconnection ) # close the localhost-SQLserver connection
  
  # create a list to return results to function
  # note: multi-argument returns are not permitted in R
  timestampList <- list("t_hr" = t_hr, "t_day" = t_day)
  
  return(timestampList)
}
# -------------------------------------------------------------------------------------------------------------------









# ----------------------------------------- grab well names sql server ----------------------------------------------
grab_wellNames <- function()
{
  
  # Description:
  # 1. This function grabs distinct well names from well data table
  # 2. Add a column that shows indices related to each well name
  
  # set up connection via ODBC to the SQL server
  dbconnection <- odbcDriverConnect("Driver={*****};Server=*****; Database=*****;Uid=*****; Pwd=*****;")
  
  # define sql statement
  s <- c("select distinct well from exatrend order by well asc")
  
  # grab input data from sql server
  d <- sqlQuery(dbconnection, s) # concatenate strings and perform sql query
  
  # close the localhost-SQLserver connection
  odbcClose(dbconnection )
  
  # create indices for each well
  nr <- nrow(d)
  d <- cbind(d,d)
  names(d) <- c("wellName", "wellIndex")
  d$wellIndex <- 0
  for (i in seq(1,nr,1))
  {
    # fill in
    d[i,2] <- i
  }
  
  return(d)
}
# -------------------------------------------------------------------------------------------------------------------









# ----------------------------------------- grab well data table from sql server ----------------------------------------------
grab_exatrend_well <- function(wellName, tInit, tEnd, tFirst)
{
  
  # Description:
  # This function does the following:
  # 1. grab data from sql server
  # 2. add dt (delta timestamp) column, i.e. t-tFirst
  # 3. rename columns with wellNames (easier for data merging with other wells)
  
  library(mice) # for imputation
  
  # set up connection via ODBC to the SQL server
  dbconnection <- odbcDriverConnect("Driver={*****};Server=*****; Database=*****;Uid=*****; Pwd=*****;")
  
  # define sql statement
  s1     <- c("select ")
  s2     <- c("timestamp, well, ")
  
  s3     <- c("case ")
  # s4     <- c("when gas < 0.1 then NULL ") # defining missing gas
  s4     <- c(" ")
  s5     <- c("when gas is NULL then NULL ") # defining missing gas
  s6     <- c("else gas")
  s7     <- c("end as 'gas_w_na', ")
  
  s8     <- c("case ")
  s9     <- c("when whp < 0.1 then NULL ") # defining missing whp
  s10     <- c("when whp is NULL then NULL ") # defining missing whp
  s11    <- c("else whp")
  s12    <- c("end as 'whp_w_na', ")
  
  s13     <- c("case ")
  s14     <- c("when wht < 0.1 then NULL ") # defining missing wht
  s15     <- c("when wht is NULL then NULL ") # defining missing wht
  s16     <- c("else wht")
  s17     <- c("end as 'wht_w_na', ")
  
  s18     <- c("case ")
  # s19     <- c("when choke < 0.1 then NULL ") # defining missing choke
  s19     <- c(" ") # defining missing choke
  s20     <- c("when choke is NULL then NULL ") # defining missing choke
  s21     <- c("else choke")
  s22     <- c("end as 'choke_w_na', ")
  
  s23     <- c("case ")
  s24     <- c("when rawsand < 0.1 then NULL ") # defining missing rawsands
  s25     <- c("when rawsand is NULL then NULL ") # defining missing rawsands
  s26     <- c("else rawsand")
  s27     <- c("end as 'rawsand_w_na', ")
  
  s28     <- c("case ")
  s29     <- c("when rawsand < 0.1 then NULL ")
  s30     <- c("when rawsand is NULL then NULL ") # defining missing rawsands
  s31     <- c("else sandrate ")
  s32    <- c("end as 'sandrate_w_na'")
  
  s33    <- c("from exatrend")
  
  s34    <- c("where (cast(timestamp as date) between ")
  s35    <- paste("'", tInit, "'")
  s35    <- str_replace_all(string=s35, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s36    <- c(" and ")
  s37    <- paste("'", tEnd, "'")
  s37    <- str_replace_all(string=s37, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s38    <- paste("'", wellName, "')")
  s38    <- str_replace_all(string=s38, pattern=" ", repl="")
  s38    <- paste(" and well = ", s38)
  
  s39    <- c(" order by timestamp asc, well asc")
  
  s_all  <- paste(	s1, s2,  s3,  s4,  s5, 
                   s6,  s7,  s8,  s9,  s10, 
                   s11, s12, s13, s14, s15, 
                   s16, s17, s18, s19, s20, 
                   s21, s22, s23, s24, s25, 
                   s26, s27, s28, s29, s30, 
                   s31, s32, s33, s34, s35,
                   s36, s37, s38, s39)
  
  
  # grab input data from sql server
  d <- sqlQuery(dbconnection, paste(s_all)) # concatenate strings and perform sql query
  
  # close the localhost-SQLserver connection
  odbcClose(dbconnection )
  
  # # adding dt (delta time) into d
  # # method: difftime(d$timestamp, min(d$timestamp), unit="hours")
  dt <- difftime(d$timestamp, tFirst, unit="hours")
  d  <- cbind(d,dt)
  names(d)[9] <- c("dt")
  
  # rename columns
  sep      <- "_"
  namG     <- str_replace_all(wellName, "-", repl = "_")
  namP     <- str_replace_all(wellName, "-", repl = "_")
  namT     <- str_replace_all(wellName, "-", repl = "_")
  namC     <- str_replace_all(wellName, "-", repl = "_")
  namR     <- str_replace_all(wellName, "-", repl = "_")
  namS     <- str_replace_all(wellName, "-", repl = "_")
  namG     <- str_replace_all(string = paste(namG, sep, "G"), " ", repl = "")
  namP     <- str_replace_all(string = paste(namP, sep, "P"), " ", repl = "")
  namT     <- str_replace_all(string = paste(namT, sep, "T"), " ", repl = "")
  namC     <- str_replace_all(string = paste(namC, sep, "C"), " ", repl = "")
  namR     <- str_replace_all(string = paste(namR, sep, "R"), " ", repl = "")
  namS     <- str_replace_all(string = paste(namS, sep, "S"), " ", repl = "")
  names(d) <- c("t", "wellName", namG, namP, namT, namC, namR, namS,"dt")
  
  
  # imputation
  tempData <- mice(d[,c(3,4,5,6,7,9)], m=5, maxit=maxit_impute, meth='norm.nob', seed=500) # sandrate not included
  d_complete <- mice::complete(tempData,1)
  
  
  # reattach timestamp
  d_complete <- cbind(d$t, d_complete)
  names(d_complete)[1] <- c("t")
  
  # remove the "dt" column
  d_complete <- d_complete[,-7]
  
  return(d_complete)
}
# -----------------------------------------------------------------------------------------------------------------------------









# ----------------------------------------- grab well data table from sql server ----------------------------------------------
grab_exatrend <- function(tInit, tEnd, tFirst, wellNames)
{
  
  # Description:
  # This function does the following:
  # 1. grab data from sql server
  # 2. add dt (delta timestamp) column, i.e. t-tFirst
  # 3. add column that stores indices based on well names
  
  # set up connection via ODBC to the SQL server
  dbconnection <- odbcDriverConnect("Driver={*****};Server=*****; Database=*****;Uid=*****; Pwd=*****;")
  
  # define sql statement
  s1     <- c("select ")
  s2     <- c("timestamp, well, gas, whp, wht, choke, ")
  s3     <- c("case ")
  s4     <- c("when rawsand < 0.1 then NULL ") # defining missing rawsands
  s5     <- c("else rawsand")
  s6     <- c("end as 'rawsand_w_na', ")
  s7     <- c("case ")
  s8     <- c("when rawsand < 0.1 then NULL when rawsand is NULL then NULL ") # defining missing rawsands
  s9     <- c("else sandrate ")
  s10    <- c("end as 'sandrate_w_na'")
  s11    <- c("from exatrend")
  s12    <- c("where (cast(timestamp as date) between ")
  s13    <- paste("'", tInit, "'")
  s13    <- str_replace_all(string=s13, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s14    <- c(" and ")
  s15    <- paste("'", tEnd, "'")
  s15    <- str_replace_all(string=s15, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s16    <- paste(")", " and (whp > 0.01 and wht > 0.01 and choke > 0.01)")
  s17    <- c(" order by timestamp asc, well asc")
  s_all  <- paste(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17)
  
  # grab input data from sql server
  d <- sqlQuery(dbconnection, paste(s_all)) # concatenate strings and perform sql query
  
  # close the localhost-SQLserver connection
  odbcClose(dbconnection )
  
  # adding dt (delta time) into d
  # method: difftime(d$timestamp, min(d$timestamp), unit="hours")
  dt <- difftime(d$timestamp, tFirst, unit="hours")
  d  <- cbind(d,dt)
  
  
  
  # add column: well index
  # -------------------------- example ---------------------------
  # df <- c("A", "C", "E", "D")
  # df <- cbind(df, c("Mali", "Indi", "Cudi", "Dadi"))
  # df <- as.data.frame(df)
  # names(df) <- c("category", "country")
  # 
  # look <- cbind(c("A", "B", "C", "D", "E"), c(90,22,37,42,51))
  # look <- as.data.frame(look)
  # names(look) <- c("category", "index")
  # 
  # new <- df$category
  # new <- look$index[match(unlist(df$category), look$category)]
  # new
  # --------------------------------------------------------------
  df <- d$well
  look <- wellNames
  new <- df
  new <- look$wellIndex[match(unlist(df), look$wellName)]
  d <- cbind(d,new) # binding well indices with matrix d
  
  # rename columns
  names(d) <- c("t", "wellName", "G", "P", "T", "Chok", "rawSand", "sandRate", "dt", "wellIndex")
  
  return(d)
}
# -----------------------------------------------------------------------------------------------------------------------------





distribute_sand <- function(x)
{
  # --------------------------------------------------------------------------
  # Description:
  # This function distributes intermittently collected sands (from desanders)
  # into daily sand rates.  Sand was measured in 'kg'.
  #
  #    Old Values New Values
  # 1          11   1.666667
  # 2           0   1.666667
  # 3           0   1.666667
  # 4           5   1.200000
  # 5           0   1.200000
  # 6           0   1.200000
  # 7           0   1.200000
  # 8           0   1.200000
  # 9           6  20.000000
  # 10         20   0.000000
  # --------------------------------------------------------------------------
  
  # convert x to other format
  x <- unlist(x)
  x <- as.matrix(x)
  
  
  # initiate vector to store new values from re-distribution
  val_vec <- matrix(0, nrow(x), 1) # a vector that will store new values (from re-distribution)
  
  
  # if all values in x are zero, then no need to distribute
  if(sum(x[2:nrow(x)])==0) # note: first value is always ignored (recommendation by Beyond Limits)
  {
    val_vec <- 0
    res <- as.data.frame(cbind(x, val_vec)) # bind results with original vector
  } else
  {
    # algorithm trick: set first value of x to be bigger than 0, i.e. 99999
    x_bak <- x
    if(x[1]==0)
    {
      x[1] <- 9999
    }
    
    # create a table 'tab' to store key information from x
    tab <- as.data.frame(cbind(x[x>0], which(x>0))) # track for values and locations of non-zero values
    names(tab) <- c("val", "loc")
    
    
    
    # specify how we want to distributes the non-zeros to the zeros in x
    divisor <- matrix(0, nrow(tab), 1)
    new_val <- matrix(0, nrow(tab), 1)
    for (i in 2:nrow(tab))
    {
      divisor[i] <- tab$loc[i]-tab$loc[i-1] # specify denominator for non-zero values
      new_val[i] <- tab$val[i] / divisor[i] # distribute non-zero values to zero valued elements in x
    }
    tab <- cbind(tab, divisor, new_val) # bind vectors to obtain a complete form of table 'tab'
    
    # assign tab$new_val to x
    
    
    j <- 2 # pointer to tab.  First value in x is always ignored (recommendation from Beyond Limits)
    
    for (i in 1:(nrow(x)-1))
    {
      # ensure re-distribution to zero-valued cells is consistent with the info in table 'tab'
      if(i < tab$loc[j] && j<=nrow(tab))
      {
        val_vec[i] <- new_val[j]
      } else
      {
        j = j+1 # increment lookup on tab
        
        # assign '0' if index exceeded the number of rows in table 'tab'
        if(j<=nrow(tab))
        {
          val_vec[i] <- new_val[j]
        } else
        {
          val_vec[i] <- 0
        }
        
      }
    }
    
    res <- as.data.frame(cbind(x_bak, val_vec)) # bind results with original vector
  }
  
  
  
  names(res) <- c("old_val", "new_val")
  
  return(res)
  
}


desander_daily <- function(tInit, tEnd)
{
  
  # -----------------------------------------------------------------------
  # Description:
  # Sand rates from different desanders are measured in different times.
  # We need to sync these and come up with a daily sand rate.
  #
  # Functions used:
  # 1. grab_ckx_sand(tInit, tEnd)
  # 2. distribute_sand(res[,2])
  # -----------------------------------------------------------------------
  
  # grab all sand date
  x <- grab_ckx_sand(tInit, tEnd)
  x$tDate <- as.Date(x$tDate) # ensure date column in "Date" format
  
  # find distinct facilityID from database
  facility_name <- sqldf("select distinct facilityid from x") # grab all facility IDs
  facility_name <- as.matrix(facility_name)
  
  tDate <- seq(as.Date(tInit), as.Date(tEnd), "days") # create sequence of dates, in daily
  
  # create matrix y containing date and columns; column names comes from distinct facilityID
  y <- matrix(0, nrow(as.matrix(tDate)), nrow(facility_name))
  y <- as.data.frame(y)
  names(y) <- as.matrix(facility_name) # assign column names
  y <- cbind(tDate,y)
  
  # fill matrix y
  
  y_orig <- y
  i <- 2 # initiate counter
  for (nam in facility_name)
  {
    # nam <- facility_name[]
    # query each facilityID, one at a time
    sql_str <- paste("select * from x where facilityID = ", 
                     str_replace_all(paste("'", nam, "'"), " ", ""))
    look <- sqldf(sql_str)
    
    # create 'df', filled with dates.  These dates will be replaced by sand rate
    df <- y$tDate
    new <- df
    
    # do a 'lookup' approach
    res <- lapply(df, function(x) look$sand_kg[match(x, look$tDate)])
    res <- as.data.frame(as.matrix(res)) # convert to dataframe
    names(res) <- c("sand_kg") # renamce column
    res <- cbind(y$tDate, res) # add Date column to res
    res$sand_kg[is.na(res$sand_kg)] <- 0 # replace NA with 0
    
    # distribute sand to zero-valued cells
    b <- distribute_sand(res[,2])
    
    # store distributed 
    y[,i] <- b[,2]
    y_orig[,i] <- b[,1]
    
    # point to the next facilityID
    i <- i + 1
    
  }
  
  y_final <- list(y, y_orig)
  names(y_final) <- c("new", "old")
  
  return(y_final)
  
}



merge_all_well <- function(wellNames, tInit, tEnd, tFirst)
{
  # ------------------------------------------
  # Description:
  #
  # Warning: This function takes a long time to run.
  #
  # Merge all data from all well, and impute missing values.
  # Warning: well CKC-07's ASD sensor is not working, hence we replace with a harcoded value of '2000'.
  #
  # Functions used: 
  # 1. grab_exatrend_well(wellNames[i,1], tInit, tEnd, tFirst)
  # 2. grab_timestamp(tInit, tEnd)
  
  
  
  # grab timestampList
  timestampList <- grab_timestamp(tInit, tEnd) # A LIST-type object that gives time in 'hours' and in 'days'.
  
  # initiate resultant matrix:
  # Notice that the initated matrix has 1 row.  
  # It will grow once this function starts merging it with data from other wells.
  #
  r <- timestampList$t_hr
  names(r) <- c("t") # rename so that tables will have related columns with the same name
  r_time <- r
  
  # record evolution of r (for debugging)
  r_track <- as.data.frame(matrix(0, nrow(wellNames), 4))
  names(r_track) <- c("wellName", "nrow", "ncol", "missing")
  
  
  # record dimensions of x (i.e. well data)
  x_track <- as.data.frame(matrix(0, nrow(wellNames), 4))
  names(x_track) <- c("wellName", "nrow", "ncol", "missing")
  
  # loop through well names, and do "inner join"
  for (i in 1:nrow(wellNames))
    # for (i in 1:3)
  {
    # grab well data from SQL Server
    x <- grab_exatrend_well(wellNames[i,1], tInit, tEnd, tFirst)
    
    # track evolution of x
    x_track[i,1] <- wellNames[i,1]
    x_track[i,2] <- nrow(as.matrix(x))
    x_track[i,3] <- ncol(as.matrix(x))
    x_track[i,4] <- sum(is.na(x))
    
    # do a left join between r_time and x
    x <- merge(r_time, x, by="t", all.r_time = TRUE)
    
    # impute x
    tempData <- mice(x, m=5, maxit=maxit_impute, meth='norm.nob', seed=500)
    x <- mice::complete(tempData,1)
    
    # do an left join to update 'r'
    r <- merge(r,x, by = "t", all.r = TRUE)
    
    # track evolution of r
    r_track[i,1] <- wellNames[i,1]
    r_track[i,2] <- nrow(as.matrix(r))
    r_track[i,3] <- ncol(as.matrix(r))
    r_track[i,4] <- sum(is.na(r))
    
  }
  
  
  # exceptions
  # 1. CKC-07 clampon was dead from 2016-2019 due to faulty sensor
  r$CKC_07_R <- 2000
  
  
  # return results
  results <- list(r, r_track, x_track, sum(is.na(r)))
  names(results) <- c("Result", "trackResult", "trackWellData", "missingVals")
  return(results)
  
}






range_ASD_coeff <- function(tInit, tEnd)
{
  # -------------------------------------------------------------------------
  # Description:
  #
  # This function computes the range of values for ASD_coeff.
  # ASD_coeff can be used to convert rawsand (ASD measurement) into sandrate.
  #
  # 27-FEB-2020, akmalaulia
  #
  # -------------------------------------------------------------------------
  
  # define sql statement (date + hours)
  s1    <- c("select max(sandrate/rawsand) as max, min(sandrate/rawsand) as min from exatrend ")
  s2    <- c("where (rawsand > 0) and (cast(timestamp as date) between ")
  s3    <- paste("'", tInit, "'")
  s3    <- str_replace_all(string=s3, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s4    <- c(" and ")
  s5    <- paste("'", tEnd, "')")
  s5    <- str_replace_all(string=s5, pattern=" ", repl="")  # removing unnecessary spaces, i.e. ' 2016-01-01 ' should be '2016-01-01'
  s_all <- paste(s1, s2, s3, s4, s5)
  
  # execute sql
  # grab input data from sql server
  dbconnection <- odbcDriverConnect("Driver={*****};Server=*****; Database=*****;Uid=*****; Pwd=*****;") # set up connection via ODBC to the SQL server
  d  <- sqlQuery(dbconnection, s_all) # grab timestamp, hours included
  odbcClose(dbconnection ) # close the localhost-SQLserver connection
  
  return(d)
  
}









fitGA <- function(ASD_coeff)
{
  
  # ---------------------------------------------------------------------------
  # Description:
  #
  # This function will be used by an optimizer to train a 
  # simple material balance model, i.e. sum(ASD(i)*ASD_coeff(i)) = total_sandrate
  # where i is a well index.
  # 
  # Output: prediction error
  # ---------------------------------------------------------------------------
  
  # convert input to matrices
  train_x <- as.matrix(train_x)
  train_y <- as.matrix(train_y)
  ASD_coeff <- as.matrix(ASD_coeff)
  
  # compute predicted sand 
  pred_y <- train_x%*%ASD_coeff
  
  # measure error
  rmse_sand <- sqrt(mean(((train_y - pred_y)^2)))
  rmse_sand <- -rmse_sand # GA is set up to maximize, not minimize.  0 is the maximum in this case.
  
  # return error
  return(rmse_sand)
  
}
