# wgr_func.R
# Akmal, 17JAN2019

# -------------------------- function name: grabDat ---------------------------------
# purpose: pull input data from database server
grabDat <- function(tInit,tEnd)
{
  
  # preprocess time range (include single quotes)
  tInit_q <- paste("'",tInit,"'") # add single quote
  tInit <- str_replace_all(string=tInit_q, pattern=" ", repl="") # remove blank spaces
  tEnd_q <- paste("'",tEnd,"'")
  tEnd <- str_replace_all(string=tEnd_q, pattern=" ", repl="") # remove blank spaces
  
  # preparing strings for SQL query
  # Warning!!! Update this query when you have more wells
  s1 <- c("SELECT [TimeStamp],<***CONFIDENTIAL****> FROM [dbo].[vMatrix] where timestamp between")
  s2 <- c("order by timestamp;")
  
  # grab input data
  d <- sqlQuery(dbconnection, paste(s1,tInit,"and",tEnd,s2)) # concatenate strings and perform sql query
  
  # return d
  
}
# ------------------------------------------------------------------------------------







# -------------------------- function name: grabDat2 ---------------------------------
# purpose: pull input data from database server
grabDatNew <- function(tInit,tEnd)
{
  
  # preprocess time range (include single quotes)
  tInit_q <- paste("'",tInit,"'") # add single quote
  tInit <- str_replace_all(string=tInit_q, pattern=" ", repl="") # remove blank spaces
  tEnd_q <- paste("'",tEnd,"'")
  tEnd <- str_replace_all(string=tEnd_q, pattern=" ", repl="") # remove blank spaces
  
  # preparing strings for SQL query
  # Warning!!! Update this query when you have more wells
  s1 <- c("SELECT [TimeStamp],<****CONFIDENTIAL***>  FROM [dbo].[vMatrix2]  where timestamp between")
  s2 <- c("order by timestamp;")
  
  # grab input data
  d <- sqlQuery(dbconnection, paste(s1,tInit,"and",tEnd,s2)) # concatenate strings and perform sql query
  
  # return d
  
}
# ------------------------------------------------------------------------------------









# -------------------------- function name: fitGA ---------------------------------
# function: fitGA
# purpose: compute fitness function for GA (genetic algorithm)
fitGA <- function(w)
{
  
  # create matrix G (note: setup matrix/vector problem, i.e. G*w = b)
  # note:
  # G      = gross rates for each well in mmscfd (for all timesteps)
  # b_pred = predicted total water produced in bbl (for all timesteps)
  # b_obs  = observed total water produced in bbl (for all timesteps)
  G <- as.matrix(mat[,1:nw])
  w <- as.matrix(w)
  b_obs <- mat[,(nw+1)]
  b_pred <- G%*%w
  
  # measure error
  res <- sqrt(mean(((b_obs-b_pred)^2)))
  res <- -res # GA is set up to maximize, not minimize.  0 is the maximum in this case.
  
  # return error
  return(res)
  
}
# ------------------------------------------------------------------------------------







# -------------------------- function name: fitOp ---------------------------------
# function: fitOp
# purpose: compute fitness function for optim()
fitOp <- function(w)
{
  
  # create matrix G (note: setup matrix/vector problem, i.e. G*w = b)
  # note:
  # G      = gross rates for each well in mmscfd (for all timesteps)
  # b_pred = predicted total water produced in bbl (for all timesteps)
  # b_obs  = observed total water produced in bbl (for all timesteps)
  G <- as.matrix(mat[,1:nw])
  w <- as.matrix(w)
  b_obs <- mat[,(nw+1)]
  b_pred <- G%*%w
  
  # measure error
  res <- sqrt(mean(((b_obs-b_pred)^2)))
  res <- res # GA is set up to maximize, not minimize.  0 is the maximum in this case.
  
  # return error
  return(res)
  
}
# ------------------------------------------------------------------------------------
