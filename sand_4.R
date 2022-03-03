# ********************************** #
# sand_4.R 
# 

# Description:
# This program finds the rawsand to sandrate conversion factor
# using simple material balance equation.
#
# Acronyms:
# WBD: line or block of lines Will Be Deactivated
# ********************************** #









# --------------------- prelims ----------------------
rm(list = ls()) # remove data
set.seed(123) # setting seed
start.time <- Sys.time() # record time lapse
# -----------------------------------------------------





# ---------------------- config ---------------------
# loadCSV <- c("y") # NOT ACTIVE: load data from csv instead of sql; this is specific to "all_w <- merge_all_well(wellNames, tInit, tEnd, tFirst)"







# ------------------ call library ---------------------
library(randomForest)
library(stats)
library(RODBC)
# library(RODBCDBI)
library(stringi)
library(stringr)
library(data.table)
library(rlang)
library(sqldf)                      # sql query
library(Metrics)                    # rmse(), etc
library(MultivariateRandomForest)
library(neuralnet)
library(mice)
library(GA)
library(parallel)
library(doParallel)
library(reshape)
# ------------------------------------------------------









# ------------------ set directories and create functions --------------------
# setwd("*****") # debug
# saveDir <- c("*****") # debug
source('funcSand_4.R')# create all functions
# ----------------------------------------------------------------------------









# ---------------- constants ------------------------

# time
tInit = c("2016-09-01") # WBD (?)
tEnd  = c("2019-12-30") # WBD (?)
tFirst = c("2005-01-05") # the first time CHOC produces gas

# well name
wellName <- '*****-04' # to deactivate

# set maxit_impute
maxit_impute <<- 5 # (global variables)


# ---------------------------------------------------









# ---------------- grab data from server -----------------------

# grab well names
wellNames <- grab_wellNames()

# grab timestamp
timestampList <- grab_timestamp(tInit, tEnd)

# grap ckx_sand
all_sand <- desander_daily(tInit, tEnd)
all_sand_day <- all_sand$new
all_sand_day$tDate <- as.Date(all_sand_day$tDate, format = "%Y-%m-%d") # convert tDate to date format


# grab all well data
all_w    <- merge_all_well(wellNames, tInit, tEnd, tFirst)
all_w_hr <- all_w$Result
names(all_w_hr)[1] <- c("tDate")                        # rename the first column
all_w_hr$tDate <- as.Date(all_w_hr$tDate, format = "%Y-%m-%d") # convert tDate to date format

# --------------------------------------------------------------







# ---------------- save workspace ------------------

# save
str <- paste("grabDataFromServer", Sys.time(), ".RData")
str <- str_replace_all(str, " ", "_")
str <- str_replace_all(str, ":", "_")
str <- str_replace_all(str, "-", "_")
save.image(file=str) # save workspace to current directory

# load onto workspace
# load(file="<filename>.RData")



# ---------------------------------------------------









# ------------------------ convert all_w_hr to daily -----------------------

# build sql script to find the daily average
s <- names(all_w_hr)[2:ncol(all_w_hr)]                # store all column names of all_w_hr, except "tDate"
s <- str_replace_all(paste("avg(", s, "),"), " ", "") # remove blank spaces
s <- paste(s, collapse = ' ')                         # collapse all names into 1 string
s <- substr(s, 1, nchar(s)-1)                         # remove the last character
sql_str <- paste("select tDate, ", s, "from all_w_hr group by tDate")

# perform query
all_w_day <- sqldf(sql_str)
dim(all_w_day)

# rename column names in 'all_w_day'
sn <- colnames(all_w_day)
sn <- str_replace_all(sn, "avg", "")
sn <- str_replace(sn, "\\(", "") # remove left bracket
sn <- str_replace(sn, "\\)", "") # remove right bracket
colnames(all_w_day) <- sn
# --------------------------------------------------------------------------







# --------------------- compile dataset for material balance approach -----------------

# target vector: sand_total
sand_total <- rowSums(all_sand_day[2:ncol(all_sand_day)])  # index start from '2' to exclude "tDate" column
sand_total <- as.data.frame(sand_total)
sand_total <- cbind(all_sand_day$tDate, sand_total) # reattach the "tDate" column
names(sand_total)[1] <- c("tDate") # rename the "...$tDate" column

# input matrix: rawsand only (i.e. ASD, stands for Acoustic Sand Detector)
in.asd <- all_w_day[,c(1,grep("_R", names(all_w_day)))]

# merge 'in.asd' and 'sand_total', using INNER JOIN
dat <- merge(in.asd, sand_total, by = "tDate")

# create training and test sets
ind <- which(dat$tDate<"2019-01-01") # indices for training set

train_ <- dat[ind,2:ncol(dat)] # training set
train_x <<- train_[1:(ncol(train_)-1)] # input (global variable)
train_y <<- train_[ncol(train_)] # observed output (global variable)

test_  <- dat[-ind,2:ncol(dat)] # test set
test_x <- test_[1:(ncol(test_)-1)]
test_y <- test_[ncol(test_)]

# -------------------------------------------------------------------------------------------







# --------------------- GA-based optimization to find ASD_coeff -----------------------------------

# Note:
# variable ASD_coeff: It is used to convert rawsand (i.e. ASD measurements) into sandrate

# set range of values for asd_coeff
ASD_range <- range_ASD_coeff(tInit, tEnd)  # get the min and max values for asc_coeff
min_ASD_coeff <- matrix(ASD_range$min, (ncol(train_)-1), ,1)
max_ASD_coeff <- matrix(ASD_range$max, (ncol(train_)-1), ,1)

# solve for ASD_coeff using Islands GA + BFGS
gaRes <- gaisl(type = "real-valued", 
               fitness=fitGA, 
               lower = min_ASD_coeff, 
               upper = max_ASD_coeff, 
               popSize = 300, 
               maxiter = 100, 
               parallel=TRUE, 
               numIslands=10,
               optim=TRUE,
               optimArgs = list(method="L-BFGS-B", poptim=0.05, pressel=0.5))

# report solution
gaRes.solution <- slot(gaRes,"solution")[1,]
gaRes.error    <- slot(gaRes,"fitnessValue")

# convert objects to matrix format
test_x <- as.matrix(test_x)
test_y <- as.matrix(test_y)

# prediction assessment
pred_y <- test_x%*%gaRes.solution

# measure error
rmse_sand <- sqrt(mean(((test_y - pred_y)^2)))

# plot results
t_axis <-  dat[-ind,1] # dates for test set only
main_str <- paste("RMSE = ", round(rmse_sand,0), " kg/day")
plot(t_axis, test_y, cex=0.8, col="red", xaxt="n", xlab="", ylab="Sand (kg/day)", main=main_str)
lines(t_axis, pred_y, type="l", col="blue")
axis.Date(1, at = seq(t_axis[1], t_axis[nrow(as.matrix(t_axis))], length.out=20),
          format= "%Y-%m", las = 2)
# ------------------------------------------------------------------------------------------------






# 
# 
# # -------------------- compile data for machine learning (ml) approach ----------------------
# 
# # # input preparation
# # # note: grab dates from t_axis
# # in_G      <- rowMeans(all_w_day[, grep("_R", names(all_w_day))]) # row-wise average using rowMeans()
# # in_P      <- rowMeans(all_w_day[, grep("_P", names(all_w_day))])
# # in_T      <- rowMeans(all_w_day[, grep("_T", names(all_w_day))])
# # in_C      <- rowMeans(all_w_day[, grep("_C", names(all_w_day))])
# # in_R      <- rowMeans(all_w_day[, grep("_R", names(all_w_day))])
# # out_sand  <- sand_total$sand_total/nrow(wellNames)
# # 
# # # compile all
# # dat.ml    <- cbind(t_axis,in_G, in_P, in_T, in_C, in_R)
# # dat.ml    <- 
# #   
# # # create training and test sets
# # # note: indices 'ind' have already created beforehand.  See previous lines.
# # ml.train_    <- dat.ml[ind,2:ncol(dat.ml)] # training set
# # ml.train_x   <- ml.train_[1:(ncol(ml.train_)-1)] # input
# # ml.train_y   <- ml.train_[ncol(ml.train_)] # observed output
# # 
# # ml.test_     <- dat.ml[-ind,2:ncol(dat.ml)] # test set
# # ml.test_x    <- ml.test_[1:(ncol(ml.test_)-1)]
# # ml.test_y    <- ml.test_[ncol(ml.test_)]
# 
# # # input matrix
# # dat.ml <- merge(all_w_day, sand_total, by = "tDate") # inner join operation
# # 
# # # create training and test sets
# # ml.train_    <- dat.ml[ind,2:ncol(dat.ml)] # training set
# # ml.train_x   <- ml.train_[1:(ncol(ml.train_)-1)] # input
# # ml.train_y   <- ml.train_[ncol(ml.train_)] # observed output
# # 
# # ml.test_     <- dat.ml[-ind,2:ncol(dat.ml)] # test set
# # ml.test_x    <- ml.test_[1:(ncol(ml.test_)-1)]
# # ml.test_y    <- ml.test_[ncol(ml.test_)]
# 
# # input preparation
# in_G      <- rowMeans(all_w_day[, grep("_G", names(all_w_day))]) # row-wise average using rowMeans()
# in_P      <- rowMeans(all_w_day[, grep("_P", names(all_w_day))])
# in_T      <- rowMeans(all_w_day[, grep("_T", names(all_w_day))])
# in_C      <- rowMeans(all_w_day[, grep("_C", names(all_w_day))])
# in_R      <- rowMeans(all_w_day[, grep("_R", names(all_w_day))])
# out_sand  <- cbind(dat$tDate, as.data.frame(dat$sand_total/nrow(wellNames)))
# colnames(out_sand) <- c("tDate", 'well_sand_kg')
# 
# # compile all
# dat2    <- cbind(all_w_day$tDate, in_G, in_P, in_T, in_C, in_R)
# colnames(dat2) <- c("tDate", "G", "P", "T", "C", "R")
# dat2    <- merge(dat2, out_sand, by="tDate") # inner join
# dat.ml <<- dat2[,2:ncol(dat2)] # global variable
# 
# # scale input data
# maxs <- apply(dat.ml, 2, max)
# mins <- apply(dat.ml, 2, min)
# scaled <- as.data.frame(scale(dat.ml, center = mins, scale = maxs - mins))
# 
# # create training and test sets
# train_ <<- scaled[ind,] # global variable
# test_ <- scaled[-ind,]
# 
# # train neuralnet
# n <- names(train_)
# f <- as.formula(paste("well_sand_kg ~", paste(n[!n %in% "well_sand_kg"], collapse = " + ")))
# nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
# 
# # plot nn
# plot(nn)
# 
# # prediction (1 well)
# pr.nn <- predict(nn,test_[,1:(ncol(test_)-1)])
# pr.nn_ <- pr.nn*(max(dat.ml$well_sand_kg)-min(dat.ml$well_sand_kg))+min(dat.ml$well_sand_kg)
# test.r <- (test_$well_sand_kg)*(max(dat.ml$well_sand_kg)-min(dat.ml$well_sand_kg))+min(dat.ml$well_sand_kg)
# 
# # mse
# # note: only for 1 well
# MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
# cor.nn <- cor(test.r, pr.nn_)
# cor.nn
# 
# # --------------------------------------------------------------------------------------
# 
# 
# 
# 
# 
# 
# # -------------------- prediction on total sand (using 'nn' object) ----------------------------
# 
# nw <- nrow(wellNames) # compute number of wells
# row_ind <- which(all_w_day$tDate >= '2019-01-01') # rows for testing
# mat_sand_prod <- matrix(0, nrow(as.matrix(row_ind)), nw) # matrix to store results of prediction
# for (i in 1:nw)
# {
#   col_ind <- seq(2+(i-1)*5, 2+(i-1)*5 + 4, by=1) # take all the cols belonging to that well
#   test_x <- all_w_day[row_ind, col_ind] # form input data
#   
#   # normalize test_x
#   maxs <- apply(test_x, 2, max)
#   mins <- apply(test_x, 2, min)
#   scaled <- as.data.frame(scale(test_x, center = mins, scale = maxs - mins))
#   
#   
#   pred <- predict(nn, test_x) # predicted raw
#   pred_dnorm <- pred*(max(dat.ml$well_sand_kg)-min(dat.ml$well_sand_kg))+min(dat.ml$well_sand_kg) # predicted, denormalized
#   
#   # store predicted results
#   mat_sand_prod[,i] <- pred_dnorm
# }
# 
# total_sand_prod <- apply(mat_sand_prod, 1, sum)
# 
# # compute rmse
# rmse.ml <- sqrt(sum((test_y - total_sand_prod)^2)/nrow(as.matrix(total_sand_prod)))
# rmse.ml
# 
# main_str <- paste("RMSE = ", round(rmse.ml,0), " kg/day")
# plot(t_axis, test_y, cex=0.8, col="red", xaxt="n", xlab="", ylab="Sand (kg/day)", main=main_str) # plot actual
# lines(t_axis, total_sand_prod, type="l", col="blue") # plot predicted
# 
# # plot mean of sand rate from each well
# plot(wellNames[,1], apply(mat_sand_prod,2,mean))
# 
# # export results
# write.csv(file="mach_learn_results.csv", cbind(as.Date(t_axis, format="%Y-%m-%d"), as.data.frame(test_y), as.data.frame(total_sand_prod)))
# write.csv(file="mach_learn_results_well.csv", cbind(as.data.frame(wellNames[,1]), as.data.frame(apply(mat_sand_prod,2,mean)) ))
# 
# # ----------------------------------------------------------------------------------------------
# 
# 
# 



# ---------------- report time lapse -----------------------
end.time <- Sys.time() # record time lapse
sprintf("Started at %s", start.time) # print start time
sprintf("Ended at %s", end.time) # print end time
# ----------------------------------------------------------
