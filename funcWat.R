# funcWat.R
# Akmal, 27DEC2018







# ---- create descriptive statistics matrix ----
rawS <- function(matHist, nc_mat, nam) {

	# notes: to find the most frequent element in an array, use
	# as.numeric(names(which.max(table(x))))

	# notes: to find 10th percentile, use
	# quantile(duration, c(0.1)) 

	# get number of rows and cols
	nr <- nrow(matHist)
	nc <- ncol(matHist)

	# duplicate original matrix
	matS_max <- matrix(0,nr,nc_mat)
	matS_min <- matrix(0,nr,nc_mat)
	matS_avg <- matrix(0,nr,nc_mat)
	matS_sd  <- matrix(0,nr,nc_mat)
	# matS_freq <- matrix(0,nr,nc_mat)
	matS_q10 <- matrix(0,nr,nc_mat)
	matS_q50 <- matrix(0,nr,nc_mat)
	matS_q90 <- matrix(0,nr,nc_mat)


	for (j in seq(1,nc_mat,1))
	{


		for (i in seq(1,nr,1))
		{
			# debug
			# cat(sprintf("--------\n"))
			# cat(sprintf("j = %i\n", j))
			# cat(sprintf("i = %i\n", i))
			# cat(sprintf("--------\n\n"))
						

			# store elements in a vector; t-1, t-2, ... , t-5
			temp <- c( matHist[i,j+(0*nc_mat)], matHist[i,j+(1*nc_mat)], matHist[i,j+(2*nc_mat)], matHist[i,j+(3*nc_mat)], matHist[i,j+(4*nc_mat)] )
				
			# start building matrices
			matS_max[i,j]  <- max(temp)
			matS_min[i,j]  <- min(temp)
			matS_avg[i,j]  <- mean(temp)
			matS_sd[i,j]   <- sd(temp)
			# matS_freq[i,j] <- as.numeric(names(which.max(table(temp))))
			matS_q10[i,j]  <- quantile(temp, 0.1, na.rm=TRUE)
			matS_q50[i,j]  <- quantile(temp, 0.5, na.rm=TRUE)
			matS_q90[i,j]  <- quantile(temp, 0.9, na.rm=TRUE)

		}
	}

	# give names to new columns
	colnames(matS_max) <- paste(nam, "H.max", sep='.') 	
	colnames(matS_max) <- paste(nam, "H.max", sep='.') 
	colnames(matS_min) <- paste(nam, "H.min", sep='.') 
	colnames(matS_avg) <- paste(nam, "H.avg", sep='.') 
	colnames(matS_sd) <- paste(nam, "H.sd", sep='.') 
	# colnames(matS_freq) <- paste(nam, "H.freq", sep='.') 
	colnames(matS_q10) <- paste(nam, "H.q10", sep='.') 
	colnames(matS_q50) <- paste(nam, "H.q50", sep='.') 
	colnames(matS_q90) <- paste(nam, "H.q90", sep='.') 
	

	# merge all matrices into a single matrix
	# matS <- cbind(matS_max, matS_min, matS_avg, matS_sd, matS_freq, matS_q10, matS_q50, matS_q90)
	matS <- cbind(matS_max, matS_min, matS_avg, matS_sd, matS_q10, matS_q50, matS_q90)



	return(matS)
}
# -------------------------------------------------------------









# ---- create 1st-order derivative matrix ----
rawD1 <- function(mat) {

	# get number of rows and cols
	nr <- nrow(mat)
	nc <- ncol(mat)

	# duplicate original matrix
	matD1 <- matrix(0,nr,nc)

	for (j in seq(1,nc,1))
	{
		for (i in seq(1,nr,1))
		{
			if(i == 1)
			{
				# assume no derivative value at 1st row
				matD1[i,j] <- 0

			} else
			{
				
				# compute the 1st-order derivative
				matD1[i,j] <- mat[i,j] - mat[i-1,j]
							
			}
		}
	}

	colnames(matD1) <- paste(colnames(mat), "D1", sep='.') # give names to new columns

	return(matD1)
}
# -------------------------------------------------------------











# ------------create 2nd-order derivatives matrix ---------------
rawD2 <- function(mat) {

	# get number of rows and cols
	nr <- nrow(mat)
	nc <- ncol(mat)

	# duplicate original matrix
	matD2 <- matrix(0,nr,nc)

	for (j in seq(1,nc,1))
	{
		for (i in seq(1,nr,1))
		{
			if(i == 1 || i == nr)
			{
			
				# assume no derivative value at 1st or last row
				matD2[i,j] <- 0

			} else
			{
			
				# compute the 1st-order derivative
				matD2[i,j] <- mat[i+1,j] - 2*mat[i,j] + mat[i-1,j]
							
			}
		}
	}

	colnames(matD2) <- paste(colnames(mat), "D2", sep='.') # give names to new columns

	return(matD2)
}
# -------------------------------------------------------------














# ------------create historical matrix ---------------
rawH <- function(mat, xh) {

	# get number of rows and cols
	nr <- nrow(mat)
	nc <- ncol(mat)

	# total number of new columns for each raw vector
	nk <- 5

	# create new matrix
	mat1 <- matrix(0,nr,nc)
	mat2 <- matrix(0,nr,nc)
	mat3 <- matrix(0,nr,nc)
	mat4 <- matrix(0,nr,nc)
	mat5 <- matrix(0,nr,nc)
	matHist <- matrix(0,nr,nc*nk) # this matrix lumps all the others into a single matrix





	# ------------- building mat1 matrix ---------------------------
	nh <- 1
	for (j in seq(1,nc,1))
	{
		for (i in seq(1,nr,1))
		{
			if(i <= nh)
			{
				# assume the value at previous time to be 0
				mat1[i,j] <- 0
			} else 
			{

				# assign historical values
				mat1[i,j] <- mat[i-1,j]

			}

		}
	}
	colnames(mat1) <- paste(colnames(mat), "H1", sep='.') # give names to new columns
	# --------------------------------------------------------------






	# ------------- building mat2 matrix ---------------------------
	nh <- 2
	for (j in seq(1,nc,1))
	{
		for (i in seq(1,nr,1))
		{
			if(i <= nh)
			{
				# assume the value at previous time to be 0
				mat2[i,j] <- 0
			} else 
			{

				# assign historical values
				mat2[i,j] <- mat[i-nh,j]

			}

		}
	}
	colnames(mat2) <- paste(colnames(mat), "H2", sep='.') # give names to new columns
	# --------------------------------------------------------------





	# ------------- building mat3 matrix ---------------------------
	nh <- 3
	for (j in seq(1,nc,1))
	{
		for (i in seq(1,nr,1))
		{
			if(i <= nh)
			{
				# assume the value at previous time to be 0
				mat3[i,j] <- 0
			} else 
			{

				# assign historical values
				mat3[i,j] <- mat[i-nh,j]

			}

		}
	}
	colnames(mat3) <- paste(colnames(mat), "H3", sep='.') # give names to new columns
	# --------------------------------------------------------------





	# ------------- building mat4 matrix ---------------------------
	nh <- 4
	for (j in seq(1,nc,1))
	{
		for (i in seq(1,nr,1))
		{
			if(i <= nh)
			{
				# assume the value at previous time to be 0
				mat4[i,j] <- 0
			} else 
			{

				# assign historical values
				mat4[i,j] <- mat[i-nh,j]

			}

		}
	}
	colnames(mat4) <- paste(colnames(mat), "H4", sep='.') # give names to new columns
	# --------------------------------------------------------------





	# ------------- building mat5 matrix ---------------------------
	nh <- 5
	for (j in seq(1,nc,1))
	{
		for (i in seq(1,nr,1))
		{
			if(i <= nh)
			{
				# assume the value at previous time to be 0
				mat5[i,j] <- 0
			} else 
			{

				# assign historical values
				mat5[i,j] <- mat[i-nh,j]

			}

		}
	}
	colnames(mat5) <- paste(colnames(mat), "H5", sep='.') # give names to new columns
	# --------------------------------------------------------------



	# lump all matrices into a single matrix
	matHist <- cbind(mat1, mat2, mat3, mat4, mat5)



	return(matHist)
}
# -------------------------------------------------------------
