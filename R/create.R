#########################################
# Rgrnn create func 					#
# creates grnn, updates set or sigmas	#
# by Liora Pospelova					#
# posvika@gmail.com						#
#########################################

create.Rgrnn <- function(set, sigma, outColumn = 1)
# @arguments:
#set 		data.frame or matrix
#sigma 		numeric vector (optional)
#outColumn 	scalar numeric
{
	if (missing(set)) 		stop("Set is missing!")
	if (!is.matrix(set)) 	print("All non-numeric data in set will be automaticly coercied")
	set = data.matrix(set)
	if (missing (sigma)) 	sigma = rep(0.2, ncol(set)-1)
	if (length(sigma) != ncol(set)-1) 
							stop("sigma length doesn't match input amount")

	grnn <- list(
		model	= "General regression neural network (multidimentional)",
		sigma 	= sigma,
		Xa = set[ ,-outColumn]
		Ya = set[ , outColumn]
		#aka Xa from grnn library
		layer1weights = numeric(), 	#dim=dim(set)-1
		#aka Ya from grnn library
		layer2weights = numeric() 	#dim=dim(set)-1
		)
	#grnn$sigma <- sigma
		#setting start weights as 1
	grnn$layer1weights <- rep(1,ncol(grnn$Xa))
	grnn$layer2weights <- rep(1,ncol(grnn$Xa))
	grnn
}