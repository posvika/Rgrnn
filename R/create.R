#########################################
# Rgrnn create func 					#
# creates grnn, updates set or sigmas	#
# by Liora Pospelova					#
# posvika@gmail.com						#
#########################################

create.Rgrnn <- function(set, sigma, outColumn = 1, changeColNames = F)
# @arguments:
#set 		data.frame or matrix
#sigma 		numeric vector (optional)
#outColumn 	scalar numeric
{
	if(missing(set)) stop("Set is missing!")
	if (missing (sigma)) sigma = rep(0.2, ncol(set)-1)
	if (length(sigma) != ncol(set)-1) stop("sigma length doesn't match input amount")

	grnn <- list(
		model	= "General regression neural network (multidimentional)",
		set 	= set,
		sigma 	= sigma,
		outColumn = outColumn
		)
	if (changeColNames ) colnames(grnn$set)[outColumn] <- "out"
	grnn
}