#########################################
# Rgrnn learn func	 					#
# seaching for sigma vector 			#
# by enumeration						#
# by Liora Pospelova					#
# posvika@gmail.com						#
#########################################

learn.Rgrnn <- function(grnn, loopAmount=1000, R2 = 0.5, sigmaSet)
# @arguments:
#grnn			grnn returned by Rgrnn library functions
#set 			(optinal) data.frame or matrix, additional learn data
#loopAmount		amount of learn cycles to stop
#R2				(optional) estimated R2 to stop  
#sigmaSet		(optional) vector of values to choose sigma[i] from 
{
	if(loopAmount<=0) 	{print("loopAmount shold be >0, set loopAmount = 1000"); 
						loopAmount = 1000}
	if(missing(sigmaSet)) sigmaSet = seq (-100,100, 0.5)
	r2 <- numeric		#temp vector (see line 25 and below)
	for (testnum in 1:nrow(grnn$Xa))
	{
		for (i in 1:loopAmount)
		{
			for (sigmaCurr in sigmaSet)
			{
				grnn$sigma[i] <- sigmaCurr
				y <-compute(grnn$layer1weights,grnn$layer2weights,grnn$Xa[testnum],grnn$sigma)
				r2 <- c(r2, sum( (y-grnn$Ya[testnum])^2 ))
				result <- c(grnn$sigma[i], y)
			}
		}
	}
}


#########################################
# Rgrnn compute func	 				#
# compute output from 1 data row 		#
# with given sigma vector				#
# by Liora Pospelova					#
# posvika@gmail.com						#
#########################################
compute.Rgrnn <- function(Xa, Ya, X, sigma)
# @arguments:
#X				input data, new instance
#Xa				=layer1weights (input matrix)
#Ya				=layer2weights (targets)
#sigma 			sigma vector for hidden layer
{
	hidden <- numeric()
	#get hidden layer results
	for(i in 1:length(X))		#loop over 1 layer neurons
	{
		#hidden - vector of hidden neurons kernel function Y()  	computation results
		hidden <- c(hidden,Y(Xa([i,] Ya[i], X, sigma[i]))
		result <- sum(hidden)
		return(result)
	}
}