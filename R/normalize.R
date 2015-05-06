normalize.getCoeff <- function (matrix)
#multiply matrix to this coeff to normalize it
{
	absMinumum = min (abs (as.matrix(matrix)))
	absMaximum = max (abs (as.matrix(matrix)))
	return (1/(absMaximum)) # - absMinumum))
}

normalize <- function(matrix)
#expects matrix or data.frame as input
#returns normalized matrix
{
	coeff <- normalize.getCoeff(matrix)
	return (matrix * coeff)
}

