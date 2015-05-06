#command example:
#source ('everything')
mat <- read.csv('../data/test_grnn.csv')
coeff <- normalize.getCoeff(mat)
mat
mat <- data.matrix(mat)
#print nice data (all characters coersed to factors)
mat
net <- create.Rgrnn(mat, outColumn = 3)
net
