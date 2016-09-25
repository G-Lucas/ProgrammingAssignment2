## The objective of makeCacheMatrix and cacheSolve functions is to
## manage the inverse of a matrix calculation in a persistent way.

## makeCacheMatrix is a function that makes possible to manage
## following operations on a matrix:
## - store a matrix
## - store the calculation of the inverse of the matrix
## - get the matrix
## - get the result of the inverse calculation of the matrix


makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(imat) im <<- imat
        getInverseMatrix <- function() im
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## cachesolve function calculates the inverse of a matrix.
## It uses the object returned by "makeCacheMatrix" function,
## and for this reason it can check if the calculation of
## the inverse of the matrix has been already done. In this
## last case, the value is returned without calculations,
## otherwise, the "cachesolve" function will compute the 
## calculation.
## The function doesn't check if the inverse of the matrix
## can be done (it suppose that the calculation is feasible).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## step 0: check if the calculation of the inverse
        ## matrix is already available
	if(!is.null(x$getInverseMatrix())) {

                message("getting cached data")

                return(x$getInverseMatrix())

	}
        dimMatrix <- nrow(x$get())
        detX <- det(x$get())
        invM <- matrix(nrow=nrow(x$get()),ncol=ncol(x$get()))
        cofM <- matrix(nrow=nrow(x$get()),ncol=ncol(x$get()))
 
        ## check if matrix is 2x2 - in this case it is not 
        ## necessary to calculate the determinant of each
        ## sub-matrix.

        if ( dimMatrix == 2 ) {
        ## step 1: calculate the cofactor matrix

          invI <- 2
          for (i in 1:2) {
             invJ <- 2
             for (j in 1:2){
	        cofM[i,j] <- x$get()[invI,invJ] * (-1)^(i+j)
      		invJ <- invJ - 1
             }
             invI <- invI -1
          }
        ## step 2: calculate the inverse matrix of the
        ## cofactor matrix
          for (i in 1:2) {
             for (j in 1:2){
		invM[i,j] <- cofM[j,i]
             }
          } 
	## step 3: multiplication of the inverse matrix with
        ## the determinant of the source matrix
	   invM <- invM * (1/detX)
           x$setInverseMatrix(invM)
        } 
        else {
	## In this case, the matrix is > than 2x2

          for (i in 1:dimMatrix) {
             for (j in 1:dimMatrix){
		## calculation of the sub-matrix without 
		## a row and a column, to calculate determinant
		subM <- x$get()[-i,]
		subM <- subM[,-j]
	        cofM[i,j] <- det(subM) * (-1)^(i+j)
             }
          }
        ## step 2: calculate the inverse matrix of the
        ## cofactor matrix
          for (i in 1:dimMatrix) {
             for (j in 1:dimMatrix){
		invM[i,j] <- cofM[j,i]
             }
          } 
	## step 3: multiplication of the inverse matrix with
        ## the determinant of the source matrix
	   invM <- invM * (1/detX)
           x$setInverseMatrix(invM)
        }
	invM
}
