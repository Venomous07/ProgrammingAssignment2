## R Programming
## Computing the Square of a Matrix

## It is the Special Vector's Function.
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                      #The null will be the inverse that will be initialize
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function()x            #This is where you can get the matrix x function
	setinv <- function(inverse) {inv <<- inverse}
	getinv <- function() {
		inver<-ginv(x)
		inver%*%x                   #This will help the inverse of the matrix to obtain
	}               				
	list(set = set, get = get, 
		setinv = setinv, 
		getinv = getinv)
}

## It calculates the mean of the set of data values.

cacheSolve <- function(x, ...)   ##this function will help to get the data that is cached.
	{
	inv <- x$getinv()
	if(!is.null(inv)){             #This will cause the function to check the inverse if it is NULL
		message("getting cached data")
		return(inv)                    #This will cause to that the returns the value of the inverse
	}
	data<- x$get()
	inv<- solve(data, ...)              #This function helps to calculate the value of the inverse
	x$setinv(inv)
	inv                                ## In this function of inverse of 'x' will return because it is a matrix
}
