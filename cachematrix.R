## Gives correct answer for matrix
## Computing Square Matrix

## Special Vector Function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                      #null will inverse be initialize
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x            #where can get matrix x function
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {
    inver<-ginv(x)
    inver%*%x                   #will help inverse of matrix be obtain
  }               				
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Calculate mean set data value

cacheSolve <- function(x, ...) 
{
	inv <- x$getinv()
  if(!is.null(inv)){             #will cause function check inverse if NULL
    message("I get the correct Answer")
    return(inv)                    #will cause to that the returns value inverse
  }
  data<- x$get()
  inv<- solve(data, ...)              #function helps calculate value inverse
  x$setinv(inv)
  inv                    ##function of inverse of 'x' will return because matrix
       
}
