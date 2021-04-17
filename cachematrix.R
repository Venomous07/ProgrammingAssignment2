## Calculate whether function NULL
## Get Answer Matrix

## Function Vector
makeCacheMatrix <- function(P = matrix()) {
  R <- NULL                              #null will 'R'
    set <- function(x) {
    P <<- x                                #matrix will 'P'
    R <<- NULL
  }
  get <- function()P                       #get matrix 'P' function
  setpasar <- function(pasar) R<-pasar
  getpasar <- function(pasar) R
  list(set = set, get = get,               #list function perform
       setpasar = setpasar, 
       getpasar = getpasar)
}

## Get Data Set Value Answer
## mean transform go(Spanish = pasar)
## data transform notitia(Latin = data)

cachepasar <- function(P, ...)
{
  R<-P$getpasar()
  if(!is.null(P)){                          #cause function check 'P', NULL
    message("ut rectam responsum")          #end message (Latin = Get Correct Answer)
    return(R)                               #cause return value 'R'
  }
  notitia<- P$get()
  R<-P$setpasar(notitia, ...)               #function help calculate value 'R'
  P$setpasar(R)
  R                                         ##function 'R' return matrix

}
P<-makeCacheMatrix(matrix(1:6,3,2))         #matrix maker
P$get()                                     #matrix visual
P$getpasar(R)                               #answer matrix, NULL
cachepasar(P)                               #answer(NULL), end message
