## Put comments here that give an overall description of what your
## functions do
############################
## This funciton is designed for Coursera R Programming course
## week 3 assignment, which is for calculating the inverse of 
## a matrix and make cache spaces of it.

## Write a short comment describing this function
############################
## This function makeCahceMatrix is for making cache spaces for
## the inverse of the matrix which the argument x gives.
## I designed for functions in makeCacheMatrix. Set function will
## set the matrix x gives to x. Get function will get the data x.
## Setinverse function will set the inverse of the matrix to 
## inverse_matrix. And getinverse funciton will return the 
## variable inverse_matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(sinverse) inverse_matrix <<- sinverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
############################
## This function cacheSolve is for returning the inverse of a 
## matrix. First of all we use getinverse function in x(using
## function makeCacheMatrix) to m. Then if m is not NULL then
## return the message and the value of m. Otherwise the value of 
## m is NULL, meaning that the inverse of the matrix is not in
## the cache before. so we get the matrix to data, then using 
## solve function which can calculate the inverse of a matrix.
## After that, using setinverse function to save the result m to
## the cache. Finally, print the inverse of the matrix m.
############################
## For example, we can use the following command to check:
## Typing command: x<-matrix(c(3,5,1,-2,-4,-1,2,1,0),3,3)
## Typing command: y<-matrix(c(-1,-1,1,2,2,-1,-6,-7,2),3,3)
## Then typing command: cacheSolve(makeCacheMatrix(x))
## before adding this R source to the Console.
## You will see the console print out a matrix equals to y.
## Thank you very much!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
