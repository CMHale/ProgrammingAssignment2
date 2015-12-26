## A function that creates a matrix object that can cache its inverse. It returns a list containing
## functions that are used as an input for the cacheSolve() function below.
## The <<- operator is used to assign a value to an object in an environment that is different from
## the current environment.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse){ 
                m <<- inverse
        }
        getinverse <- function(){
                m
        }
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the above matrix object (m). If the inverse has already been calculated,
## then it retrieves it from the cache. If the input is new, it calculates the inverse of the data and
## sets the inverse in the cache via the setinverse function. 
## Running the function again will then return the inverse.


cacheSolve <- function (x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                        message ("getting cached data")
                        return(m)
                }
                matrix <- x$get()
                m <- solve(matrix, ...) 
                x$setinverse(m)
                return(m)
        }