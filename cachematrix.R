## There are two functions in this file
## 1. makeCacheMatrix 
## 2. cacheSolve
## 
## makeCacheMatrix function has four functions - set, get, setinverse and
## getinverse. 'set' sets the matrix to the input matrix, 'get' retrieves
## the matrix, 'setinverse' sets the inverse matrix and 'getinverse'
## retrieves it. The setter functions use '<<-' assignment operator
## to assign the values to the parent frame.
## Hence, when the function makeCacheMatrix is passed to cacheSolve and 
## the setinverse function is called from cacheSolve, it actually calculates
## the inverse and assigns it to the 'inversematrix' variable in the parent
## frame ie., cacheSolve
##
##
## cacheSolve function checks for a cached version of inverse matrix
## If there is no cached version, it solves the inverse of the input matrix,
## sets it and returns the value

## makeCacheMatrix has setter and getter functions for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        ## Assign a default matrix as inversematrix
        inversematrix <- matrix()
        
        
        set <- function(y)
        {
                x <<- y
                inversematrix <<- matrix()
        }
        
        get <- function()
        {
                x
        }
        
        setinverse <- function(x)
        {
                inversematrix <<- x
        }
        
        getinverse <- function()
        {
                inversematrix
        }
        
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve returns the inverse matrix by either solving it or
## retrieving it from the cache

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        defaultmatrix <- matrix()
        if(!identical(defaultmatrix, inversematrix))
        {
                message("getting cached data")
                return(inversematrix)
        }
        
        ## There is no inverse matrix in the cache. Hence, use 'solve'
        ## function to find the inverse
        matrix <- x$get()
        inversematrix <- solve(matrix, ...)
        x$setinverse(inversematrix)
        inversematrix
}
