## S.G. April 2014 
## R programming assignment 2 (Coursera)

## Set of functions used to create a new matrix object 
## and to compute its inverse

## New matrix object holds (caches) its inverse and is created 
## using the makeCacheMatrix function
## Inverse of matrix is computed on new matrix object using the solve method 

## This function creates a new matrix object
## New matrix object caches its inverse
## Setters and Getters provided
makeCacheMatrix <- function(x = matrix()) 
{
    minv <- NULL
    
    set <- function(y)
    {
        x <<- y
        ## set minv to NULL when object is (re)set
        minv <<- NULL
    }
    
    get <- function()
    {
        ## return matrix
        x
    }
    
    setinv <- function(inv)
    {
        ## set inverse (this shouldn't be a public method...)
        minv <<- inv
    }
    
    getinv <- function()
    {
        ## get cached inverse
        minv
    }
    
    ## list of new matrix object methods (setters and getters)
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the new matrix object 
## if not previously computed or uses the cached value
cacheSolve <- function(x, ...) 
{
        m <- x$getinv()
        
        if(!is.null(m)) 
        {
            message("getting cached data")
            return(m)
        }
        
        data <- x$get()
        
        ## assume data (matrix) is square and invertible
        m <- solve(data, ...)
        
        x$setinv(m)
        
        m
}

## unit tests (commented out - with printed output)
## m <- makeCacheMatrix(x=matrix(c(4,2,7,6),nrow=2))
## m$get()
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## m$set(matrix(c(4,7,2,6),nrow=2))
## m$get()
## [,1] [,2]
## [1,]    4    2
## [2,]    7    6
## m$getinv()
## NULL
## mi <- cacheSolve(m)
## mi
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4
## cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4
