## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
        
        ## Initializeing the inverse property veriable.
        i <- NULL
        
        ## Setting the function or Method to set the matrix.
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Declaring the method the get the matrix results.
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## Declaring a funtion or method to set the inverse of the matrix.
        setInverse <- function(inverse) {
                i <<- inverse       
        }
        
        ## Getting all the inversed element from method in inverse of the matrix.
        getInverse <- function() {
                ## Return all the inverse property values.
                i
        }
        
        ## Returning a list of the methods from makeCacheMatrix funtions.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Returning a matrix that is the inverse of 'x' matrix.
        m <- x$getInverse()
        
        ## Just return the inverse if its already in the set.
        if(!is.null(m)) {
                message("||-->> Getting all the cached data from cached Matrix.")
                return(m)
        }
        
        ## Get the matrix from existing Object.
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication by 'data' using solve() functions
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
}
