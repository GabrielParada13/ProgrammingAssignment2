## Two functions that cache de inverse of a matrix

## Creates a special matrix object that cache inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      
        ## Initializes de inverse matrix
        i <- NULL
    
        ## Method to set the matrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() {
            ## Return the matrix
            m
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
            i <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function(){
            ## Return the inverse
            i
        }
        
        ## Return a list of the methods
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getInverse()
    
        ## Just return the inverse if its already set
        if( !is.null(m)){
                message("getting cached data")
                return(m)
        }
    
        ## Get the matrix from the object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Calculate the inverse of the object
        x$setInverse(m)
        
        ## Return the matrix
        m
}
