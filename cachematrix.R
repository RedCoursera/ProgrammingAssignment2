## "Matrix inversion is usually a costly computation, and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly."  (Information in quotes is from the Programming Assignment 2 instructions.)
## The functions below provide a way to do this.  Here's an example of the usage of these functions:
#define a matrix
    #myMatrix <- matrix(1:4,2,2)
#create an instance of the makeCacheMatrix helper function, which can store the calculated inverse of the matrix
#passed to the function
    #solveMatrix <- makeCacheMatrix(myMatrix)
#calculate and cache the inverse of the matrix in solveMatrix
    #cacheSolve(solveMatrix)
#on the second run, you will see "getting cached data"; the matrix inverse is retrieved and not recalculated
    #cacheSolve(solveMatrix)
#to change the matrix to be inverted
    #solveMatrix$set(matrix(5:8,2,2))
#to calculate the new matrix inverse; will *not* see "getting cached data"
    #cacheSolve(solveMatrix)

	
## "This function creates a special "matrix" object that can cache its inverse."
makeCacheMatrix <- function(x = matrix()) {

    #Variable to hold the inverse of a matrix
    m <- NULL
    
    #To store the matrix on which the matrix inversion will be calculated
    set <- function(y) {
        #<<- is the deep assignment arrow to create/modify a variable in the parent environment,
		#which in this case is a makeCacheMatrix function instance
        x <<- y
        #Clear the variable storing the matrix inverse as it will need to be recalculated
        m <<- NULL
    }
    
    #To pass the matrix on which the inverse will be calculated
    get <- function() x
    
    #To store the calculated matrix inverse in the parent environment (makeCacheMatrix function instance)
    setSolve <- function(solved) m <<- solved
    
    #To pass the calculated inverse of the matrix
    getSolve <- function() m
    
    #List of the sub-functions; values can be obtained via $ notation
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
		 
}


## "This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache."
cacheSolve <- function(x, ...) {

    m <- x$getSolve()
    
    #Determine if inverse has already been calculated
    if(!is.null(m)) {
        message("getting cached data")
        #Exit out of function, returning the already calculated inverse
        return(m)
    }
    #Inverse has not already been calculated; obtain it via sub-functions in makeCacheMatrix function instance (x)
    
    #Get the matrix on which to calculate the inverse
    data <- x$get()
    
    #Calculate the inverse of the matrix
    m <- solve(data, ...)
    
    #Return the now calculated matrix inverse to the instance of makeCacheMatrix so it can be retrieved later if needed
    x$setSolve(m)
    
    #Return the now calculated matrix inverse to the user interface
    m
	
}
