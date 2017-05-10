## Put comments here that give an overall description of what your
## functions do

## Submission for Charlie Ellery
        ## Given the calculation of an inverse for a matrix can be an involved exercise, the functions below
        ## cache the inverse of the matrix x within the parent environment of the function in order to allow 
        ## it to be recalled from the parent environment when needed in any subsequent calls. This avoids the 
        ## need to recalculate the cache for this particular matrix every time.
        ## It is made up of two functions, makeCacheMatrix and cacheSolve, that must be used together.

## Write a short comment describing this function
        ## The 'makeCacheMatrix' function creates a list of functions (operations) to carry out on the argument
        ## matrix; in the context of the above overview, it either sets a value in the parent environment, or it
        ## obtains a value from the parent environment (if that particular one has already been cached in the parent 
        ## environment). 'makeCacheMatrix' establishes the parent environment.

makeCacheMatrix <- function(x = matrix()) {             
        inv <- NULL                                     ## 'inv' is the inverse matrix that has been cached. It is NULL to ensure it is empty when the function starts to run.
        set <- function(y) {                            ## 'set' function will set the argument matrix to the parent environment and call it x, and at the same time clear the cache in parent environment for 'inv'
                x <<- y
                inv <<- NULL
        }
        get <- function() x                             ## 'get' function will obtain the x argument (in this case matrix) from parent environment, defined as argument to 'makeCacheMatrix'
        setinv <- function(inverse) inv <<- inverse     ## 'setinv' function, if run, will set the value of the inverse matrix in cache. The inverse is not calculated here, but in the 'cacheSolve' function
        getinv <- function() inv                        ## 'getinv' function, if run, will call the value of the inverse matrix (inv) which has been cached in parent environment
        list(set = set, get = get,                      ## 'list' specifies the output of 'makeCacheMatrix' - a list of the above functions. It specifies the name of each so that it can be called by name later.
                      setinv = setinv,
                      getinv = getinv)
}


## Write a short comment describing this function
        ## 'cacheSolve' takes input and checks if the inverse of that matrix has been saved to  cache
        ## already (in the parent environment). If it has, it will return the value from cache as the 
        ## inverse, without calculating it via the 'getinv' function in 'makeCacheMatrix'. If there is
        ## no inverse cached, it will calculate it by calling the original matrix through the 'get' function
        ## above and through R function 'solve.' If it does calculate it, it will then call the 'setinv' function 
        ## to save that inverse matrix to cache in the parent environment.
        ##The argument for the 'cacheSolve' function must be the output from the function above, 'makeCacheMatrix'
        ## in order to be able to successfully run and call the functions that allow it to access the parent environment.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                               ## 'inv' is set to be the 'getinv' function from 'makeCacheMatrix' output
        if(!is.null(inv)) {                             ## if statement - 'if value of inv (getinv function) is not empty in parent environment, retrieve it and write 'getting cached data'
                message("Getting cached data")
                return(inv)
        }
        input <- x$get()                                ## setting the value of input as 'get' function from 'makeCacheMatrix.' 'input' will be used subsequently.
        inv <- solve(input)                             ## using R solve function to obtain inverse of original matrix, and assigning it to 'inv'
        x$setinv(inv)                                   ## Using 'setinv' function from 'makeCacheMatrix'the value of 'inv' is set within parent environment
        inv                                             ## value of inv (inverse to original matrix) is returned
}





#Ignore below
# Final validation
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
myMatrix_Object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_Object)
cacheSolve(myMatrix_Object)
m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_Object$set(m2)
cacheSolve(myMatrix_Object)


# Query area
?list
?matrix
?solve
a <- matrix(1:4, 2, 2)
a
solve(a)
## Matrices are multiplied if you multiply everything in the nth row of the first matrix by everything in the nth column of the second matrix, if you are calculating a certain 