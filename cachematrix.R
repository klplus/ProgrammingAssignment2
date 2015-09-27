## The two functions in this file are created to save memory and reduce 
## computation time, the fuction did the inversion of matrices


## This function saves the original matrix (x) and the other is supposed to return
## NULL or the inverse of x that is invr in my case. The function is a matrix that 
## takes a numeric arguement
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinvr <- function(solvedinv) inverse <<- solvedinv
        getinvr <- function() inverse
        list(set = set, get = get,
             setinvr = setinvr,
             getinvr = getinvr)

}


## The function below takes a variable as an arguement of same type and return
## the value of makecachematrix. It returns the inverseof the matrices recorded 
##in the matrix catche, it serves as the first check if there is an inverse that
##has been computed, computes the inverse of the matrix cache($get()), stores the inverse
##cache maybe use it at a later time

cacheSolve <- function(x, ...) {
        inverse <- x$getinvr()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinvr(inverse)
        inverse
}
