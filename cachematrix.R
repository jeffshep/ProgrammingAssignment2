##This function returns a list containing 4 functions:
        ##set, get, setinv, getinv
## The operator '<<-' assigns a value to an object in an environment 
## different from the current.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function (inverse) inv <<- inverse
        getinv <- function () inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Computes a matrix this is the inverse of the input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        #if the inverse has been previously calculated
        if (!is.null(inv)){
                #if the inversion result is there
                message("getting cached data")
                #return the calulated inversion
                return (inv)
        }
        #if not, calculate the inverse
        a.data <- x$get()
        inv <- solve(a.data, ...)
        #set to the object
        x$setinv(inv)
        #return result
        inv
}
