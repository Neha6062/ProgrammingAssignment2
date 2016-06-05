## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        #`<<-` to assign a value to an object in an environment 
        # different from the current environment.
        set <-function(y){
            x <<- y
            i <<-NULL
                         }
        get <- function()x
        setinverse <- function(inverse) i<<- inverse
        getinverse <- function()i
        list( set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## return: inverse of the original matrix input to makeCacheMatrix()
        m <- x$getinverse()
        # if the inverse has already been calculated
        if(!is.null(m)){
          # get it from the cache and skips the computation.
            message("getting cached data")
            return(m)
        }
        # otherwise, calculates the inverse 
        data <- x$get()
        m<- solve(data, ...)
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(m)
        m
}
