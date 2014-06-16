# this function (MakeCacheMatrix) sets the value of the matrix, 
# gets the value of the matrix
# sets the value of the inverse, and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        Inv <- function(solve) m <<- solve
	  getinverse <- function() m	
        list( set = set, get = get, Inv = Inv, getinverse = getinverse)

}


#cacheSolve calculates the mean of the special "matrix" created in the
#function. First it checks whether it has already been calculated.
#If so, it gets the mean from the cache and skips the computation.
#Otherwise it calculates the inverse of the data and sets the inverse value
#in the cache via the set function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$Inv(m)
        m

}
