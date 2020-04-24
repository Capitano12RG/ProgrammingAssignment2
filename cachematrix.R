##The first function, makeVector creates a special "vector", which is really a list containing a function to
#	1.	set the value of the matrix
#	2.	get the value of the matrix
# 3.   set the value of the matrix
#4.    get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                #print("y is as follows:",y)
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
             )
#print(x)
}

#The following function calculates the mean of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        #print(m)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
      
}


#source("Inversematrix assignment_rajat.R")
#> cacheSolve(makeCacheMatrix(matrix(c(4,2,7,6),2,2)))