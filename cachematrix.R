# makeCacheMatrix creates a list containing a function to 
# sets the matrix with y 
# returns the matrix 
# set the inverse of the matrix with inverse 
# returns the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

invMatrix <- NULL 
     set <- function(y) { 
           x <<- y 
	   invMatrix <<- NULL 
          } 
     get <- function() x 
     setinverse <- function(inverse) invMatrix <<- inverse 
     getinverse <- function() invMatrix 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}

# The following function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, returns the cached matrix 
# If it is not cached, it computes the inverse, sets the value in the cache via 
# setinverse function and returns the cached matrix. 

cacheSolve <- function(x, ...) { 
# Check if the inverse is already cached.. 
invMatrix <- x$getinverse() 
       if(!is.null(invMatrix)) { 
                print("Get the cache matrix.") 
		return(invMatrix) 
	} 
 # Get the matrix and call Inverse function on the matrix
	mat <- x$get() 
	invMatrix <- solve(mat) 
#Set the inverse of matrix
	x$setinverse(invMatrix) 
# Return Inverse of matrix
	invMatrix
} 
