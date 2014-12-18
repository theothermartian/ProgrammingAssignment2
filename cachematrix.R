## 18th December.@Sayak Saha.

## This function creates a special "matrix" object that can cache its inverse.
## This object stores a matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	#Setting the value of the matrix
    set <- function(y) {
            x <<- y 			#Sets values of x globally
            inv <<- NULL
    }

    #Getting the value of the matrix
    get <- function(){
    	return(x)
    }

    #Setting the value of the inverse, thereby caching it
    setinv <- function(inverse){
    	inv <<- inverse
    }
    
    #returns the inverse to the calling function
    getinv <- function(){ 
    	return(inv)
	}


    #returns this object list which contains the getter and setter functions
    #and the cached values if any
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of the "matrix" object returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
       
        inv <- x$getinv()

        #if inverse is already cached
        if(!is.null(inv)) 
        {
            return(inv)
        }

        #gets the value of the matrix from the special matrix object created in the above function
        mat <- x$get()

        #calculating ghe inverse...assuming the matrix is invertible
        inv <- solve(mat)

        #setting the value of inverse in the matrix object
        x$setinv(inv)


        #Finally returning thr calculated or cached inverse
        return(inv)
}
