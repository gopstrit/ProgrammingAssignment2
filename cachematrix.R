## CourseEra Assignment 2
## These functions caches the Inverse of a Matrix
## Steps are mentioned in the comments

## This function creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        #steps in comments
        #The user will input x, which is a square matrix
        
        #The main function first creates an empty object m (if need be)
        m <- NULL 
        
        
        #defining set function
        #setting the values of x [as being input in the main function]
        #also clearing the slate for the stored value in m, if so required
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #defining get function
        # get function gets the value of matrix x fed into the makeCacheMatrix (by the user)
        get <- function() x
        
        #defining setinverse function
        #this function defines that the inverse calculated in cacheSolve has to be stored in m
        setinverse <- function(solve) m <<- solve
        
        #defining getinverse function
        #this function returns the inverse stored in m
        #i.e. if this function is called, the value stored in m is returned
        getinverse <- function() m
        
        #defining the list of above functions to be stored in the main function
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
                        
}       



## This function computes the inverse of the special matrix returned  
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #steps in comments
        #first, calling for the already stored value (if any) in m
        m <- x$getinverse()
        
        #now we check if m is empty
        #if it is not empty, it will return the stored value
        #else, it will compute the inverse, store it in m, and then will return the value
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        #feeding the vector of matrix as defined in makeCacheMatrix (by the user)
        data <- x$get()
        
        #calculating the inverse of data and storing it in m
        m <- solve(data, ...)
        
        #passing the computed inverse to the setinverse function in makeCacheMatrix function
        #the value of computed invese will thus be stored there in m
        x$setinverse(m)
        #next time, the stored value of m can be simply returned without having to compute it again
        
        #returning the inverse of the matrix in m
        m
}
