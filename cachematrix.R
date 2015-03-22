## Put comments here that give an overall description of what your
## functions do
##
## This file has two funcions, function 1 to create a matrix with caching and 
## the second function calculates the matrix inverse, if not already calculated

## Write a short comment describing this function
## ----------------------------------------------
## 
## The first function, makeVector creates a special "Matrix", which is really a 
## list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matrix inverse (if already cached)
makeCacheMatrix <- function(x = matrix()) 
    {
        ## Initialize Inverse to NULL, as we have not done the calculations yet
        Inverse <- NULL
        
        ## This function allows us to set the value for the matrix passed from 
        ## the calling/parent environment. 
        ## Example:
        ## step 1. create a marix in RStudio command window
        ## > m <- rbind(c(0.8147,0.9134,0.2785), c(0.9058,0.6324,0.5469),
        ##              + c(0.1270,0.0975,0.9575))
        ## 
        ## m is stored as a matrix in the parent workspace
        ##
        ## step 2. create a matrix list using makeCacheMatrix function
        ## > mi <- makeCacheMatrix();
        ##
        ## step 3. Assign the matrix to the empty matrix list created in step 2
        ##         "mi"
        ## > mi$set(m)
        ## in set function "x <<- y" will take the value from the parent/calling
        ## function and populates the special matrix
        set <- function(y) 
        {
            x <<- y
            Inverse <<- NULL
        }
        
        ## This function allows us to get the matrix that we have passed in 
        ## earlier, either as input to "makeCacheMatrix" or using "set" function
        get <- function() x
        
        ## This function is returing another function as output to the calling
        ## environment, in this case we are passing "solve" function to 
        ## compute the inverse of a matrix. The results are stored in the 
        ## caller workspace 
        setInverse <- function(solve) Inverse <<- solve
        
        ## function to retrieve the calculated inverse
        getInverse <- function() Inverse
        
        ## This will return the special matrix which is actually a list. 
        ## Constructing this list was the motivation for this function.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

    }


## Write a short comment describing this function
## ----------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) 
    {
        ## Return a matrix that is the inverse of 'x', x is the list construced 
        ## using makeCacheMatrix function.
        ##
        ## Query for the calculated inverse stred in the special matrix created 
        ## by makeCacheMatrix function. 
        Inverse <- x$getInverse()
        
        ## If the value returned is not null, then retrieve the cached result 
        ## and return it as the result to the calling function/ workspace/ 
        ## environment or parent
        if(!is.null(Inverse)) 
        {
            message("getting cached data")
            return(Inverse)
        }
        
        ## If the inverse is not calculated, retrieve the matrix from the list  
        ## constructed
        data <- x$get()
        
        ## Calculate the inverse and store it in the list. This result is now
        ## cached for future reference. Return the computed inverse.
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
    }
