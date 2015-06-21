## Put comments here that give an overall description of what your functions do:


##--------------------------------------------------------------------------------------------

## Write a short comment describing this function:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates and returns a list of functions used by cacheSolve to get or set 
## the inverted matrix in cache 

makeCacheMatrix <- function(x = matrix()) 
{    
    cache <- NULL                                    ## allocate memory to cache; initialize to NULL    
    
    set <- function(y)                               ## create the matrix in the working environment
    {
        x <<- y
        cache <<- NULL
    }   
    
    get <- function() x                              ## get the value of the matrix
    
    setMatrix <- function(inverse) cache <<- inverse ## invert the matrix and store in cache
    
    getInverse <- function() cache                   ## get the inverted matrix from cache
    
    ## return the created functions to the working environment
    list(set = set, get = get,
         setMatrix = setMatrix,
         getInverse = getInverse)
}


## Write a short comment describing this function:
## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix If the 
## inverted matrix does not exist in cache, it it created in the working environment 
## and it's inverted value is stored in cache

cacheSolve <- function(x, ...) 
{    
    cache <- x$getInverse()         ## attempt to get the inverse of the matrix stored in cache   
    
    if (!is.null(cache))            ## return inverted matrix from cache if it exists
    {                               ## if it is not null then data exists
        message("cached data exists; getting cached data!")  ## let user know it is getting data                
        return(cache)               ## display matrix data 
    }
    else                            ## else it is null so create the matrix in the working environment
    {
        message("cached data does NOT exists; caching data now!")    ## let user know it was loaded
        matrix <- x$get()           ## load matrix into memory; it does not currently exist         
    }                                    
    
    ## make sure matrix is square and invertible; refresh your linear algebra for this!!!
    ## if not, handle error and warning exceptions cleanly
    tryCatch( 
    {
        cache <- solve(matrix, ...) ## try to set and return inverse of matrix
    },
    
    error = function(e) 
    {
        message("Error message:")
        message(e)                  ## return "Error message:" then the error message represented by 'e'        
        return(NA) 
    },
    
    warning = function(w) 
    {
        message("Warning message:")
        message(w)                       ## return "Warning message:" then the warning message represented by 'w'        
        return(NA)
    },
    
    finally =                            ## no warnings or errors so SUCCESS!!    
    {                                 
        x$setMatrix(cache)               ## set inverted matrix in cache
    } 
    )                                    ## End tryCatch block
    
    return (cache)                       ## display matrix in console
}

## -------------------------------- TESTING ---------------------------------------
## --------------------------------- TEST 1 ---------------------------------------
## > source("cachematrix.R")             ## load R program
## > matrixb <- makeCacheMatrix()        ## create functions
## > matrixb$set(matrix(c(1,2,4,0,3,1,4,4,0), 3, 3))      ## create matrix in working environment
## > cacheSolve(matrixb)                 ## 1st run returns inverted matrix from working environment
##    
## cached data does NOT exists; caching data now!
##             [,1]        [,2]        [,3]
## [1,]  0.09090909 -0.09090909  0.27272727
## [2,] -0.36363636  0.36363636 -0.09090909
## [3,]  0.22727273  0.02272727 -0.06818182
##
## > cacheSolve(matrixb)                 ## 2nd and subsequent runs returns inverted matrix from cache

## cached data exists; getting cached data
##             [,1]        [,2]        [,3]
## [1,]  0.09090909 -0.09090909  0.27272727
## [2,] -0.36363636  0.36363636 -0.09090909
## [3,]  0.22727273  0.02272727 -0.06818182


## --------------------------------- TEST 2 ---------------------------------------
## > source("cachematrix.R")            ## load R program
## > c <- makeCacheMatrix()             ## create functions
## > c$set(matrix(c(0,2,-1,-2), 2, 2))  ## create matrix in working environment
## > cacheSolve(c)                      ## 1st run returns inverted matrix

## cached data does NOT exists; caching data now!
##      [,1] [,2]
## [1,]   -1  0.5
## [2,]   -1  0.0

## cacheSolve(c)                        ## 2nd and subsequent runs returns inverted matrix from cache
## cached data exists; getting cached data
##      [,1] [,2]
## [1,]   -1  0.5
## [2,]   -1  0.0

## --------------------------------- TEST 3 ---------------------------------------
## > source("cachematrix.R")           ## load R program
## > a <- makeCacheMatrix()            ## create functions
## > a$set(matrix(1:4, 2, 2))          ## create matrix in working environment
## > cacheSolve(a)                     ## 1st run returns inverted matrix
## 
## cached data does NOT exists; caching data now!
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)                     ## 2nd and subsequent runs returns inverted matrix from cache
##
## cached data exists; getting cached data         
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

##---------------------------------------------------------------------------------------------------
# Assignment: Caching the Inverse of a Matrix

#Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). Your assignment is to write a pair of functions that 
# cache the inverse of a matrix.

# Write the following functions:

# 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2) cacheSolve: This function computes the inverse of the special "matrix" returned by 
#    makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#    then the cachesolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. For example, 
# if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.

# In order to complete this assignment, you must do the following:

# 1) Fork the GitHub repository containing the stub R files at https://github.com/rdpeng/ProgrammingAssignment2 
#    to create a copy under your own account.
# 2) Clone your forked GitHub repository to your computer so that you can edit the files locally on your own machine.
# 3) Edit the R file contained in the git repository and place your solution in that file (please do not rename the file).
# 4) Commit your completed R file into YOUR git repository and push your git branch to the GitHub repository under your account.
# 5) Submit to Coursera the URL to your GitHub repository that contains the completed R code for the assignment.

# In addition to submitting the URL for your GitHub repository, you will need to submit the 40 character SHA-1 hash 
#    (as string of numbers from 0-9 and letters from a-f) that identifies the repository commit that contains the 
#    version of the files you want to submit. You can do this in GitHub by doing the following

# 1) Going to your GitHub repository web page for this assignment
# 2) Click on the "?? commits" link where ?? is the number of commits you have in the repository. 
#    For example, if you made a total of 10 commits to this repository, the link should say "10 commits".

# 3) You will see a list of commits that you have made to this repository. The most recent commit is at the very top. 
#    If this represents the version of the files you want to submit, then just click the "copy to clipboard" button 
#    on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the 
#    course web site when you submit your assignment. If you don't want to use the most recent commit, then go down 
#    and find the commit you want and copy the SHA-1 hash.

# A valid submission will look something like (this is just an example!)

# https://github.com/rdpeng/ProgrammingAssignment2

# 7c376cc5447f11537f8740af8e07d6facc3d9645