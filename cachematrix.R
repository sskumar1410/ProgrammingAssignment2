#This program is to calculate the inverse of the matrix
#Two functions have been defined in this,namely 
#MakeCacheMatrix(),CacheSolve()
#Efficacy of this program , it would n't recalcaulate the inverse of the matrix
#if it is already once calculated (in order to save the computation time)
#The value will be fetched from the stored object directly instead of recalculating once
#it again

#Decription about the function makeCacheMatrix
#This internally contains list of functions
#1)set()- Setting the value of the matrix
#2)get() - will  retrive the value of the matrix
#3)setinv()- Will calculate the inverse of the matrix
#4)getinv()-To retrive the inverse of the matrix
#This function would call the function cacheSolve() for the computing Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #setting the value of the input matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # Retriving the value of the passed matrix
        get <- function() x
        
        
        # setting the inverse of the matrix
        setinv<- function(inverse) 
        {
                m <<- inverse
        } 
        
        #Calling the function cacheSolve to find the inverse of the matrix
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

#Description about the function cacheSolve
#This will calculate the inverse of the matrix by using the function solve(x)
#When second time this function is called,it would check value of the matrix 
#If it is n't  null , it would pass the value of object m ( already calculated
#value), By this way the computaion time is saved.

cacheSolve <- function(x, ...) {
        m<-x$getinv()
        if(!is.null(m)){
                message(" getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinv(m)
        m
        
        #the function will return the inverse of the matrix 
       
}


#Sample Test Results
#> v<-makeCacheMatrix()
#> v$set(matrix(c(0,2,2,1),2,2))
#> v$get()
#     [,1] [,2]
#[1,]    0    2
#[2,]    2    1
# Calculating inverse of the matrix
#> cacheSolve(v)
#      [,1] [,2]
#[1,] -0.25  0.5
#[2,]  0.50  0.0
#> cacheSolve(v)
#Second time when called ,it is retriveing the cache value
# getting cached data
#      [,1] [,2]
#[1,] -0.25  0.5
#[2,]  0.50  0.0

