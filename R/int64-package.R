

#' Class \code{"binary"}
#' 
#' Binary representation
#' 
#' 
#' @name binary-class
#' @aliases binary-class show,binary-method
#' @docType class
#' @section Objects from the Class: Objects can be created by one of the forms
#' of the \code{\link{binary}} methods.
#' @author Romain Francois, sponsored by the Google Open Source Programs Office
#' @keywords classes
#' @examples
#' 
#' binary( 1:4 )
#' 
NULL





#' Get binary representation
#' 
#' Get binary representation
#' 
#' 
#' @name binary-methods
#' @aliases binary binary-methods binary,integer-method binary,int64-method
#' binary,uint64-method binary,numeric-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"integer\")")}{ Method for integer Vectors }
#' 
#' \item{list("signature(object = \"int64\")")}{ Method for
#' \code{\linkS4class{int64}} vectors } \item{list("signature(object =
#' \"uint64\")")}{ Method for \code{\linkS4class{uint64}} vectors }
#' 
#' \item{list("signature(object = \"numeric\")")}{ Method for numeric vectors }
#' }
#' @keywords methods
#' @examples
#' 
#'     binary( 1:4 )
#'     binary( c( 1.0, 2.0 ) )
#'     binary( as.int64( 1:4 ) )
#'     binary( as.uint64( 1:4 ) )
#' 
NULL





#' Class \code{"int64"}
#' 
#' Vector of signed 64 bit integers
#' 
#' 
#' @name int64-class
#' @aliases int64-class [,int64-method Math,int64-method Math2,int64-method
#' [<-,int64-method Arith,ANY,int64-method Arith,int64,ANY-method
#' Arith,int64,int64-method as.character,int64-method names,int64-method
#' names<-,int64-method Compare,ANY,int64-method Compare,int64,ANY-method
#' Compare,int64,int64-method length,int64-method show,int64-method
#' Summary,int64-method c,int64-method is.na,int64-method
#' @docType class
#' @section Objects from the Class: Objects can be created by using the
#' \code{\link{int64}} function, by converting character vectors or integer
#' vectors using the \code{\link{as.int64}} function.
#' @author Romain Francois. Sponsored the Google Open Source Programs Office.
#' @seealso \code{\link{as.int64}} to convert character or integer vectors.
#' 
#' \code{\link{int64}} to create new \code{\linkS4class{int64}} vectors of a
#' given size.
#' 
#' The \code{\linkS4class{uint64}} class to represent unsigned 64 bit integer
#' vectors.
#' @keywords classes
#' @examples
#' 
#'     x <- int64( 4 )
#'     
#'     # setting subsets
#'     x[1:2] <- 1:2
#'     x[3:4] <- c("123456789012345", "9876543219876")
#'     x
#'     
#'     # arithmetic operations
#'     x * 2L
#'     x + x
#'     x - 3L
#'     
#'     # arithmetic operations first convert both operands to 64 bit integer type
#'     # so some precision will be lost
#'     as.int64(1) + 1.5
#'     # but it feels appropriate for dealing with large values
#'     as.int64(43124567245667) + 1.5
#'     
#'     # logical operations
#'     x < 3L
#'     x != c( 1L, 2L )
#'     
#'     # Summary operations
#'     range( x )
#'     min( x )
#'     max( x )
#'     length(x)
#'     
#'     
#'     df <- data.frame( a = 1:4 )
#'     df$b <- x
#'     df
#'     
#'     as.character( x )
#'     
#'     
#' 
NULL





#' 64 bit integer types
#'
#' An implementation of 64-bit integer vectors for R.
#' 
#' @name int64-package
#' @useDynLib int64, .registration = TRUE
#' @import methods
#' @docType package
#' @author Romain Francois, Sponsored by Google Open Source Programs Office, Balasubramanian Narasimhan
#' Maintainer: Balasubramanian Narasimhan <naras@stanford.edu>
#' @keywords package
#' @examples
#' 
#'     as.int64( 1:4 )
#'     as.int64( c("123456789", "9876543219876543" ) )
#' 
NULL


#' Sorting 64 bits integer vector
#' 
#' Sorting 64 bits integer vector
#' 
#' 
#' @name sort-methods
#' @aliases sort-methods sort,ANY-method sort,int64-method sort,uint64-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \"ANY\")")}{ Standard method (from base) }
#' 
#' \item{list("signature(x = \"int64\")")}{ Sorting signed 64 bit integer
#' vectors (\code{\linkS4class{int64}} }
#' 
#' \item{list("signature(x = \"uint64\")")}{ Sorting unsigned 64 bit integer
#' vectors (\code{\linkS4class{uint64}} } }
#' @keywords methods
NULL





#' Class \code{"uint64"}
#' 
#' Vector of signed 64 bit integers
#' 
#' 
#' @name uint64-class
#' @aliases uint64-class Math,uint64-method Math2,uint64-method [,uint64-method
#' [<-,uint64-method Arith,ANY,uint64-method Arith,uint64,ANY-method
#' Arith,uint64,uint64-method as.character,uint64-method names,uint64-method
#' names<-,uint64-method Compare,ANY,uint64-method Compare,uint64,ANY-method
#' Compare,uint64,uint64-method length,uint64-method show,uint64-method
#' Summary,uint64-method c,uint64-method is.na,uint64-method
#' @docType class
#' @section Objects from the Class: Objects can be created by using the
#' \code{\link{uint64}} function, by converting character vectors or integer
#' vectors using the \code{\link{as.uint64}} function.
#' @author Romain Francois. Sponsored the Google Open Source Programs Office.
#' @seealso \code{\link{as.uint64}} to convert character or integer vectors.
#' 
#' \code{\link{uint64}} to create new \code{\linkS4class{uint64}} vectors of a
#' given size.
#' 
#' The \code{\linkS4class{int64}} class to represent signed 64 bit integer
#' vectors.
#' @keywords classes
#' @examples
#' 
#'     x <- uint64( 4 )
#'     
#'     # setting subsets
#'     x[1:2] <- 1:2
#'     x[3:4] <- c("123456789012345", "9876543219876")
#'     x
#'     
#'     # arithmetic operations
#'     x * 2L
#'     x + x
#'     x - 3L
#'     
#'     # logical operations
#'     x < 3L
#'     x != c( 1L, 2L )
#'     
#'     # Summary operations
#'     range( x )
#'     min( x )
#'     max( x )
#'     length(x)
#'     
#'     
#'     df <- data.frame( a = 1:4 )
#'     df$b <- x
#'     df
#'     
#'     as.character( x )
#'     
#'     
#' 
NULL





#' Unique implementation for 64 bit integer vectors
#' 
#' Implementation of \code{\link{unique}} for 64 bit integer vectors
#' 
#' 
#' @name unique-methods
#' @aliases unique-methods unique,ANY-method unique,int64-method
#' unique,uint64-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \"ANY\")")}{default implementation (from base)}
#' 
#' \item{list("signature(x = \"int64\")")}{ signed 64 bit integer vectors.
#' \code{\linkS4class{int64}} }
#' 
#' \item{list("signature(x = \"uint64\")")}{ unsigned 64 bit integer vectors.
#' \code{\linkS4class{uint64}} } }
#' @keywords methods
#' @examples
#' 
#'     x <- as.int64( c(1:5, 1L, 5L) )
#'     unique( x )
#' 
#'     x <- as.uint64( c(1:5, 1L, 5L) )
#'     unique( x )    
#' 
NULL



