# Copyright (C) 2011 Romain Francois
# Copyright (C) 2011 Google Inc.  All rights reserved.
#
# This file is part of int64.
#
# int64 is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# int64 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with int64.  If not, see <http://www.gnu.org/licenses/>.

setClassUnion( "maybeNames", c("character", "NULL" ) )

names_int64 <- function(x){
    x@NAMES    
}
namesgets_int64 <- function( x, value){
    if( missing(value) || is.null(value) ){
        x@NAMES <- NULL
    } else if( is.character( value )){
        if( length( value ) == length( x@.Data ) ){
            x@NAMES <- value
        } else if(length(value) < length(x@.Data) ) {
            x@NAMES <- c( value, rep( NA, length(x@.Data) - length(value) ) )
        } else {
            stop( "error assigning names" )
        }
    } else {
        stop( "must be character vector or NULL" )
    }
    x    
}

#' @export
setClass( "int64", contains = "list", 
    representation( NAMES = "maybeNames")
)

#' @export
setClass( "uint64", contains = "list" ,
    representation( NAMES = "maybeNames")
)

setMethod( "names", "int64", names_int64 )
setMethod( "names<-", "int64", namesgets_int64 )
setMethod( "names", "uint64", names_int64 )
setMethod( "names<-", "uint64", namesgets_int64 )


#' @export
setClass( "binary",  
    representation( data = "character", bits = "integer" )
)

setGeneric( "binary", function(object) standardGeneric("binary") )

#' @exportMethod binary
setMethod( "binary", "integer", function(object){
   new( "binary", data = .Call( int64_format_binary, object ), bits = 32L ) 
} )

setMethod( "binary", "numeric", function(object){
   new( "binary", data = .Call( int64_format_binary, object ), bits = 64L ) 
} )
setMethod( "binary", "int64", function(object){
   new( "binary", data = .Call( int64_format_binary, object ), bits = 64L ) 
} )
setMethod( "binary", "uint64", function(object){
   new( "binary", data = .Call( int64_format_binary, object ), bits = 64L ) 
} )

#' @exportMethod show
setMethod( "show", "binary", function(object){
    print( noquote( object@data ) )
    invisible(object)
})



#' Creates new int64 vectors of a given length
#' 
#' Creates new \code{\linkS4class{int64}} vectors of a given length
#' 
#' 
#' @param length Length of the resulting vector
#' @return A new \code{\linkS4class{int64}} vector of the given length
#' @author Romain Francois, Sponsored by the Google Open Source Programs Office
#' @seealso \code{\link{as.int64}} for converting integer or character vectors
#' into \code{\linkS4class{int64}} vectors.
#' 
#' \code{\link{uint64}} for unsigned 64 bit integer vectors, i.e. of class
#' \code{\linkS4class{uint64}}
#' @keywords manip
#' @examples
#' 
#' x <- int64(10L)
#' 
#' @export int64
int64 <- function(length=0L){
    x <- new("int64", rep( list(integer(2L)), length ) )
    x
}


#' Creates new uint64 vectors of a given length
#' 
#' Creates new \code{\linkS4class{uint64}} vectors of a given length
#' 
#' 
#' @param length Length of the resulting vector
#' @return A new \code{\linkS4class{uint64}} vector of the given length
#' @author Romain Francois, Sponsored by the Google Open Source Programs Office
#' @seealso \code{\link{as.uint64}} for converting integer or character vectors
#' into \code{\linkS4class{uint64}} vectors.
#' 
#' \code{\link{int64}} for signed 64 bit integer vectors, i.e. of class
#' \code{\linkS4class{int64}}.
#' @keywords manip
#' @examples
#' 
#' x <- uint64(10L)
#' 
#' @export uint64
uint64 <- function(length=0L){
    x <- new("uint64", rep( list(integer(2L)), length ) )
    x
}

#' @exportMethod length
setMethod( "length", "int64", function(x){
    length(x@.Data)
} )
setMethod( "length", "uint64", function(x){
    length(x@.Data)
} )
show_int64 <- function(object){
    if( !length(object)) {
        writeLines( sprintf( "%s(0)", class(object) ) )
    } else { 
        if( is.null( object@NAMES ) ){
            print( noquote( as.character( object ) ) )
        } else {
            x <- as.character( object )
            names(x) <- object@NAMES
            print(noquote(x))
        }
    }
    invisible(object)
} 
setMethod( "show", "int64", show_int64)
setMethod( "show", "uint64", show_int64)



#' Convert character or integer vectors into int64 vectors.
#' 
#' Convert character or integer vectors into \code{\linkS4class{int64}}
#' vectors.
#' 
#' For conversion of character vectors, the C function \code{atol} is used.
#' 
#' @param x A character or integer vector
#' @return A new \code{\linkS4class{int64}} vector.
#' @author Romain Francois, sponsored by the Google Open Source Programs Office
#' @seealso \code{\link{as.uint64}} for conversion to unsigned long vectors.
#' @references C++ \code{atol} function:
#' \url{https://en.cppreference.com/w/c/string/byte/atoi}
#' @keywords manip
#' @examples
#' 
#' as.int64( c(1L, 2L ) )
#' as.int64( c("123456789123456", "987654321987654" ) )
#' 
#' @export as.int64
as.int64 <- function(x){
    if( is.character(x) ){
        wrong <- ! grepl("^[-]?[0-9]+$", x) 
        if( any(wrong) ){
            x[wrong] <- "NA"
            warning( "NAs introduced" )
        }
    }
    new( "int64", .Call(int64_as_int64, x) ) 
}


#' Convert character or integer vectors into uint64 vectors.
#' 
#' Convert character or integer vectors into \code{\linkS4class{uint64}}
#' vectors.
#' 
#' For conversion of character vectors, the C function \code{atol} is used.
#' 
#' @param x A character or integer vector
#' @return A new \code{\linkS4class{int64}} vector.
#' @author Romain Francois, sponsored by the Google Open Source Programs Office
#' @seealso \code{\link{as.int64}} for conversion to signed long vectors.
#' @references C++ \code{atol} function:
#' \url{https://en.cppreference.com/w/c/string/byte/atoi}
#' @keywords manip
#' @examples
#' 
#' as.uint64( c(1L, 2L ) )
#' as.uint64( c("123456789123456", "987654321987654" ) )
#' 
#' @export as.uint64
as.uint64 <- function(x){
    if( is.character(x) ){
        wrong <- ! grepl("^[0-9]+$", x) 
        if( any(wrong) ){
            x[wrong] <- "NA"
            warning( "NAs introduced" )
        }
    }
    new( "uint64", .Call(int64_as_uint64, x) ) 
}

#' @exportMethod [
setMethod( "[", "int64", function(x, i, j, ...){
    new( "int64", x@.Data[ i ] )
} )
setMethod( "[", "uint64", function(x, i, j, ...){
    new( "uint64", x@.Data[ i ] )
} )
setMethod( "[<-", "int64", function(x, i, j, ..., value ){
    data <- x@.Data
    data[i] <- as.int64( value )@.Data
    new( "int64", data )
} )
setMethod( "[<-", "uint64", function(x, i, j, ..., value ){
    data <- x@.Data
    data[i] <- as.uint64( value )@.Data
    new( "uint64", data )
} )



#' @exportMethod Arith
setMethod( "Arith", signature(e1 = "int64", e2 = "int64" ), 
function(e1,e2){
   numbers <- .Call( int64_arith_int64_int64, .Generic, e1, e2, FALSE )
   new( "int64", numbers ) 
} )
setMethod( "Arith", signature(e1 = "int64", e2 = "ANY" ), 
function(e1,e2){
   numbers <- .Call( int64_arith_int64_int64, .Generic, e1, as.int64(e2), FALSE )
   new( "int64", numbers )     
} )
setMethod( "Arith", signature(e1 = "ANY", e2 = "int64" ), 
function(e1,e2){
   numbers <- .Call( int64_arith_int64_int64, .Generic, as.int64(e1), e2, FALSE )
   new( "int64", numbers ) 
} )

setMethod( "Arith", signature(e1 = "uint64", e2 = "uint64" ), 
function(e1,e2){
   numbers <- .Call( int64_arith_int64_int64, .Generic, e1, e2, TRUE )
   new( "uint64", numbers ) 
} )
setMethod( "Arith", signature(e1 = "uint64", e2 = "ANY" ), 
function(e1,e2){
   numbers <- .Call( int64_arith_int64_int64, .Generic, e1, as.uint64(e2), TRUE )
   new( "uint64", numbers )     
} )
setMethod( "Arith", signature(e1 = "ANY", e2 = "uint64" ), 
function(e1,e2){
   numbers <- .Call( int64_arith_int64_int64, .Generic, as.uint64(e1), e2, TRUE )
   new( "uint64", numbers ) 
} )

#' @exportMethod Compare
setMethod( "Compare", signature(e1 = "int64", e2 = "int64" ), 
function(e1,e2){
   .Call( int64_compare_int64_int64, .Generic, e1, e2, FALSE )
} )
setMethod( "Compare", signature(e1 = "ANY", e2 = "int64" ), 
function(e1,e2){
   .Call( int64_compare_int64_int64, .Generic, as.int64(e1), e2, FALSE )
} )
setMethod( "Compare", signature(e1 = "int64", e2 = "ANY" ), 
function(e1,e2){
   .Call( int64_compare_int64_int64, .Generic, e1, as.int64(e2), FALSE )
} )

setMethod( "Compare", signature(e1 = "uint64", e2 = "uint64" ), 
function(e1,e2){
   .Call( int64_compare_int64_int64, .Generic, e1, e2, TRUE )
} )
setMethod( "Compare", signature(e1 = "ANY", e2 = "uint64" ), 
function(e1,e2){
   .Call( int64_compare_int64_int64, .Generic, as.uint64(e1), e2, TRUE )
} )
setMethod( "Compare", signature(e1 = "uint64", e2 = "ANY" ), 
function(e1,e2){
   .Call( int64_compare_int64_int64, .Generic, e1, as.uint64(e2), TRUE)
} )


#' @exportMethod Summary
setMethod( "Summary", "int64", function(x,..., na.rm = FALSE){
   .Call( int64_summary_int64, .Generic, x, FALSE)
} )
setMethod( "Summary", "uint64", function(x,..., na.rm = FALSE){
   .Call( int64_summary_int64, .Generic, x, TRUE)
} )

#' @exportMethod as.character
setMethod( "as.character", "int64", function(x,...){
  .Call( int64_as_character_int64, x, FALSE)  
})
setMethod( "as.character", "uint64", function(x,...){
  .Call( int64_as_character_int64, x, TRUE)  
})


#' @method as.data.frame int64
#' @export
as.data.frame.int64 <- as.data.frame.vector

#' @method as.data.frame uint64
#' @export
as.data.frame.uint64 <- as.data.frame.vector

#' @method format int64
#' @export
format.int64 <- format.uint64 <- function(x, ...){
    as.character(x)   
}

#' @method format uint64
#' @export
format.uint64 <- format.int64

#'
#'
#' 
#' Give numeric limits of integer types
#' 
#' Give numeric limits of integer types
#' 
#' 
#' @param type type. must be integer, int64 or uint64.
#' @return A vector of two values of the appropriate type.
#' @author Romain Francois, sponsored by the Google Open Source Programs Office
#' @keywords manip
#' @examples
#' 
#' numeric_limits( "integer" )
#' numeric_limits( "int64" )
#' numeric_limits( "uint64" )
#' 
#' @export numeric_limits
numeric_limits <- function( type ){
    .Call( int64_limits, type )
}

setGeneric( "unique" )
#' @exportMethod unique
setMethod( "unique", "int64", function(x, incomparables = FALSE, ...){
    new( "int64", .Data = unique( x@.Data, incomparables, ... ) )  
} )
setMethod( "unique", "uint64", function(x, incomparables = FALSE, ...){
    new( "uint64", .Data = unique( x@.Data, incomparables, ... ) )  
} )

setGeneric( "sort" )

#' @exportMethod sort
setMethod( "sort", "int64", function(x, decreasing = FALSE, ...){
    .Call( int64_sort, x, FALSE, decreasing )
} )
setMethod( "sort", "uint64", function(x, decreasing = FALSE, ...){
    .Call( int64_sort, x, TRUE, decreasing )
} )

#' @exportMethod Math
setMethod( "Math", "int64", function(x){
    .Call( int64_math, .Generic, x, FALSE)
} )
setMethod( "Math", "uint64", function(x){
    .Call( int64_math, .Generic, x, TRUE )
} )

# implementation of signif using string maniplation
int64_Math2 <- function( type = "int64", .Generic, x, digits ){
    if( .Generic == "round" ) x else{
        if( any(digits<0 ) ) stop("digits must be positive")
        
        # signif
        s <- as.character( x )
        len <- nchar( s )
        signs <- ! grepl( "^-", s )
        s <- sub( "^-", "", s ) 
        
        # recycling
        digits <- as.integer( rep( digits, length = length( s ) ) )
        digits[ digits == 0L ] <- 1L
        
        res <- .Call( int64_signif, s, digits, len )
        res <- sprintf( "%s%s", ifelse(signs, "", "-"), res )
        
        if( type == "int64" ) as.int64(res) else as.uint64(res) 
    }
}

#' @exportMethod Math2
setMethod( "Math2", "int64", function(x, digits  = 6L){
    int64_Math2( "int64", .Generic, x, digits )
} )
setMethod( "Math2", "uint64", function(x, digits = 6L){
    int64_Math2( "uint64", .Generic, x, digits )
} )


#' @exportMethod is.na
setMethod( "is.na", "int64", function(x){
  .Call( int64_isna, x, FALSE )  
})
setMethod( "is.na", "uint64", function(x){
  .Call( int64_isna, x, TRUE )  
})

c_int64 <- function(as, ctor){
    function(x, ..., recursive = FALSE ){
    dots <- list(...)
    if( !length(dots) ) return(x)
    
    dots <- lapply( dots, function(x) as(x)@.Data )
    n <- length(x) + sum( sapply( dots, length ) ) 
    
    res <- ctor(n) 
    
    res@.Data[ 1:length(x) ] <- x@.Data
    start <- length(x)+1L
    for( i in 1:length(dots)){
        data <- dots[[i]]
        res@.Data[ start:(start+length(data)-1L) ] <- data
        start <- start + length(data)
    }
    res
} 
}


#' @exportMethod c
setMethod( "c", "int64", c_int64( as.int64, int64 ) )
setMethod( "c", "uint64", c_int64( as.uint64, uint64 ) )

setAs("character", "int64", function(from) as.int64(from))
setAs("character", "uint64", function(from) as.uint64(from))

setAs("integer", "int64", function(from) as.int64(from))
setAs("integer", "uint64", function(from) as.uint64(from))

setAs("logical", "int64", function(from) as.int64(from))
setAs("logical", "uint64", function(from) as.uint64(from))

setAs("numeric", "int64", function(from) as.int64(from))
setAs("numeric", "uint64", function(from) as.uint64(from))

#' @importFrom utils head
#' @method str int64
#' @export
str.int64 <- str.uint64 <- function(object, ...){
    writeLines( sprintf( " %s [1:%d] %s ...", class(object), length(object), 
        paste( as.character( utils::head( object, 3 ) ), collapse = " " )
    ) )
}

#' @method str uint64
#' @export
str.uint64 <- str.int64
