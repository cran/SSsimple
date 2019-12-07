internal.chk.mod.params.short <-
function( P0, beta0, d, n ) {
    if( !is.null(beta0) ) {
        if( length( beta0 ) != 1 & length( beta0 ) != d ) {
            return( "length of beta0 must be either 1 or width of H" )
        } else {
            if( length( beta0 ) == 1 ) {
                beta0 <- rep( beta0, d )
            } else {
                beta0 <- beta0
            }
        }
    }
    if( !is.null(P0) ) {
        if( is.matrix(P0) ) {
            dimP0 <- dim(P0) ; if( dimP0[1] != d | dimP0[2] != d ) { print("P0 must be d x d where d is width of H") ; return(NULL) }
        } else {
            if( length(P0) != 1 & length(P0) != d ) { print( "length of P0 must be either 1 or width of H") ; return(NULL) }
            P0 <- diag(P0, d)
        }
    }
    return( list( P0=P0, beta0=beta0 ) )
}
