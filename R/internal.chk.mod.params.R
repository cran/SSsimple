internal.chk.mod.params <-
function( F, H, Q, R, P0=NULL, beta0=NULL, d, n ) {
    if( is.matrix(Q) ) {
        
        dimQ <- dim(Q) ; if( dimQ[1] != d | dimQ[2] != d ) { print("Q must be d x d where d is width of H") ; return(NULL) }
    } else {
        if( length(Q) != 1 & length(Q) != d ) { print( "length of Q must be either 1 or width of H") ; return(NULL) }
        Q <- diag(Q, d)
    }
    if( is.matrix(R) ) {
        dimR <- dim(R) ; if( dimR[1] != n | dimR[2] != n ) { print("R must be n x n where n is height of H") ; return(NULL) }
    } else {
        if( length(R) != 1 & length(R) != n ) { print( "length of R must be either 1 or height of H" ) ; return(NULL) }
        R <- diag(R, n)
    }
    if( is.matrix(F) ) {
        dimF <- dim(F) ; if( dimF[1] != d | dimF[2] != d ) { print("F must be d x d where d is width of H") ; return(NULL) }
    } else {
        if( length(F) != 1 & length(F) != d ) { print( "length of F must be either 1 or width of H") ; return(NULL) }
        F <- diag(F, d)
    }
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
    return( list( F=F, H=H, Q=Q, R=R, P0=P0, beta0=beta0 ) )
}
