\name{SS.ID}
\alias{SS.ID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
System Identification
}
\description{
Perform non-iterative, subspace grey-box system identification
}
\usage{
SS.ID(Z, d, rsN = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
A \emph{T} x \emph{n} data matrix 
}
  \item{d}{
A scalar integer.  The system ``order.''
}
  \item{rsN}{
A 3-element integer vector, containing \emph{r}, \emph{s}, \emph{N} as described by Ljung.
}
}
\details{
Only works when \emph{T} >> \emph{n} (one resolved to using \code{SS.ID} when this is not true is free to pluck columns from Z until it is).

Complaints issued from this function to the effect that a matrix that is some function of ``PP'' cannot be inverted might be remedied by turning \emph{r} and \emph{s} down (the first two elements of the \code{rsN} argument), or perhaps by adding a small amount of noise to Z.

This is subspace estimation.  \code{SS.ID} estimates system hyperparameters from data.  One can usually henceforth solve (using \code{\link{SS.solve}}) for good quality observation-space estimates, but should not assume the resulting state estimates are anywhere near truth.  One may wish to use estimates generated with this function as initial values for iterative estimation techniques, e.g., package \code{Stem}.
}
\value{
A named list.
\item{F.hat}{A \emph{d} x \emph{d} matrix.}
\item{H.hat}{An \emph{n} x \emph{d} matrix.}
\item{Q.hat}{A \emph{d} x \emph{d} matrix.}
\item{R.hat}{An \emph{n} x \emph{n} matrix.}
}

\references{
Lennart Ljung.  \emph{System Identification, Theory for the User}.  Prentice Hall, 1999.
}

\author{
%%  ~~who you are~~
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}


\examples{
Q <- diag(1/10, 2)
R <- diag(2, 3)
H <- matrix(1, 3, 2)
F <- diag(0.99, 2)

set.seed(9999)
xs <- SS.sim(F, H, Q, R, 2000, rep(0, 2))

## notice that while the parameter estimates appear somewhat inaccurate ...
ssid <- SS.ID( xs$Z , 2,  c(3, 6, 900) ) ; ssid

## the observation estimate:
sss <- SS.solve( xs$Z, ssid$F.hat, ssid$H.hat, ssid$Q.hat, ssid$R.hat, nrow(xs$Z), 10^5, c(0,0)) 
Z.hat <- t( ssid$H.hat \%*\% t( sss$B.apri ) )
sqrt( mean( (xs$Z - Z.hat)^2 ) )

## is nontheless very close to that using true hyperparameter values:
sss.true <- SS.solve( xs$Z, F, H, Q, R, nrow(xs$Z), 10^5, c(0,0)) 
Z.hat <- t( H \%*\% t( sss.true$B.apri ) )
sqrt( mean( (xs$Z - Z.hat)^2 ) )
}




% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
