\name{SS.solve.SMW}
\alias{SS.solve.SMW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal Estimation
}
\description{
Solve a state space system using the Kalman Filter.
}
\usage{
SS.solve.SMW(Z, F, H, Q, inv.R, length.out, P0, beta0=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
A \emph{T} x \emph{n} data matrix. 
}
  \item{F}{
The state matrix.  A scalar, or vector of length \emph{d}, or a \emph{d} x \emph{d} matrix.  When scalar, \code{F} is constant diagonal.  When a vector, \code{F} is diagonal.  
}
  \item{H}{
The measurement matrix.  Must be \emph{n} x \emph{d}. 
}
  \item{Q}{
The state variance.  A scalar, or vector of length \emph{d}, or a \emph{d} x \emph{d} matrix.  When scalar, \code{Q} is constant diagonal.  When a vector, \code{Q} is diagonal.
}
  \item{inv.R}{
The inverse of the measurement variance.  A scalar, or vector of length \emph{n}, or a \emph{n} x \emph{n} matrix.  When scalar, \code{inv.R} is constant diagonal.  When a vector, \code{inv.R} is diagonal.
}
  \item{length.out}{
Scalar integer.
}
  \item{P0}{
Initial \emph{a priori} prediction error.
}
  \item{beta0}{
Initial state value.  A scalar, or a vector of length \emph{d}.
}
}


\details{
\code{H} is the master argument from which system dimensionality is determined.  Otherwise identical to \code{\link{SS.solve}}, except that the Woodbury identity is used for inversion.  This method offers a computationally reduced means of solving the system realization of interest; however, this method must be supplied with the inverse of the measurement variance matrix, R -- not R.
}


\value{
A named list.
\item{B.apri}{A \emph{T} x \emph{d} matrix, the \emph{i}th row of which is the best state estimate prior to observing data at time \emph{i}.}
\item{B.apos}{A \emph{T} x \emph{d} matrix, the \emph{i}th row of which is the best state estimate given the observation at time \emph{i}.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}

\note{
For a definition of the system of interest, please see \code{\link{SSsimple}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}


\examples{
set.seed(999)
H <- matrix(1)
R <- 7
inv.R <- 1 / R
x <- SS.sim( 1, H, 1, R, 100, 0 )
y <- SS.solve.SMW( x$Z, 1, H, 1, inv.R, 100, 10^5, 0 )

z.hat <- t( H \%*\% t( y$B.apri ) )

plot( x$Z, type="l", col="blue" )
points( z.hat[ ,1], type="l", col="red" )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
