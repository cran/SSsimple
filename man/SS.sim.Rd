\name{SS.sim}
\alias{SS.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation
}
\description{
Simulate a state space system
}
\usage{
SS.sim(F, H, Q, R, length.out, beta0=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{F}{
The state matrix.  A scalar, or vector of length \emph{d}, or a \emph{d} x \emph{d} matrix.  When scalar, \code{F} is constant diagonal.  When a vector, \code{F} is diagonal.
}
  \item{H}{
The measurement matrix.  Must be \emph{n} x \emph{d}.
}
  \item{Q}{
The state variance.  A scalar, or vector of length \emph{d}, or a \emph{d} x \emph{d} matrix.  When scalar, \code{Q} is constant diagonal.  When a vector, \code{Q} is diagonal.
}
  \item{R}{
The measurement variance.  A scalar, or vector of length \emph{n}, or an \emph{n} x \emph{n} matrix.  When scalar, \code{R} is constant diagonal.  When a vector, \code{R} is diagonal.
}
  \item{length.out}{
Scalar integer.
}
  \item{beta0}{
Initial state value.  A scalar, or a vector of length \emph{d}.
}
}
\details{
\code{H} is the master argument from which system dimensionality is determined.
}
\value{
A named list.
\item{Beta}{A \emph{T} x \emph{d} matrix, the \emph{i}th row of which is the state at time \emph{i}.}
\item{Y}{A \emph{T} x \emph{n} matrix, the \emph{i}th row of which is the noiseless observation at time \emph{i}.}
\item{Z}{A \emph{T} x \emph{n} matrix, the \emph{i}th row of which is the observation at time \emph{i}.}
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
tau <- 30	
	
x <- I( 0:10 / 10 )

H <- H.omega.sincos( x, c( 1*pi, 4*pi ) )

xs <- SS.sim( 0.99, H, 1, 2, tau, rep(0, ncol(H)) )

\dontrun{
for(i in 1:nrow(xs$Z)) {
	plot(x, xs$Z[ i, ], ylim=range(xs$Z), main=i)
	Sys.sleep(1/10)
}
}
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
