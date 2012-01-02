\name{SS.stst}
\alias{SS.stst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Steady State
}
\description{
Find steady state of system, i.e., locate when Kalman gain converges
}
\usage{
SS.stst(F, H, Q, R, P0, epsilon, verbosity=0)
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
The measurement variance.  A scalar, or vector of length \emph{n}, or a \emph{n} x \emph{n} matrix.  When scalar, \code{R} is constant diagonal.  When a vector, \code{R} is diagonal.
}
  \item{P0}{
Initial \emph{a priori} prediction error.
}
  \item{epsilon}{
A small scalar number.
}
  \item{verbosity}{
0, 1 or 2.
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A named list.
\item{P.apri}{A \emph{d} x \emph{d} matrix giving \emph{a priori} prediction variance.}
\item{P.apos}{A \emph{d} x \emph{d} matrix giving \emph{a posteriori} prediction variance.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}


\examples{
H <- matrix(1)

SS.stst(1, H, 1, 1, P0=10^5, epsilon=10^(-14), verbosity=1)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
