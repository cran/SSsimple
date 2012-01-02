\name{SS.stst.tv}
\alias{SS.stst.tv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Steady State
}
\description{
Find steady state of time-varying system, i.e., locate when Kalman gain converges
}
\usage{
SS.stst.tv(F, H, Q, R, P0, epsilon, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{F}{
A list of \emph{d} x \emph{d} matrices.
}
  \item{H}{
A list of \emph{n} x \emph{d} matrices.
}
  \item{Q}{
A list of \emph{d} x \emph{d} matrices.
}
  \item{R}{
A list of \emph{n} x \emph{n} matrices.
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

\details{
Note:  The test for convergence has been (very, very slightly) modified since v0.5.1.  The current test has been implemented for rigor.  Users who have results based on earlier releases may observe infinitesimal differences in the resulting prediction error.
}

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
F.tv <- list()
for(i in 1:10000) {
	F.tv[[i]] <- diag( c(1/(i+10), 1/(i+10)) )
}

H <- matrix(1, 2, 2)

SS.stst.tv(F.tv, H, 1, 1, 10^5, 10^(-10), verbosity=2)
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
