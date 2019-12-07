\name{SS.solve.tv}
\alias{SS.solve.tv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal Estimation
}
\description{
Solve a time-varying state space system using the Kalman Filter
}
\usage{
SS.solve.tv(Z, F, H, Q, R, length.out, P0, beta0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
A \emph{T} x \emph{n} data matrix 
}
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
  \item{length.out}{
A scalar integer.
}
  \item{P0}{
Initial \emph{a priori} prediction error.
}
  \item{beta0}{
Initial state value.  A scalar, or a vector of length \emph{d}.
}
}
\details{
This function is a more general, and slower, implementation of \code{SS.solve}.  This function can also accept arguments in non-time-varying fashion (\emph{a la} \code{\link{SS.solve}}).
}
\value{
A named list.
\item{B.apri}{A \emph{T} x \emph{d} matrix, the \emph{i}th row of which is the best state estimate prior to observing data at time \emph{i}.}
\item{B.apos}{A \emph{T} x \emph{d} matrix, the \emph{i}th row of which is the best state estimate given the observation at time \emph{i}.}
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

\seealso{
\code{\link{SS.solve}}
}


%%\examples{
%%
%%}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
