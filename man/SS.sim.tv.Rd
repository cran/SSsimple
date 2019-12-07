\name{SS.sim.tv}
\alias{SS.sim.tv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation
}
\description{
Simulate a time-varying state space system
}
\usage{
SS.sim.tv(F, H, Q, R, length.out, beta0=0)
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
  \item{length.out}{
A scalar integer.
}
  \item{beta0}{
Initial state value.  A scalar, or a vector of length \emph{d}.
}
}
\details{
This function is a more general, and slower, implementation of \code{SS.sim}.  This function can also accept arguments in non-time-varying fashion (\emph{a la} \code{\link{SS.sim}}).
}



\value{
A named list.
\item{Beta}{A \emph{T} x \emph{d} matrix, the \emph{i}th row of which is the state at time \emph{i}.}
\item{Y}{A \emph{T} x \emph{n} matrix, the \emph{i}th row of which is noiseless observation at time \emph{i}.}
\item{Z}{A \emph{T} x \emph{n} matrix, the \emph{i}th row of which is observation at time \emph{i}.}
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
set.seed(9999)

H.tv <- list()
for(i in 1:200) {
	H.tv[[i]] <- matrix( c( sin(i * 0.05), cos(i * 0.05) ), 1, 2 )
}

ssx <- SS.sim.tv( 0.99, H.tv, 0.001, 1, 200, c(4,4) )

plot(ssx$Z[ ,1], type="l")
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
