\name{H.omega.sincos}
\alias{H.omega.sincos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bases Transformation
}
\description{
Create H as sine-cosine bases expansion over R^1
}
\usage{
H.omega.sincos(x, u)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of locations of length \emph{n}.
}
  \item{u}{
A vector of frequencies of length \emph{d}/2.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
An \emph{n} x \emph{d} matrix.
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
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{H.omega.cos.2D}}
}

\examples{
x <- I(0:10) / 10
u <- I(1:4) * pi
H.omega.sincos(x, u)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
