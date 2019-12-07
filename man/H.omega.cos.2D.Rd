\name{H.omega.cos.2D}
\alias{H.omega.cos.2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bases Transformation
}
\description{
Create H as cosine bases expansion over R^2
}
\usage{
H.omega.cos.2D(x, y, u.x, u.y, phs.x, phs.y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of locations on \emph{x} of length \emph{n}.
}
  \item{y}{
A vector of locations on \emph{y} of length \emph{n}.
}
  \item{u.x}{
A vector of frequencies on \emph{x}.
}
  \item{u.y}{
A vector of frequencies on \emph{y}.
}
  \item{phs.x}{
A vector of phase shifts on \emph{x}.  Should be same length as \code{u.x}.
}
  \item{phs.y}{
A vector of phase shifts on \emph{y}.  Should be same length as \code{u.y}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
An \emph{n} x \emph{d} matrix, with \emph{d} = \code{length(u.x) * length(u.y)}.
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
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{H.omega.sincos}}
}



\examples{
x <- rep( I(0:10) / 10,   11 )
y <- rep( I(0:10) / 10,   each=11 )
u.x <- I(1:2) * pi
u.y <- I(1:2) * pi
H <- H.omega.cos.2D(x, y, u.x, u.y, c(0,0), c(0,0))

b <- rep(1, ncol(H))

z <- H \%*\% b

plot(x, y, cex=z-min(z))

}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
