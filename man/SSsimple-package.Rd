\name{SSsimple-package}
\alias{SSsimple-package}
\alias{SSsimple}
\docType{package}
\title{
Simple State Space Models
}
\description{
Simulate, solve (estimate), fit state space models
}

\details{
\tabular{ll}{
Package: \tab SSsimple\cr
Type: \tab Package\cr
Version: \tab 0.6.1 \cr
Date: \tab 2013-03-31\cr
License: \tab GPL (>= 2) \cr
LazyLoad: \tab yes\cr
}

If you wish to parameterize a state space model given only data, \code{Z}, use the function \code{\link{SS.ID}}.  If you wish to simulate data, use \code{\link{SS.sim}} or \code{\link{SS.sim.tv}}.  If you have data, know the model parameters, and wish to solve the lowest L2 estimate, use \code{\link{SS.solve}} or \code{\link{SS.solve.tv}}.

The two functions, \code{\link{H.omega.sincos}} and \code{\link{H.omega.cos.2D}}, provide a means of introducing response curvature over a domain (probably space) through the common sine bases expansion.

Finally, \code{\link{SS.stst}} and \code{\link{SS.stst.tv}}, attempt to find the time at which a system acheives a ``steady-state.''

The system of interest is defined as

\bold{b}(t) = \bold{F} \bold{b}(t-1) + \bold{n}(t) ,    \bold{n}(t) ~ \emph{N}[\bold{0}, \bold{Q}]

\bold{z}'(t) = \bold{H} \bold{b}(t) + \bold{e}(t) ,    \bold{e}(t) ~ \emph{N}[\bold{0}, \bold{R}]

%\deqn{\beta(t) = F \beta(t-1) + \nu(t) , \nu(t) ~ N[ 0, Q ]}
%\deqn{z'(t) = H  \beta(t) + \epsilon(t)   ,   \epsilon(t) ~ N[ 0, R ]}

Functions whose names end in ``.tv'' provide for the usage of time-varying \code{F}, \code{H}, \code{Q}, \code{R}.
}


\author{
Dave Zes

Maintainer: Dave Zes <zesdave@gmail.com>
}

%%\references{
%% ~~ Literature or other references for background information ~~
%%}

%% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%% ~~ the R documentation directory ~~

\keyword{ package }

%%\seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%% \code{\link[<pkg>Stem<pkg>]{<Stem>}} ~~
%%}

%%\examples{
%% ~~ simple examples of the most important functions ~~
%%}
