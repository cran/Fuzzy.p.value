\name{T}
\alias{T}

\title{
Class of triangular fuzzy numbers
}

\description{
The are several famous fuzzy sets which have meaningful means and are user friendly for introducing. 
Several linear functions are constructed in this package for easily inspiration to the fuzzy concepts \eqn{\ll approximately smaller \gg} , \eqn{\ll approximately bigger \gg} and  \eqn{\ll approximately equal \gg}.
For example, function \eqn{T} is one of them which users can use it to construct the fuzzy concept \eqn{\ll approximately equal \gg} in fuzzy statistics and fuzzy hypotheses. 
As presented bellow, the membership function of fuzzy set \code{T} has three parameters \eqn{a}, \eqn{b} and  \eqn{c}:
\deqn{ T(a,b,c)(x)=\left\{
\begin{array}{lcc}
\frac{x-a}{b-a}    &\ \ if   & \  \  a < x \leq b
\\
\frac{x-c}{b-c}    &\ \ if   & \  \  b < x \leq c
\\
0   &\ \ if   & \  \  elsewhere
\end{array}
\right. }
}

\usage{
T(a, b, c)
}

\arguments{
  \item{a}{
Considering the introduced above membership function, the first parameter is the first point of the support of triangular fuzzy number.
}
  \item{b}{
The second parameter is the core of triangular fuzzy number.
}
  \item{c}{
The third parameter is the end point of the support of triangular fuzzy number.
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
This function easily introduce the membership function of a triangular fuzzy number.
}

\references{
Parchami, A., Taheri, S. M., and Mashinchi, M. (2010). Fuzzy p-value in testing fuzzy hypotheses with crisp data. Statistical Papers 51: 209-226.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2012). Testing fuzzy hypotheses based on vague observations: a p-value approach. Statistical Papers 53: 469-484.
}

%% \author{
%% Abbas Parchami }

\examples{
# Introducing the membership function of triangular fuzzy number (for test statistics)
t = T(2,4,7)

## The function is currently defined as
function (a, b, c) 
(TriangularFuzzyNumber(a, b, c))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fuzzy p-value }
\keyword{ Fuzzy significance level }
\keyword{ Fuzzy data }
\keyword{ Fuzzy hypothesis }
\keyword{ Triangular fuzzy number }
\keyword{ Zadehs extension principle }
\keyword{ Package "FuzzyNumbers" }
