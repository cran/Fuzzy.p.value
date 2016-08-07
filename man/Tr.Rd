\name{Tr}
\alias{Tr}

\title{
Class of trapezoidal fuzzy numbers
}

\description{
Several linear functions are constructed in this package for easily inspiration to the fuzzy concepts \eqn{\ll approximately smaller \gg} , \eqn{\ll approximately bigger \gg} and  \eqn{\ll approximately equal \gg}.
For example, function \eqn{Tr} is one of them which users can use it to construct the fuzzy concept \eqn{\ll approximately equal \gg} for fuzzy statistics or fuzzy hypotheses. 
As presented bellow, the membership function of fuzzy set \code{Tr} has four parameters \eqn{a}, \eqn{b}, \eqn{c} and \eqn{d}:
\deqn{ Tr(a,b,c,d)(x)=\left\{
\begin{array}{lcc}
\frac{x-a}{b-a}    &\ \ if   & \  \  a < x \leq b
\\
1   &\ \ if   & \  \  b < x \leq c
\\
\frac{x-d}{c-d}    &\ \ if   & \  \  c < x \leq d
\\
0   &\ \ if   & \  \  elsewhere
\end{array}
\right. }
}

\usage{
Tr(a, b, c, d)
}

\arguments{
  \item{a}{
Considering the introduced above membership function, the first parameter is the first point of the support of trapezoidal fuzzy number.
}
  \item{b}{
The second parameter is the first point of the core of trapezoidal fuzzy number.
}
  \item{c}{
The third parameter is the end point of the core of trapezoidal fuzzy number.
}
  \item{d}{
The fourth parameter is the end point of the support of trapezoidal fuzzy number.
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
This function easily introduce the membership function of a trapezoidal fuzzy number.
}

\references{
Parchami, A., Taheri, S. M., and Mashinchi, M. (2010). Fuzzy p-value in testing fuzzy hypotheses with crisp data. Statistical Papers 51: 209-226.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2012). Testing fuzzy hypotheses based on vague observations: a p-value approach. Statistical Papers 53: 469-484.
}

%% \author{
%% Abbas Parchami }

\examples{
# Introducing the membership function of trapezoidal fuzzy number (for test statistics t)
t = Tr(2,4,6,9)

## The function is currently defined as
function (a, b, c, d) 
(TrapezoidalFuzzyNumber(a, b, c, d))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fuzzy p-value }
\keyword{ Fuzzy significance level }
\keyword{ Fuzzy data }
\keyword{ Fuzzy hypothesis }
\keyword{ Trapezoidal fuzzy number }
\keyword{ Zadehs extension principle }
\keyword{ Package "FuzzyNumbers" }
