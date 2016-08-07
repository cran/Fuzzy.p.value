\name{S}
\alias{S}

\title{
Class of linear fuzzy sets to construct concept "approximately smaller"
}

\description{
The are several famous fuzzy sets which have meaningful means and are user friendly for introducing. 
Several linear functions are constructed in this package for easily inspiration to the fuzzy concepts \eqn{\ll approximately smaller \gg} , \eqn{\ll approximately bigger \gg} and  \eqn{\ll approximately equal \gg}.
For example, function \eqn{S} is one of them which can users can use it to construct the fuzzy concept \eqn{\ll approximately smaller \gg} in fuzzy hypotheses. 
As presented bellow, the membership function of fuzzy set \code{S} has two parameters \eqn{a} and  \eqn{b}:
\deqn{ S(a,b)(x)=\left\{
\begin{array}{lcc}
1   &\ \ if   & \  \  x \leq a
\\
\frac{x-b}{a-b}    &\ \ if   & \  \  a < x \leq b
\\
0   &\ \ if   & \  \  x >b
\end{array}
\right. }
}

\usage{
S(a, b)
}

\arguments{
  \item{a}{
The first parameter of fuzzy set \eqn{S(a,b)}. For better understanding, try to draw the above membership function.
}
  \item{b}{
The second parameter of fuzzy set \eqn{S(a,b)}. For better understanding, try to draw the above membership function.
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
This function convert a fuzzy hypothesis to its boundary which is a triangular fuzzy number. Therefore, although the input of this function is not triangular fuzzy number, but its output is triangular.
}

\references{
Parchami, A., Taheri, S. M., and Mashinchi, M. (2010). Fuzzy p-value in testing fuzzy hypotheses with crisp data. Statistical Papers 51: 209-226.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2012). Testing fuzzy hypotheses based on vague observations: a p-value approach. Statistical Papers 53: 469-484.
}

%% \author{
%% Abbas Parchami }

\examples{
# Introducing the fuzzy alternative hypothesis H1
H1 = S(1275,1325)

## The function is currently defined as
function (a, b) 
(TriangularFuzzyNumber(a, a, b))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fuzzy p-value }
\keyword{ Fuzzy significance level }
\keyword{ Fuzzy data }
\keyword{ Fuzzy hypothesis }
\keyword{ Zadehs extension principle }
\keyword{ Package "FuzzyNumbers" }
