\name{B}
\alias{B}

\title{
Class of linear fuzzy sets to construct concept "approximately bigger"
}

\description{
The are several famous fuzzy sets which have meaningful means and are user friendly for introducing. 
Several linear functions are constructed in this package for easily inspiration to the fuzzy concepts \eqn{\ll approximately smaller \gg} , \eqn{\ll approximately bigger \gg} and  \eqn{\ll approximately equal \gg}.
For example, function \eqn{B} is one of them which users can use it to construct the fuzzy concept \eqn{\ll approximately bigger \gg} in fuzzy hypotheses. 
As presented bellow, the membership function of fuzzy set \code{B} has two parameters \eqn{a} and  \eqn{b}:
\deqn{ B(a,b)(x)=\left\{
\begin{array}{lcc}
1   &\ \ if   & \  \  b \leq x
\\
\frac{x-a}{b-a}    &\ \ if   & \  \  a \leq x <b
\\
0   &\ \ if   & \  \  x <a
\end{array}
\right. }
}

\usage{
B(a, b)
}

\arguments{
  \item{a}{
The first parameter of fuzzy set \eqn{B(a,b)}. For better understanding, try to draw the above membership function.
}
  \item{b}{
The second parameter of fuzzy set \eqn{B(a,b)}. For better understanding, try to draw the above membership function.
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
# Introducing the fuzzy null hypothesis H0
H0 = B(1275,1325)


## The function is currently defined as
function (a, b) 
(TriangularFuzzyNumber(a, b, b))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fuzzy p-value }
\keyword{ Fuzzy significance level }
\keyword{ Fuzzy data }
\keyword{ Fuzzy hypothesis }
\keyword{ Zadehs extension principle }
\keyword{ Package FuzzyNumbers }
