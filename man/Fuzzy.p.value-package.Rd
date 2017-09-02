\name{Fuzzy.p.value-package}
\alias{Fuzzy.p.value-package}
\alias{Fuzzy.p.value}
\docType{package}
\title{
Computing Fuzzy p-Value}

\description{
Statistical testing hypotheses has an important rule for making decision in practical and applied problems. In traditional testing methods, data, parameters, hypotheses and other elements of problem are consider crisp. 
But in applied sciences such as economics, agriculture and social sciences, it may be confront with vague definitions and fuzzy concepts. In such situations, the classical methods can not solve the vague test and they need to generalized for using in fuzzy environments. 
The vagueness entrance in testing hypotheses problem can be done via data or/and hypotheses. Therefore, the following three major problems can be usually considered for a fuzzy environment: 
(1) testing crisp hypotheses based on fuzzy data, 
(2) testing fuzzy hypotheses based on crisp data, and 
(3) testing fuzzy hypotheses based on fuzzy data.
Similar to the classical testing hypotheses, one can consider different procedure methods for solving the above mentioned problems such as Neyman-Pearson, Bayes, likelihood ratio, minimax and p-value. Computing Fuzzy p-Value package, i.e. \code{Fuzzy.p.value} package, is an open source (LGPL 3) package for R which investigate on the above three problems on the basis of fuzzy p-value approach. 
All formulas and given examples are match with Springer Chapter book (2016) to easily show the performance of the proposed methods.
}

\details{
In this package, We consider only two distributions Normal and Poisson as two suitable candidates from the class of all continues and discrete distributions. This program extendable for other distributions which users can do it if needed. 
}

\author{
Abbas Parchami

Maintainer: \packageMaintainer{Fuzzy.p.value}
}

\references{
Filzmoser, P., and Viertl, R. (2004). Testing hypotheses with fuzzy data: the fuzzy p-value. Metrika 59: 21-29.

Holena, M. (2004). Fuzzy hypotheses testing in a framework of fuzzy logic. Fuzzy Sets and Systems 145: 229-252.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2010). Fuzzy p-value in testing fuzzy hypotheses with crisp data. Statistical Papers 51: 209-226.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2012). Testing fuzzy hypotheses based on vague observations: a p-value approach. Statistical Papers 53: 469-484.

Wang, X., Kerre, E. E. (2001). Reasonable properties for the ordering of fuzzy quantities (II). Fuzzy Sets and Systems 118: 387-405.

Yuan, Y. (1991). Criteria for evaluating fuzzy ranking methods. Fuzzy Sets Syst 43: 139-157.
}

\keyword{ Fuzzy p-value }
\keyword{ Fuzzy significance level }
\keyword{ Fuzzy data }
\keyword{ Fuzzy hypothesis }
\keyword{ Testing hypotheses }
\keyword{ Fuzzy ranking method }

\seealso{
FuzzyNumbers
}

%%\examples{
%%~~ simple examples of the most important functions ~~
%%}
