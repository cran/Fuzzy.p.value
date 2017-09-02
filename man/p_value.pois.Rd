\name{p_value.pois}
\alias{p_value.pois}

\title{
Testing hypotheses based on fuzzy p-value for Poisson distribution
}

\description{
Function \code{p_value.pois} can draw the membership function of fuzzy p-value for the following three major problems which can be usually considered for the following tests in a fuzzy environment: 
(1) testing crisp hypotheses based on fuzzy data, 
(2) testing fuzzy hypotheses based on crisp data, and 
(3) testing fuzzy hypotheses based on fuzzy data.  
Also, one can consider a fuzzy significance of level for each of above tests by function \code{p_value.pois}. 
It must be noted that function \code{p_value.pois}  is applied when the test statistics has Poisson distribution. This idea can be extended for other test statistics which has other discrete distribution. 
All needed formulas are presented in  Springer Chapter book 2016 and the considered examples are match with this reference for the readers.
}

\usage{
p_value.pois(kind, H0, H1, t, n, sig)
}

\arguments{
  \item{kind}{
The kind of testing hypotheses is one of three numbers 0, 1 and 2, which is distinguishable from the form of alternative (fuzzy/non-fuzzy) hypothesis.
Set \eqn{kind = 0}, when the kind of alternative hypothesis H1 is non-equality (i.e., \eqn{\neq}). 
Set \eqn{kind = 1}, when the kind of H1 show "smaller than" for the parameter (i.e., \eqn{<}).
Set \eqn{kind = 2}, when the kind of H1 show "bigger than" for the parameter (i.e., \eqn{>}).
}
  \item{H0}{
The null hypothesis of the test which must introduce only via functions \code{T}, \code{Tr}, \code{S} and \code{B}.
}
  \item{H1}{
The alternative hypothesis of the test which must introduce only via functions \code{T}, \code{Tr}, \code{S} and \code{B}.
}
  \item{t}{
The observed value of the test statistic (the observed value of the mean), which must introduce only via functions \code{T} and \code{Tr}. Note that if it is precise and crisp the user must indicate it by special triangular or trapezoidal fuzzy numbers, for example such as \eqn{t=T(23,23,23)}
}
  \item{n}{
Sample size
}
  \item{sig}{
The significance of level for the test which must introduce only via functions \code{T} and \code{Tr}. Therefore, if it is precise (crisp) it must be indicated by special triangular or trapezoidal fuzzy numbers, for example such as \eqn{sig=T(0.1, 0.1, 0.1)}
}
}

\details{
In this package, the users must introduce \eqn{H0}, \eqn{H1}, \eqn{t} and significance level only by fuzzy numbers \code{T}, \code{Tr}, \code{S} or \code{B}. Therefore, if each of them are precise (crisp), user must introduce it by special fuzzy sets (i.e. by indicator functions). 
For example the crisp null hypothesis "\eqn{H_0: \ \lambda > 5.2}" must be introduced by \eqn{H0 = B(5.2, 5.2)} to function \code{p_value.pois}.
}

\value{
The function \code{p_value.pois} is constructed for hypotheses testing in fuzzy environment where the test statistics has Poisson distributions and the parameter which discussed by two hypotheses is the mean of Poisson. 
The major duty of function \code{p_value.pois} is:  
(1) draw the membership functions of p-value and significance level for a Poisson test, 
(2) compute numerical measures of \eqn{\Delta_{SP}} and \eqn{\Delta_{PS}} (see Parchami and Mashinchi (2016) for formulas), 
(3) decide to accept null hypothesis or accept alternative hypothesis in the test, and finally 
(4) compute the degree of acceptance for the accepted hypothesis for a Poisson test.
}

\references{
Filzmoser, P., and Viertl, R. (2004). Testing hypotheses with fuzzy data: the fuzzy p-value. Metrika 59: 21-29.

Holena, M. (2004). Fuzzy hypotheses testing in a framework of fuzzy logic. Fuzzy Sets and Systems 145: 229-252.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2010). Fuzzy p-value in testing fuzzy hypotheses with crisp data. Statistical Papers 51: 209-226.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2012). Testing fuzzy hypotheses based on vague observations: a p-value approach. Statistical Papers 53: 469-484.

Wang, X., Kerre, E. E. (2001). Reasonable properties for the ordering of fuzzy quantities (II). Fuzzy Sets and Systems 118: 387-405.

Yuan, Y. (1991). Criteria for evaluating fuzzy ranking methods. Fuzzy Sets Syst 43: 139-157.
}

 \author{
 Abbas Parchami }

\examples{
# Solving Example 5 from Springer Chapter book 2016
# Considering the assumptions of this example, it must be note that H0 and H1 are not about 
#   the parameter of the test statistics, and in fact hypotheses must be 12*H0 and 12*H1
H0 = B( 12*2.75 ,  12*3.25 )
H1 = S( 12*2.75 ,  12*3.25 )
t = T( 27, 27, 27)
sig = T(0.0,0.05,0.1)
p_value.pois( kind=1, H0, H1, t, n=12, sig)

# Solving Example 7 from Springer Chapter book 2016
H0 = B( 12*2.75 ,  12*3.25 )
H1 = S( 12*2.75 ,  12*3.25 )
t = T( 24, 27, 30)
sig = T(0.0,0.05,0.1)
p_value.pois( kind=1, H0, H1, t, n=12, sig)


## The function is currently defined as
function (kind, H0, H1, t, n, sig) 
{
    alpha_L = seq(0, 1, 0.01)
    alpha_U = seq(1, 0, -0.01)
    if (kind == 1) {
        p_L = ppois(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"])
        p_U = ppois(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"])
    }
    else if (kind == 2) {
        p_L = 1 - ppois(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"])
        p_U = 1 - ppois(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"])
    }
    else if (supp(t)[1] >= supp(H0)[2]) {
        p_L = 2 * (1 - ppois(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"]))
        p_U = 2 * (1 - ppois(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"]))
    }
    else if (supp(t)[2] <= supp(H0)[1]) {
        p_L = 2 * ppois(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"])
        p_U = 2 * ppois(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"])
    }
    else {
        return("The fuzzy p-value can not defined for this example, since the fuzziness of 
		the problem is very high. This case may be accured for the testing equality 
		fuzzy null hypothesis, against the non-equality fuzzy althenative hypothesis")
    }
    alpha = cbind(alpha_L, alpha_U)
    p = cbind(p_L, p_U)
    plot(p, alpha, type = "l", lwd = 3, col = 1, xlim = c(-0.04, 
        1.02))
     s1 = supp(sig)[1]
     s2 = core(sig)[1]
     s3 = core(sig)[1]
     s4 = supp(sig)[2]
     lines( c(0,s1,s2,s3,s4,1), c(0,0,1,1,0,0), type='l', lty=3, lwd=2, col=2 )
# plot(sig, lty=3, lwd=2, col=2, add=TRUE) # Conflict importing 2 plot from different packages
    legend("topright", c("Fuzzy p-value", "Significance level"), 
        col = c(1, 2), text.col = 1, lwd = c(3, 2), lty = c(1, 
            3))
     if( class(sig) == "numeric" ){
            sig <- TriangularFuzzyNumber(sig, sig, sig)
            }
     P_L = p_L
     P_U = p_U
     knot.n = 100
     S_L = alphacut(sig, round(seq(0, 1, .01), 5))[,"L"]
     S_U = alphacut(sig, round(seq(0, 1, .01), 5))[,"U"]
     Int1 = ( P_U - S_L ) * ( P_U > S_L )
     Int2 = ( P_L - S_U ) * ( P_L > S_U )
     Arz = 1 / (knot.n - 1)  #Arze Mostatilha baraye mohasebe-ye Integral
     Integral1 <- ( sum( Int1 ) - Int1[1]/2 - Int1[length(Int1)]/2 ) *Arz
     Integral2 <- ( sum( Int2 ) - Int2[1]/2 - Int2[length(Int2)]/2 ) *Arz
     Delta_PS = Integral1 + Integral2
     Int3 = ( S_U - P_L ) * ( S_U > P_L )
     Int4 = ( S_L - P_U ) * ( S_L > P_U )
     Integral3 <- ( sum( Int3 ) - Int3[1]/2 - Int3[length(Int3)]/2 ) *Arz
     Integral4 <- ( sum( Int4 ) - Int4[1]/2 - Int4[length(Int4)]/2 ) *Arz
     Delta_SP = Integral3 + Integral4
    print("Delta_SP = ")
    print(Delta_SP)
    print("Delta_PS = ")
    print(Delta_PS)
    Degree_P_biger_than_S = Delta_PS/(Delta_PS + Delta_SP)
    Degree_S_biger_than_P = 1 - Degree_P_biger_than_S
    if (Degree_P_biger_than_S >= Degree_S_biger_than_P) {
        a = "The null hypothesis (H0) is accepted with degree D(P>S)="
        b = round(Degree_P_biger_than_S, 4)
        c = ", at  the considered significance level."
        noquote(sprintf("\%s \%s \%s ", a, b, c))
    }
    else {
        if (Degree_P_biger_than_S < Degree_S_biger_than_P) {
            a = "The althernative hypothesis (H1) is accepted with degree D(S>P)="
            b = round(Degree_S_biger_than_P, 4)
            c = ", at  the considered significance level."
            noquote(sprintf("\%s \%s \%s ", a, b, c))
        }
        else {
            return(noquote(paste0("Impossible case")))
        }
    }
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fuzzy p-value }
\keyword{ Fuzzy significance level }
\keyword{ Fuzzy data }
\keyword{ Fuzzy hypothesis }
\keyword{ Poisson test }
\keyword{ Package "FuzzyNumbers" }
\keyword{ Fuzzy ranking method }
