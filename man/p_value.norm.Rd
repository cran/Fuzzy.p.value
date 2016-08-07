\name{p_value.norm}
\alias{p_value.norm}

\title{
Testing hypotheses based on fuzzy p-value for Normal distribution
}
\description{
Function \code{p_value.norm} can draw the membership function of fuzzy p-value for the following three major problems which can be usually considered for the following tests in a fuzzy environment: 
(1) testing crisp hypotheses based on fuzzy data, 
(2) testing fuzzy hypotheses based on crisp data, and 
(3) testing fuzzy hypotheses based on fuzzy data.  
Also, one can consider a fuzzy significance of level for test by function \code{p_value.norm}. 
It must be noted that function \code{p_value.norm}  is applied when the test statistics has Normal distribution. This idea can be extended for other test statistics which has other continue distribution. 
All needed formulas are presented in  (Parchami and Mashinchi, 2016) and the considered examples are match with this reference.
}

\usage{
p_value.norm(kind, H0, H1, t, s.d, n, sig)
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
  \item{s.d}{
The standard deviation of the normal data.
}
  \item{n}{
Sample size
}
  \item{sig}{
The significance of level for the test which must introduce only via functions \code{T} and \code{Tr}. Therefore, if it is precise (crisp) it must be indicated by special triangular or trapezoidal fuzzy numbers, for example such as \eqn{sig=Tr(.05, .05, .05, .05)}
}
}

\details{
In this package, the users must introduce \eqn{H0}, \eqn{H1}, \eqn{t} and significance level only by fuzzy numbers \code{T}, \code{Tr}, \code{S} or \code{B}. Therefore, if each of them are precise (crisp), user must introduce it by special fuzzy sets (i.e. by indicator functions). 
For example the crisp null hypothesis "\eqn{H_0: \ \mu > 0}" must be introduced by \eqn{H0 = B(0,0)} to function \code{p_value.norm}.
For another example, if the observed value of test statistics is crisp number \eqn{t = 4.1}, it must only be introduced by \eqn{t = T(4.1,4.1,4.1)} or \eqn{t = Tr(4.1,4.1,4.1,4.1)} to the function \code{p_value.norm}.
}

\value{
The function \code{p_value.norm} is constructed for hypotheses testing in fuzzy environment where the test statistics has Normal distributions and the parameter which discussed by two hypotheses is the mean of Normal. 
The major target and the duty of function \code{p_value.norm} is:  
(1) draw the membership functions of p-value and significance level for a Normal test,  
(2) compute numerical measures of \eqn{\Delta_{SP}} and \eqn{\Delta_{PS}} (see Parchami and Mashinchi (2016) for formulas), 
(3) decide to accept null hypothesis or accept alternative hypothesis in the test, and finally 
(4) compute the degree of acceptance for the accepted hypothesis for a Normal test.
}

\references{
Filzmoser, P., and Viertl, R. (2004). Testing hypotheses with fuzzy data: the fuzzy p-value. Metrika 59: 21-29.

Holena, M. (2004). Fuzzy hypotheses testing in a framework of fuzzy logic. Fuzzy Sets and Systems 145: 229-252.

Parchami, A., and Mashinchi, M. (2016). Testing hypotheses based on fuzzy p-value in fuzzy environment, Title of book: Fuzzy Statistical Decision Making: Theory and Applications, C. Kahraman and O. Kabak (eds.), Springer, Accepted.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2010). Fuzzy p-value in testing fuzzy hypotheses with crisp data. Statistical Papers 51: 209-226.

Parchami, A., Taheri, S. M., and Mashinchi, M. (2012). Testing fuzzy hypotheses based on vague observations: a p-value approach. Statistical Papers 53: 469-484.

Wang, X., Kerre, E. E. (2001). Reasonable properties for the ordering of fuzzy quantities (II). Fuzzy Sets and Systems 118: 387-405.

Yuan, Y. (1991). Criteria for evaluating fuzzy ranking methods. Fuzzy Sets Syst 43: 139-157.
}

 \author{
 Abbas Parchami }

\examples{
# Solving Example 1 from (Parchami and Mashinchi, 2016)
p_value.norm( kind=2, H0=T(1300,1300,1300), H1=B(1300,1300), t=T(1288,1313,1331), s.d=30, 
n=10, sig=T(0,0.05,0.1) )

# Solving Example 2 from (Parchami and Mashinchi, 2016)
p_value.norm( kind=0, H0=T(1300,1300,1300), H1=B(1300,1300), t=T(1300,1313,1321), s.d=30, 
n=10, sig=T(0,0.15,0.3) )

# Solving Example 4 from (Parchami and Mashinchi, 2016)
H0 = T(1275,1300,1325) #  Fuzzy null hypothesis
H1 = B(1275,1325) #  Fuzzy althernative hypothesis
t = T(1315,1327,1342) #  Fuzzy observed value for the test statistics
sig = T(0.0,0.15,0.3) #  Fuzzy significance level
p_value.norm( kind=2, H0, H1, t, s.d=120, n=36, sig)

# Solving Example 6 from (Parchami and Mashinchi, 2016)
H0 = T(1275,1300,1325)
H1 = B(1275,1325)
t = T(1327,1327,1327) #  Crisp observed value for the test statistics
sig = T(0, .05, .1)
p_value.norm( kind=2, H0, H1, t, s.d=120, n=36, sig)


## The function is currently defined as
function (kind, H0, H1, t, s.d, n, sig) 
{
    alpha_L = seq(0, 1, 0.01)
    alpha_U = seq(1, 0, -0.01)
    if (kind == 1) {
        p_L = pnorm(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"], s.d/(n^0.5))
        p_U = pnorm(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"], s.d/(n^0.5))
    }
    else if (kind == 2) {
        p_L = 1 - pnorm(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"], s.d/(n^0.5))
        p_U = 1 - pnorm(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"], s.d/(n^0.5))
    }
    else if (supp(t)[1] >= supp(H0)[2]) {
        p_L = 2 * (1 - pnorm(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"], s.d/(n^0.5)))
        p_U = 2 * (1 - pnorm(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"], s.d/(n^0.5)))
    }
    else if (supp(t)[2] <= supp(H0)[1]) {
        p_L = 2 * pnorm(alphacut(t, alpha_L)[, "U"], alphacut(H0, 
            alpha_L)[, "L"], s.d/(n^0.5))
        p_U = 2 * pnorm(alphacut(t, alpha_U)[, "L"], alphacut(H0, 
            alpha_U)[, "U"], s.d/(n^0.5))
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
    Int_1 <- function(delta) (alphacut(sig, delta)[, "U"] - p_L[(100 * 
        delta) + 1]) * ifelse(alphacut(sig, delta)[, "U"] - p_L[(100 * 
        delta) + 1] >= 0, 1, 0)
    Int_2 <- function(delta) (alphacut(sig, delta)[, "L"] - p_U[(100 * 
        delta) + 1]) * ifelse(alphacut(sig, delta)[, "L"] - p_U[(100 * 
        delta) + 1] >= 0, 1, 0)
    Delta_SP = area(Int_1, 0, 1) + area(Int_2, 0, 1)
    print("Delta_SP = ")
    print(Delta_SP)
    Int_3 <- function(delta) (p_U[(100 * delta) + 1] - alphacut(sig, 
        delta)[, "L"]) * ifelse(p_U[(100 * delta) + 1] - alphacut(sig, 
        delta)[, "L"] >= 0, 1, 0)
    Int_4 <- function(delta) (p_L[(100 * delta) + 1] - alphacut(sig, 
        delta)[, "U"]) * ifelse(p_L[(100 * delta) + 1] - alphacut(sig, 
        delta)[, "U"] >= 0, 1, 0)
    Delta_PS = area(Int_3, 0, 1) + area(Int_4, 0, 1)
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
\keyword{ Normal test }
\keyword{ Package "FuzzyNumbers" }
\keyword{ Fuzzy ranking method }
