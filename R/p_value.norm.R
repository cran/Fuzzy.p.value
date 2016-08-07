p_value.norm <-
function(kind, H0, H1, t, s.d, n, sig)  {
alpha_L=seq(0,1, .01)
alpha_U=seq(1,0,-.01)

if  ( kind == 1 )  # For H1 means approximately Smaller than ...
{
p_L = pnorm( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"]  , s.d/(n^0.5))
p_U = pnorm( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"]  , s.d/(n^0.5))
} 
 else if  ( kind == 2 ) # For H1 means approximately Bigger than ...
{
p_L = 1-pnorm( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"]  , s.d/(n^0.5))
p_U = 1-pnorm( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"]  , s.d/(n^0.5))
} 
 else if  ( supp(t)[1] >= supp(H0)[2] ) # For H1 means approximately equal to ... and t_l >= m_r
{
p_L = 2*(1-pnorm( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"]  , s.d/(n^0.5)))
p_U = 2*(1-pnorm( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"]  , s.d/(n^0.5)))
} 
 else if  ( supp(t)[2] <= supp(H0)[1] ) # For H1 means approximately equal to ... and t_r <= m_l
{
p_L = 2* pnorm( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"]  , s.d/(n^0.5))
p_U = 2* pnorm( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"]  , s.d/(n^0.5))
} 
 else  # For H1 means approximately equal to ... and  m_l<t< m_r
{
return("The fuzzy p-value can not defined for this example, since the fuzziness of the problem is very high.  
 This case may be accured for the testing equality fuzzy null hypothesis, against the non-equality fuzzy althenative hypothesis")
}

alpha = cbind(alpha_L,alpha_U);
p = cbind(p_L,p_U);
plot(p,alpha, type='l', lwd=3, col=1, xlim=c(-0.04,1.02) )
s1 = supp(sig)[1]
s2 = core(sig)[1]
s3 = core(sig)[1]
s4 = supp(sig)[2]
lines( c(0,s1,s2,s3,s4,1), c(0,0,1,1,0,0), type='l', lty=3, lwd=2, col=2 )
#plot( sig , lty=3, lwd=2, col=2, add=TRUE )
legend( "topright", c("Fuzzy p-value", "Significance level"), col = c(1,2), text.col = 1, lwd = c(3,2), lty = c(1,3) )


Int_1 <- function(delta)   ( alphacut(sig, delta)[,"U"] - p_L[(100*delta)+1] ) * ifelse(  alphacut(sig, delta)[,"U"] - p_L[(100*delta)+1]  >= 0  , 1, 0); 
Int_2 <- function(delta)   ( alphacut(sig, delta)[,"L"] - p_U[(100*delta)+1] ) * ifelse(  alphacut(sig, delta)[,"L"] - p_U[(100*delta)+1]  >= 0  , 1, 0); 

Delta_SP= area(Int_1, 0, 1)  +  area(Int_2, 0, 1);
print("Delta_SP = ")
print(Delta_SP)

Int_3 <- function(delta)   ( p_U[(100*delta)+1]  -  alphacut(sig, delta)[,"L"]  ) * ifelse(  p_U[(100*delta)+1]  -  alphacut(sig, delta)[,"L"]  >= 0  , 1, 0); 
Int_4 <- function(delta)   ( p_L[(100*delta)+1]  -  alphacut(sig, delta)[,"U"]  ) * ifelse(  p_L[(100*delta)+1]  -  alphacut(sig, delta)[,"U"]  >= 0  , 1, 0); 

Delta_PS= area(Int_3, 0, 1)  +  area(Int_4, 0, 1);
print("Delta_PS = ")
print(Delta_PS)

Degree_P_biger_than_S= Delta_PS / (Delta_PS + Delta_SP);
Degree_S_biger_than_P = 1- Degree_P_biger_than_S;

if (Degree_P_biger_than_S >= Degree_S_biger_than_P) 
{
a = "The null hypothesis (H0) is accepted with degree D(P>S)="
b = round(Degree_P_biger_than_S, 4)
c = ", at  the considered significance level."
noquote( sprintf("%s %s %s ", a, b, c))
}
 else {if (Degree_P_biger_than_S < Degree_S_biger_than_P) 
{
a = "The althernative hypothesis (H1) is accepted with degree D(S>P)="
b = round(Degree_S_biger_than_P, 4)
c = ", at  the considered significance level."
noquote( sprintf("%s %s %s ", a, b, c))
}
         else {return( noquote( paste0("Impossible case" ) ) )} }
}
