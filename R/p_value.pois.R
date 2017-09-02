p_value.pois <-
function(kind, H0, H1, t, n, sig)  {
alpha_L=seq(0,1, .01)
alpha_U=seq(1,0,-.01)

if  ( kind == 1 )  # For H1 means approximately Smaller than ...
{
p_L = ppois( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"] )
p_U = ppois( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"] )
} 
 else if  ( kind == 2 ) # For H1 means approximately Bigger than ...
{
p_L = 1-ppois( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"] )
p_U = 1-ppois( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"] )
} 
 else if  ( supp(t)[1] >= supp(H0)[2] ) # For H1 means approximately equal to ... and t_l >= m_r
{
p_L = 2*(1-ppois( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"] ))
p_U = 2*(1-ppois( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"] ))
} 
 else if  ( supp(t)[2] <= supp(H0)[1] ) # For H1 means approximately equal to ... and t_r <= m_l
{
p_L = 2* ppois( alphacut(t, alpha_L)[,"U"] ,  alphacut(H0, alpha_L)[,"L"] )
p_U = 2* ppois( alphacut(t, alpha_U)[,"L"] ,  alphacut(H0, alpha_U)[,"U"] )
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
