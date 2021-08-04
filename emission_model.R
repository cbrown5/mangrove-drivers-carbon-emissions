# ---------------------------------------------------------------------
# Emissions model
# CJ Brown J Holdorf 2020 
# ----------------------------------------------------------------------

# Opportunity to store carbon 
#for r == d
#where t is the time period
# A1 is initial area
# d is deforestation rate
# r is emission rate
#Cmax is maximum emissions
#s is sequestration rate 

em_mod_rd <- function(t, A1, d, r, Cmax, s = 0){
  x <- ((A1*s*(1-exp(-d*t)))/d) - 
    (A1*Cmax*(1-exp(-d*t) - (d*t*exp(-d*t)))) - 
    (A1*s*t) 
  x*-1 #because we want positive values
}

#for r!= d
em_mod_rnotd <- function(t, A1, d, r, Cmax, s = 0){
  x <- ((A1*s*(1-exp(-d*t)))/d) - #sequestartion with deforesting
    ((A1*Cmax*(d*exp(-r*t) - r*exp(-d*t)+(r-d)))/(r-d)) - 
    #emission from deforesting
    (A1*s*t) #sequestration no deforestation
  x*-1
}


emission_model <- function(t, A1, d, r, Cmax, s = 0){

  stopifnot((length(r) == 1) & (length(d) ==1))  
  if(r==d){
         em_mod_rd(t, A1, d, r, Cmax, s)
    } else {
         em_mod_rnotd(t, A1, d, r, Cmax, s)
    }
  
}