
#Golden Section Search
#find maximum of f on (0,1)
fct = function(x){
  return((4/3)*log(1+x) - x)
}

source("C:/Users/MaryLena/Desktop/FALL2019/STAT 6324/golden_section.R")

golden.section.search(fct,0,1,.001)

library(stats4)
optimize(fct, c(0,1))


#Bisection method

f1 = function(x){return(1 + x^2 +x^3)}
dfdx = function(x){2*x + 3*x^2}


Bisection = function(f, interval, ertol = 0.001){
  e = Inf
  while (abs(e) > ertol){
  x = 0.5*(interval[2] + interval[1])
  if (f(x)>0){interval[2] = x}
  else{interval[1] = x}
  e = f(x)
  }
  return(x)
}

Bisection(dfdx, c(-100,100))

#Newton and Secant method
Newton = function(input, dfdx,ertol, x){
e = Inf
while(abs(e) > ertol){
  if (dfdx(x)==0){return("Error: derivative equal to zero")}
  x = x - input(x)/dfdx(x)
  e = input(x)
}
  return(x)
}

Secant_noil = function(input,ertol=0.001, x){
  e = Inf
  while(abs(e) > ertol){
    x_n = x[2] - (x[2] - x[1])/(input(x[2])-input(x[1]))*input(x[2])
    if (x_n > 0){x[2] = x_n}
    else{x[1] = x_n}
    e = min(input(x))
  }
  mat = data.frame(x, F_x = input(x))
  return(mat[mat$F_x == min(mat$F_x),])
}

Secant_il = function(input,ertol=0.001, x){
  e = Inf
  change.pos = 0
  change.neg = 0
  while(abs(e) > ertol){
    if (change.pos >=2){x_n = x[2] - (x[2] - x[1])/(input(x[2])-0.5*input(x[1]))*input(x[2])}
    else if (change.neg >= 2){x_n = x[2] - (x[2] - x[1])/(0.5*input(x[2])-input(x[1]))*0.5*input(x[2])}
    else{x_n = x[2] - (x[2] - x[1])/(input(x[2])-input(x[1]))*input(x[2])}
    if (x_n > 0){x[2] = x_n; change.pos = change.pos +1; change.neg = 0}
    else{x[1] = x_n; change.neg = change.neg + 1; change.pos =0}
    e = min(input(x))
  }
  mat = data.frame(x, F_x = input(x))
  return(mat[mat$F_x == min(mat$F_x),])
}



Newton(f1, dfdx, error, x_init)

system.time(Newton(f1, dfdx, 0.001,5))

f2 = Vectorize(f)
Secant_noil(f2, error, c(-5,5))
system.time(Secant_noil(f2,  c(-5,5)))
system.time(Secant_noil(f2,  c(-5,5)))


#Nelder-Mead optimization

install.packages("stats")
library(stats)

func = function(x){
  a = x[1]^2 + x[2]^2 + x[3]^2
  return(a)
}

optim(c(100,-50,25), func, method="Nelder-Mead")

