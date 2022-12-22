## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(RelDists)
library(EstimationTools)
library(gamlss)

## ---- echo=FALSE, message=F---------------------------------------------------
old_par <- par(mfrow = c(1, 1)) # save previous graphical parameters

## 5^(1/3) = 1.71; 2^(1/3) = 1.26; 0.5^(1/2) = 0.71
paleta <- c("#00004A", "#00A4FF", "#F6F906", "#FF3300", "purple")
curve(dOW(x, 1.71, 3, 0.7), from=0, to=3.5, ylim=c(0,1.6), col=paleta[1],
          ylab=expression(f[' '] (x)), xlab="x", las=1, lwd=2)
curve(dOW(x, 1.26, 3, 0.2), from=0, to=3.5, col=paleta[2], 
      add=TRUE, lwd=2) 
curve(dOW(x,0.71, 2, 1.3), from=0, to=3.5, col=paleta[3], 
      add=TRUE, lwd=2)
curve(dOW(x,1.7,1,0.8), from=0, to=3.5, col=paleta[4], 
      add=TRUE, lwd=2)
legend("topright", legend=c(expression(paste(mu,
       " = ", 1.71,"      " ,sigma," = ",3,"   " ,nu," = ", 0.7)),
                            expression(paste(mu, 
       " = ", 1.26,"      " ,sigma," = ",3,"   " ,nu," = ", 0.2)),
                            expression(paste(mu,
       " = ", 0.71,"      " ,sigma," = ",2,"   " ,nu," = ", 1.3)),
                            expression(paste(mu,
       " = ", 1.7,"        " ,sigma," = ",1,"   " ,nu," = ", 0.8))),
       col=paleta[1:4], bty="n", lwd=2)

## 5^(1/3) = 1.71; 2^(1/3) = 1.26; 0.5^(1/2) = 0.71
curve(pOW(x, 1.71, 3, 0.7), from=0, to=3.5, col=paleta[1],
      ylab=expression(F[' '] (x)), xlab="x", las=1, lwd=2)
curve(pOW(x, 1.26, 3, 0.2), from=0, to=3.5, col=paleta[2], 
      add=TRUE, lwd=2) 
curve(pOW(x,0.71, 2, 1.3), from=0, to=3.5, col=paleta[3], 
      add=TRUE, lwd=2)
curve(pOW(x,1.7,1,0.8), from=0, to=3.5, col=paleta[4], 
      add=TRUE, lwd=2)
legend("bottomright", legend=c(expression(paste(mu, 
       " = ", 1.71,"      " ,sigma," = ",3,"   " ,nu," = ", 0.7)),
                            expression(paste(mu,
       " = ", 1.26,"      " ,sigma," = ",3,"   " ,nu," = ", 0.2)),
                            expression(paste(mu, 
       " = ", 0.71,"      " ,sigma," = ",2,"   " ,nu," = ", 1.3)),
                            expression(paste(mu,
       " = ", 1.7,"        " ,sigma," = ",1,"   " ,nu," = ", 0.8))),
       col=paleta[1:4], bty="n", lwd=2)

par(old_par) # restore previous graphical parameters

## ---- echo=FALSE--------------------------------------------------------------
old_par <- par(mfrow = c(1, 1)) # save previous graphical parameters

par(mgp=c(3,0.7,0))
curve(hOW(x, 1/85, 9, 0.7), from=0, to=69, ylim=c(0,0.035), col=paleta[1],
      ylab=expression(h[' '] (x)), xlab="x", las=1, lwd=2)
curve(hOW(x, 1/100, 0.5, 0.3), from=0, to=70, col=paleta[2], 
      add=TRUE, lwd=2) 
curve(hOW(x, 1/50, 1, 1), from=0, to=70, col=paleta[3], 
      add=TRUE, lwd=2)
curve(hOW(x, 1/45, 8, 0.01), from=0, to=69, col=paleta[4], 
      add=TRUE, lwd=2)
curve(hOW(x, 1/75, -1.5, -0.1), from=0, to=70, col=paleta[5], 
      add=TRUE, lwd=2)
legend("topright", legend=c(expression(paste(mu, 
       " = ", 0.012,"     " ,sigma," = ",9,"        " ,nu," = ", 0.7)),
                            expression(paste(mu, 
       " = ", 0.01,"       " ,sigma," = ",0.5,"     " ,nu," = ", 0.3)),
                            expression(paste(mu, 
       " = ", 0.02,"       " ,sigma," = ",1,"        " ,nu," = ", 1)),
                            expression(paste(mu, 
       " = ", 0.0222,"  " ,sigma," = ",8,"         " ,nu," = ", 0.01)),
                            expression(paste(mu, 
       " = ", 0.0133,"  " ,sigma," = ",-1.5,"   " ,nu," = ", -0.1))), 
       col=paleta, bty="n", lwd=2)

par(old_par) # restore previous graphical parameters

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.height=6------------------
old_par <- par(mfrow = c(1, 1)) # save previous graphical parameters

hyp <- function(x) 1/x

library(viridis)
col1 <- magma(18)[15]
col2 <- magma(18)[12]
col3 <- magma(18)[7]
col4 <- magma(18)[10]
col5 <- magma(18)[1]
col6 <- "blue"

par(mfrow=c(1,1), mar=c(5,5,1,1), mai=c(1,1,0.2,0.2), 
    mgp=c(3.6,1.1,0))
x <- seq(-3.5, 3.5, length.out = 500)
y <- hyp(x)
plot(x, y, type="l", xlim=c(-3,3), ylim=c(-3,3),
     xlab=expression(sigma), ylab=expression(nu), las=1)

# Increasing zone
x_inc <- x[which(x > 1)]
y_inc <- hyp(x_inc)
polygon(x = c(1, 1, x_inc, x_inc[length(x_inc)]), border = 1,
        y = c(3.5, 1, y_inc, 3.5), col=col1)

# Unimodal zone
x_unim1 <- x[which(x < 1 & x > 1/3.5)]
y_unim1 <- hyp(x_unim1)
polygon(x = c(x_unim1, 1, 1, 1/3.5), border = 1,
        y = c(y_unim1, 1, 3.5, 3.5), col=col2)

x_unim2 <- x[which(x < -1/3.5)]
y_unim2 <- hyp(x_unim2)
polygon(x = c(x_unim2, -1/3.5, -3.5, -3.5), border = 1,
        y = c(y_unim2, -3.5, -3.5, -1/3.5), col=col3)

# Bathtub zone
polygon(x = c(1, x_inc, 3.5, 3.5, 1), border = 1,
        y = c(1, y_inc, 1/3.5, 0, 0), col=col4)

# Decreasing zone
polygon(x = c(x_unim1, 1, 1, 0, 0), border = 1,
        y = c(y_unim1, 1, 0, 0, 3.5), col=col6)
polygon(x = c(x_unim2, -1/3.5, 0, 0, -3.5, -3.5),
        y = c(y_unim2, -3.5, -3.5, 0, 0, -1/3.5), col=col5)
box()
abline(h=0); abline(v=0)
legend("topleft", legend=c("Increasing", "Unimodal 1", 
                           "Unimodal 2", "Bathtub", 
                           "Decreasing 1", "Decreasing 2"),
       col=1, pch=21, pt.cex=2.5, pt.bg=c(col1,col2,col3,col4,col6,col5), 
       y.intersp=1.2)

par(old_par) # restore previous graphical parameters

## -----------------------------------------------------------------------------
data("equipment")
my_initial_guess <- initValuesOW(formula = equipment ~ 1)

summary(my_initial_guess)

## -----------------------------------------------------------------------------
old_par <- par(mfrow = c(1, 1)) # save previous graphical parameters

par(mar = c(3.7, 3.7, 1, 10), mgp = c(2.5, 1, 0))
plot(my_initial_guess, las = 1)
legend.HazardShape(x = 1.07, y = 1.04, xpd = TRUE)

par(old_par) # restore previous graphical parameters

## -----------------------------------------------------------------------------
# Custom search region
myvalues <- list(sigma = "all(sigma > 1)",
                 nu = "all(nu < 1/sigma)")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# gamlss set up
con.out <-gamlss.control(n.cyc = 300, trace=TRUE)
myOW <- myOW_region(family = OW(sigma.link='identity'),
                    valid.values = myvalues, initVal = my_initial_guess)

param_ee <- gamlss(equipment ~ 1, sigma.fo = ~ 1, nu.fo = ~ 1, 
                   sigma.start = 5, nu.start = 0.1,
                   control = con.out, family = myOW)
summary(param_ee)

## ---- echo=FALSE, include=FALSE-----------------------------------------------
mu1 <- exp(coef(param_ee, what = 'mu'))
sigma1 <- coef(param_ee, what = 'sigma')
nu1 <- exp(coef(param_ee, what = 'nu'))

mymu1 <- formatC(mu1, format = "e", digits = 2)
mysigma1 <- round(sigma1, 4)
mynu1 <- round(nu1, 4)

## -----------------------------------------------------------------------------
# Do not forget to load 'RelDists' package
data("mice")
init_vals <- initValuesOW(formula = mice ~ 1)

summary(init_vals)

## -----------------------------------------------------------------------------
old_par <- par(mfrow = c(1, 1)) # save previous graphical parameters

par(mar = c(3.7, 3.7, 1, 10), mgp = c(2.5, 1, 0))
plot(init_vals, las = 1)
legend.HazardShape(x = 1.07, y = 1.04, xpd = TRUE)

par(old_par) # restore previous graphical parameters

## ---- message=FALSE, warning=FALSE, message=FALSE-----------------------------
# gamlss set up
myOW <- myOW_region(initVal = init_vals)

# Custom search region
# Do not forget to load 'gamlss' library
param_mm <- gamlss(mice ~ 1, sigma.fo = ~ 1, nu.fo = ~ 1,
                   sigma.start = 2, nu.start = 6,
                   control = con.out,
                   family = myOW)
summary(param_mm)

## ---- echo=FALSE, include=FALSE-----------------------------------------------
mu2 <- exp(coef(param_mm, what = 'mu'))
sigma2 <- coef(param_mm, what = 'sigma')
nu2 <- exp(coef(param_mm, what = 'nu'))

mymu2 <- formatC(mu2, format = "e", digits = 2)
mysigma2 <- round(exp(sigma2), 4)
mynu2 <- round(nu2, 4)

