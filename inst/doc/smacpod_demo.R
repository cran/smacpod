## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE, fig.height = 6, fig.width = 6----------------------------
# load packages
library(smacpod)
data(grave)
# pch changes point symbol
plot(grave, pch = c(1, 19), main = "grave locations", xlab = "x", ylab = "y")

## ---- echo = FALSE------------------------------------------------------------
set.seed(1)

## ---- fig.height = 6, fig.width = 6-------------------------------------------
# identify affected observations
aff <- (grave$marks == "affected")
# estimated spatial density function for affected observations
f <- spdensity(grave[aff,])
# plot spatial density function of affected
plot(f, "spatial density of affected graves")

## ---- fig.width=6, fig.height=6-----------------------------------------------
rhat <- logrr(grave, case = "affected")
plot(rhat, main = "log relative risk of affected versus unaffected graves")
contour(rhat, add = TRUE)

## ---- fig.width=6, fig.height=6-----------------------------------------------
# construct tolerance envelopes for logrr
renv = logrr(grave, case = "affected", nsim = 99, level = 0.90)
# print info for tolerance envelopes
renv
# plot areas with rhat outside of envelopes
plot(renv)

## ---- fig.width=6, fig.height=6-----------------------------------------------
# a better color scale (in my opinion)
# making it easier to distinguish the clusters of cases relative
# to controls (red) and vice versa (blue)
grad = gradient.color.scale(min(renv$v, na.rm = TRUE), max(renv$v, na.rm = TRUE))
plot(renv, col = grad$col, breaks = grad$breaks)

## -----------------------------------------------------------------------------
logrr.test(renv)

## ---- fig.width=6, fig.height=6-----------------------------------------------
kd = kdest(grave, case = "affected")
plot(kd, cbind(iso, theo) ~ r, legend = FALSE, main = "")

## ---- fig.width=6, fig.height=6-----------------------------------------------
kdenv <- kdest(grave, case = "affected", nsim = 49,
              level = 0.95)
# print some information about kdenv
print(kdenv)
# determine distances KD(r) outside tolerance envelopes
summary(kdenv)
# plot results
plot(kdenv, ylab = "difference in K functions")
legend("topleft", legend = c("obs", "avg", "max/min env", "95% env"),
       lty = c(1, 2, 1, 2), col = c("black", "red", "darkgrey", "lightgrey"),
       lwd = c(1, 1, 10, 10))

## -----------------------------------------------------------------------------
kdplus.test(kdenv)

## ---- fig.width=6, fig.height=6-----------------------------------------------
scan = spscan.test(grave, nsim = 49, case = "affected")
# print info about scan
scan
# summary of scan test results
summary(scan)
# plot scan test results
plot(scan, chars = c(1, 20), main = "most likely cluster for grave data",
     border = "orange")
# extract most likely and other significant clusters
clusters(scan)

## -----------------------------------------------------------------------------
qnn.test(grave, q = c(3, 5, 7, 9, 11, 13, 15), nsim = 49, case = "affected")

