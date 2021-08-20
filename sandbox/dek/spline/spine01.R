# What's best: use df or knots?
f <- system.file("extdata/R6903548_029.nc", package="oceInspect")
d <- oce::read.oce(f)
# Using just first column
SA <- d[["SA"]][,1]
CT <- d[["CT"]][,1]
p <- d[["pressure"]][,1]
ok <- is.finite(SA) & is.finite(CT) & is.finite(p)
summary(ok)
par(mar=c(3,3,1,1), mgp=c(2,0.7,0))
plot(SA, CT, pch=20, cex=1, col=gray(0.66, 0.9))

ps <- seq(0, max(p, na.rm=TRUE), 1)
n <- length(SA)
col <- 1
for (ndf in c(1,5, 10)) {
    SAs <- predict(smooth.spline(p, SA, df=n/ndf), ps)$y
    CTs <- predict(smooth.spline(p, CT, df=n/ndf), ps)$y
    lines(SAs, CTs, lwd=2, col=col)
    col <- col + 1
}

