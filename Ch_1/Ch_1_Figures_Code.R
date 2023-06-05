

set.seed(0)
dat <- data.frame(x=(x=runif(10000, 0, 50)),
                  y=rnorm(10000, 10*x, 100))


## breaks: where you want to compute densities
breaks <- seq(0, max(dat$x), len=5)
dat$section <- cut(dat$x, breaks)


## Get the residuals
dat$res <- residuals(lm(y ~ x, data=dat))

# Compute densities for each section, flip the axes, add means of sections
## Note: densities need to be scaled in relation to section size (2000 here)
dens <- do.call(rbind, lapply(split(dat, dat$section), function(x) {
  d <- density(x$res, n=5000)
  res <- data.frame(x=max(x$x)- d$y*2000, y=d$x+mean(x$y))
  res <- res[order(res$y), ]
  ## Get some data for normal lines as well
  xs <- seq(min(x$res), max(x$res), len=5000)
  res <- rbind(res, data.frame(y=xs + mean(x$y),
                               x=max(x$x) - 2000*dnorm(xs, 0, sd(x$res))))
  res$type <- rep(c("empirical", "normal"), each=5000)
  res
}))
dens$section <- rep(levels(dat$section), each=10000)
