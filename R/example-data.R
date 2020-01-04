#### Functions for generating example data
#### 

### An example with two datasets generated from the same copula, but with different 
### margins.  The file naming is vague because we don't want to give away the game when
### we use it in an example for a talk or tutorial.
exampdata_2d <- function(cor=1/sqrt(2), df=Inf)
{
    ## Default is a normal copula with correlation == 1/sqrt(2) (because pt(df=Inf) is 
    ## the same as the normal distribution)
    cop <- copula::ellipCopula('t', cor, df=df)
    
    cop_obs <- copula::rCopula(500, cop)
    
    ## Example 1 is just the multivariate-t that the copula is based on
    examp1 <- stats::qt(cop_obs, df)
    
    ## Example 2 is a normal distribution for x1 and a chi-squared distribution for x2.
    examp2 <- cop_obs
    examp2[,1] <- stats::qnorm(examp2[,1])
    examp2[,2] <- stats::qchisq(examp2[,2], df=1)
    
    list(X1=examp1, X2=examp2)
}

### Create a correlated extreme value time series by taking maxima over a series of
### multivariate normals
evts <- function(cormat = matrix(c(1,0.5,0.5,1), nrow=2), n = 1200, grp=12) {
    basets <- MASS::mvrnorm(n, rep(0, nrow(cormat)), cormat)
    group <- seq(0, n-1) %/% grp
    x <- 
        t(
            sapply(split(as.data.frame(basets), group),  # preserve columns; split on a matrix collapses them
                   function(d) {
                       apply(as.matrix(d), 2, max)
                   })
        )
    sweep(x, 2, colMeans(x))
}
