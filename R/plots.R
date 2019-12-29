#### Helper functions for making plots and such

## Make a scatter plot of bivariate data stored as columns in a matrix.  The plot
## is returned as a ggplot2 object, so you can customize theme and display as you like.
scatterplot_mat <- function(pts)
{
    ptsdf <- as.data.frame(pts)
    ggplot2::ggplot(data=ptsdf, ggplot2::aes(x=V1, y=V2)) +
        ggplot2::geom_point() +
        ggplot2::xlab(latex2exp::TeX('X_1')) + ggplot2::ylab(latex2exp::TeX('X_2')) +
        ggplot2::theme_bw()
}

## Todo:  find a way to change the label from "Cor" to "tau"
pairplot_mat <- function(pts)
{
    if(is.null(colnames(pts))) {
        colnames(pts) <- paste0('X', seq(1, ncol(pts)))
    }
    ptsdf <- as.data.frame(pts)
    GGally::ggpairs(ptsdf, upper=list(continuous=GGally::wrap(GGally::ggally_cor, method='kendall'))) + 
        ggplot2::theme_bw()
}
