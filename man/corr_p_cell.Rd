\name{corr_p_cell}
\alias{corr_p_cell}
\title{
Correlation Cell FUN
}
\description{
To calculating Pearson product-moment correlation coefficient.
}
\usage{
       corr_p_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
       digits = 3)
}
\arguments{
  \item{x}{
The x variable
}
  \item{y}{
The y variable
}
  \item{z}{
NOT USED
}
  \item{w}{
Weights for x and y variable.
}
\item{cell_ids}{
Index vector for selecting values in cell.
}
\item{row_ids}{
NOT USED
}
\item{col_ids}{
NOT USED
}
\item{vnames}{
NOT USED
}
\item{vars}{
NOT USED
}
  \item{n_min}{
Minimum n in the cell for useful calculation. Cells with n<n_min deliver no output.
}
\item{digits}{
Integer indicating the number of significant digits.
}
}
\author{
ADES <ades-s@web.de>
}
\examples{
sex     <- factor(rbinom(1000, 1, 0.4),  labels=c('Men', 'Women'))
height  <- rnorm(1000, mean=1.70, sd=0.1)
weight  <- rnorm(1000, mean=70, sd=5)
bmi     <- weight/height^2
d<-data.frame(sex, bmi, height, weight)
tabular.ade(x_vars=c('bmi','height','weight'), xname=c('BMI','Height','Weight'),
            y_vars=c('bmi','height','weight'), yname=c('BMI','Height','Weight'),
            rows=c('sex','ALL'), rnames=c('Gender'), data=d, FUN=corr_p_cell)
}
\keyword{ correlation }
