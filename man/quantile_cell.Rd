\name{quantile_cell}
\alias{quantile_cell}
\title{
quantile Cell FUN
}
\description{
For calculating sample or weighted quantiles
}
\usage{
quantile_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
              digits = 3, probs = 0.5, plabels=FALSE)
}
\arguments{
  \item{x}{
The x variable for calculations
}
  \item{y}{
NOT USED
}
  \item{z}{
NOT USED
}
  \item{w}{
Weights for x variable.
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
  \item{probs}{
A single or a vector of numeric probabilities for sample quantile with values in [0,1].
}
  \item{plabels}{
Logical asking whether to label the quantile in the cell or only draw the value.
}
}
\author{
ADES <ades-s@web.de>
}
\examples{
sex     <- factor(rbinom(1000, 1, 0.4),  labels=c('Men', 'Women'))
height  <- rnorm(1000, mean=1.66, sd=0.1)
height[which(sex=='Men')]<-height[which(sex=='Men')]+0.1
weight  <- rnorm(1000, mean=70, sd=5)
decades <- rbinom(1000, 3, 0.5)
decades <- factor(decades, labels=c('[35,45)','[45,55)','[55,65)','[65,75)'))
d<-data.frame(sex, decades, height, weight)
tabular.ade(x_vars=c('height', 'weight'), xname=c('Height [m]','Weight [kg]'),
   rows=c('sex',     'ALL'), rnames=c('Gender'),
   cols=c('decades', 'ALL'), cnames=c('Age decades'),
   data=d, FUN=quantile_cell, probs = 0.99)
}
\keyword{ quantile }
