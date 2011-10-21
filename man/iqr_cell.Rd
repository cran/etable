\name{iqr_cell}
\alias{iqr_cell}
\title{
Median IQR Cell FUN.
}
\description{
For calculate median and interquartile range. (weighting is possible)
}
\usage{
          iqr_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
          digits = 3, add_n=FALSE)
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
  \item{add_n}{
Logical asking whether to draw N for each cell.
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
   rows=c('sex','ALL'), rnames=c('Gender'),
   cols=c('decades'),   cnames=c('Age decades'),
   data=d, FUN=iqr_cell, add_n=TRUE)
}
\keyword{ median }
