\name{miss_cell}
\alias{miss_cell}
\title{
missing values Cell FUN
}
\description{
For counting the number of missing values in each cell.
}
\usage{
miss_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
          pct = FALSE, digits = 0, prefix='', suffix='')
}
\arguments{
  \item{x}{
The x variable
}
  \item{y}{
NOT USED
}
  \item{z}{
NOT USED
}
  \item{w}{
NOT USED (The number of missing will not be weighted!).
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
NOT USED
}
  \item{pct}{
Logical asking whatever to draw absolute or relative frequency of missing values.
}
  \item{digits}{
Integer indicating the number of decimal places.
}
  \item{prefix}{
Free text added in each cell bevor results.
}
  \item{suffix}{
Free text added in each cell after results.
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
d$height[round(runif(250,1,1000))]<- NA
d$weight[round(runif(25 ,1,1000))]<- NA
tabular.ade(x_vars=c('height', 'weight'), xname=c('Height [m]','Weight [kg]'),
        cols=c('sex','decades','ALL'), cnames=c('Gender', 'Age decades'),
        data=d, FUN=miss_cell, prefix='Miss:')
}
\keyword{ missings }
