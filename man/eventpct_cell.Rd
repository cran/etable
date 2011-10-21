\name{eventpct_cell}
\alias{eventpct_cell}
\title{
Factor level frequencies Cell FUN.
}
\description{
For calculating frequencies or proportions of a certain level from factor x.
}
\usage{
      eventpct_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
      digits=1, digits2=0, event=2, type=1)
}
\arguments{
\item{x}{
The factor x for calculations
}
\item{y}{
NOT USED
}
\item{z}{
NOT USED
}
\item{w}{
Weights for x factor. Only if calculating weigted frequences.
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
Integer indicating the number of decimal places (percentages)
}
\item{digits2}{
Integer indicating the number of decimal places (N, needed if N is not integer because of weighting)
}
\item{event}{
The Number of factor level to calculate frequencies. from 1 to nlevels(x)
}
\item{type}{
Type of representation, one of following.
\itemize{
      \item{1, pct (n) }
      \item{2, n (pct) }
      \item{3, pct }
      \item{4, n }
      }
}
}
\author{
ADES <ades-s@web.de>
}
\examples{
sex     <- factor(rbinom(1000, 1, 0.4), labels=c('Men', 'Women'))
event   <- factor(rbinom(1000, 1, 0.1), labels=c('no',  'yes'))
decades <- rbinom(1000, 3, 0.5)
decades <- factor(decades, labels=c('[35,45)','[45,55)','[55,65)','[65,75)'))
d<-data.frame(sex, decades, event)
tabular.ade(x_vars=c('event'), xname=c('Event'),
   rows=c('sex','ALL'), rnames=c('Gender'),
   cols=c('decades', 'ALL'),   cnames=c('Age decades'),
   data=d, FUN=eventpct_cell)
}
\keyword{ percentages }
\keyword{ frequency }
