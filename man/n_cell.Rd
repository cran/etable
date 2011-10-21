\name{n_cell}
\alias{n_cell}
\title{
Frequency Cell FUN
}
\description{
For calculating relative or absolute frequencies.
}
\usage{
   n_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
          digits=0, type="n")
}
\arguments{
\item{x}{
The x variable (can be easy 1:N if no missings)
}
\item{y}{
NOT USED
}
\item{z}{
NOT USED
}
\item{w}{
Weights for x variable. Only if calculating weigted frequences.
}
\item{cell_ids}{
Index vector for selecting values in cell.
}
\item{row_ids}{
Index vector for selecting values in row.
}
\item{col_ids}{
Index vector for selecting values in col.
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
\item{digits}{
Integer indicating the number of decimal places
}
\item{type}{
Type of frequencies, one of following.
\itemize{
      \item{n, Number in cell. }
      \item{pct, Overall percentages. }
      \item{npct, Overall percentages and n. }
      \item{rowpct, Percentages of rows. }
      \item{colpct, Percentages of cols. }
      \item{all, Overall, row, col percentages. }
      }
}
}
\details{
The function calculate frequencies for cell.
If x has no missings the frequencies are independent from x.
}
\author{
ADES <ades-s@web.de>
}
\examples{
sex     <- factor(rbinom(1000, 1, 0.4),  labels=c('Men', 'Women'))
decades <- rbinom(1000, 3, 0.5)
decades <- factor(decades, labels=c('[35,45)','[45,55)','[55,65)','[65,75)'))
d<-data.frame(sex, decades)
tabular.ade(x_var='sex',  rows=c('sex',     'ALL'), rnames=c('Gender'),
                          cols=c('decades', 'ALL'), cnames=c('Age decades'),
            data=d, FUN=n_cell, , type="all")
}
\keyword{ percentages }
\keyword{ frequency }
