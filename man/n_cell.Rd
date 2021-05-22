\name{n_cell}
\alias{n_cell}

\title{
Frequency Cell FUN
}
\description{
For calculation of relative or absolute frequencies.
}
\usage{
   n_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
          digits=0, digits2=1, type="n")
}

\arguments{
\item{x}{
The x variable (can be just 1:N if without missings values)
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
Integer indicating the number of decimal places (N)
}
\item{digits2}{
Integer indicating the number of decimal places (percentages)
}
\item{type}{
Type of frequencies, one of following.
\itemize{
      \item{n, number in cell. }
      \item{pct, overall percentages. }
      \item{pctn, overall percentages and n. }
      \item{rowpct, percentages of rows. }
      \item{colpct, percentages of cols. }
      \item{rowpctn, percentages of rows and n. }
      \item{colpctn, percentages of cols and n. }
      \item{all, overall, row, col percentages. }
      }
}
}
\details{
The function calculate frequencies for cell.
If x has no missing values the frequencies are independent of x.
Missing values in x will be removed from calculation.

}
\author{
Andreas schulz <ades-s@web.de>
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