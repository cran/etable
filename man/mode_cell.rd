\name{mode_cell}
\alias{mode_cell}

\title{
Mode cell function
}
\description{
Shows the most frequent value (mode)
}
\usage{
   mode_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
          digits=3)
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
Weights for x variable. Only if calculating weighted mode.
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
Integer indicating the number of significant digits.
}
}
\author{
Andreas Schulz <ades-s@web.de>
}
\examples{

sex     <- factor(rbinom(1000, 1, 0.4),  labels=c('Men', 'Women'))
note    <- as.factor(rbinom(1000, 4, 0.5)+1)
decades <- rbinom(1000, 3, 0.5)
decades <- factor(decades, labels=c('[35,45)','[45,55)','[55,65)','[65,75)'))
d<-data.frame(sex, decades, note)

tabular.ade(x_vars=c('note'), xname=c('Noten'),
       rows=c('sex','ALL','decades'), rnames=c('Gender', 'Age decades'),
       data=d, FUN=mode_cell)


}
\keyword{ mode }
\keyword{ frequency }