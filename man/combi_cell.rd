\name{combi_cell}
\alias{combi_cell}

\title{
Dichotomous and continuous variable combination cell function
}
\description{
Calculates different statistics depending on the type of variable.
}
\usage{
combi_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
              digits=3, style=1)
}

\arguments{
  \item{x}{
The x variable for calculations, if not using y
}
  \item{y}{
The y variable for calculations, if not using x
}
  \item{z}{
NOT USED
}
  \item{w}{
Weights for x or y variable.
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
  \item{style}{
Type of representation.
\itemize{
      \item{1 N, Proportion, Median, Q1, Q3 }
      \item{2 N, Proportion, Mean, SD }
      }
}
}

\author{
Andreas Schulz <ades-s@web.de>
}
\examples{


sex     <- factor(rbinom(1000, 1, 0.4),  labels=c('Men', 'Women'))
height  <- rnorm(1000, mean=1.7, sd=0.1)
weight  <- rnorm(1000, mean=70, sd=5)
bmi     <- weight/height^2
event   <- factor(rbinom(1000, 1, 0.1), labels=c('no',  'yes'))
d<-data.frame(sex, height, weight, bmi, event)

tabular.ade(x_vars=names(d), cols=c('sex','ALL'), rnames=c('Gender'),
            data=d, FUN=combi_cell)


}
\keyword{ mean }
\keyword{ median }
