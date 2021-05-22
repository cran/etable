\name{tabular.ade}
\alias{tabular.ade}
\title{
Tabular representation of a wide selection of statistics
}
\description{
Creates simple to highly customized tables for a wide selection of descriptive statistics, with or without weighting the data.
}
\usage{
tabular.ade(x_vars, xname=NULL, y_vars=NULL, yname=NULL,
            z_vars=NULL, zname=NULL,
            rows=NULL, rnames=NULL, cols=NULL, cnames=NULL, w=NULL,
            data=NULL, FUN, allnames=FALSE, nonames=TRUE, alllabel='Total',
            inset='?', remove='', n_min=0, ...)


}
\arguments{
\item{x_vars}{
This variable will be used to calculate the statistics for it.
\itemize{
      \item{ a character string with the name of the variable in the data.frame }
      \item{ a vector of character strings with names of variables in data.frame}
}
}
\item{xname}{
Labels for x.
\itemize{
      \item{ a character string with the label for x }
      \item{ a vector of character strings with labels for x, with the same length as x. }
}
}
\item{y_vars}{
This variable can be used to calculate bivariable statistics.
\itemize{
      \item{ a character string with the name of the variable in the data.frame }
      \item{ a vector of character strings with names of variables in data.frame}
}
}
\item{yname}{
Labels for y.
\itemize{
      \item{ a character string with the label for y }
      \item{ a vector of character strings with labels for y, with same length as x. }
}
}
\item{z_vars}{
This variable can be used for additional calculations.
\itemize{
      \item{ a character string with the name of the variable in the data.frame }
}
}

\item{zname}{
Labels for z.
\itemize{
      \item{ a character string with the label for y}
}
}
\item{rows}{
These factors will be used to separate the rows of the table in subgroups.
\itemize{
      \item{ a character string with the name of the factor variable in the data.frame }
      \item{ a vector of character strings with names of factor variables in data.frame (max 6)}
      \item{ a vector with names of factors and/or Keyword 'ALL' adds extra overall group for leading factor.}
}
}
\item{rnames}{
Labels for rows.
\itemize{
      \item{ a character string with the label for rows }
      \item{ a vector of character strings with labels for rows, with same length as rows. }
      \item{ a vector with names of factors and/or keyword 'ALL' adds extra overall group for leading factor.}
}
}
\item{cols}{
These factors will be used to separate the columns of the table in subgroups.
\itemize{
      \item{ a character string with the name of the factor variable in the data.frame }
      \item{ a vector of character strings with names of factor variables in data.frame (max 6)}
}
}
\item{cnames}{
Labels for cols.
\itemize{
      \item{ a character string with the label for cols }
      \item{ a vector of character strings with labels for rows, with same length as cols. }
}
}
\item{w}{
This numeric variable will be used to weight the table.
\itemize{
      \item{ a character string with the name of the factor variable in the data.frame }
}
}
\item{data}{
A data frame with all used variables.
}
\item{FUN}{
An abstract cell function to calculate statistics in every cell of the table. See details.
}
\item{allnames}{
Logical asking whether to fill every cell with labels or only the first one.
}
\item{nonames}{
Logical asking whether to use dimnames for variable labels or make all labeling in the table self.
}
\item{alllabel}{
Label for overall group without splitting in this factor.
}
\item{inset}{
Inset text in each cell, '?' will be replaced with the value of the cell.
}
\item{remove}{
Remove a character string from each cell.
}
\item{n_min}{
min N in each cell, it will be only passed in the cell function.
But it is necessary to suppress calculation of statistics using only few values.
}
\item{\dots}{
additional parameters passed to the FUN
}
}
\details{
FUN can be a cell function from this package or a custom cell function.

The custom cell function must take the following parameters, but it is not necessary to use them.

\itemize{
      \item{x, The whole x variable. }
      \item{y, The whole y variable. }
      \item{z, The whole z variable. }
      \item{w, The whole w variable. }
      \item{cell_ids, Index vector to select values that belong in this cell. }
      \item{row_ids,  Index vector to select values that belong in this row. }
      \item{col_ids,  Index vector to select values that belong in this col. }
      \item{vnames,   A vector of length 3, with labels of variables (x,y,z)}
      \item{vars,     A vector of length 3, with names  of variables (x,y,z)}
      \item{n_min  ,  Min needed N for calculation. }
      \item{... ,  additional custom parameters. }
}

For an example with simple mean see below.

}
\value{
A character Matrix.(Table)
}
\author{
Andreas Schulz <ades-s@web.de>
}
\examples{
# 1) simple own FUN cell function.
s_mean<- function(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min, ds=3){
out<- ''
if(length(cell_ids)>= n_min){
out<-  format(mean(x[cell_ids], na.rm=TRUE), digits=ds)
}
return(out)
}
##########################################
# 2) simple 2 x 2 table of means
sex   <- factor(rbinom(5000, 1, 0.5), labels=c('Men', 'Women'))
age   <- round(runif(5000, 18, 89))
treat <- factor(rbinom(5000, 1, 0.3), labels=c('control', 'treated'))
d<-data.frame(sex, age, treat)

tabular.ade(x_vars='age', xname='Age [y]', rows='sex', rnames='Sex', cols='treat',
cnames='Treatment', data=d, nonames=FALSE, FUN=s_mean)

##########################################
# 3) Relative frequency table
d$dosis <- round(runif(5000, 0.5, 6.49))
tabular.ade(x_vars='age', xname='Age [y]', rows=c('sex', 'treat'),
rnames=c('Sex', 'Treatment'), cols='dosis', cnames='Dosis', data=d, FUN=n_cell,
type='pct')

##########################################
# 4) Weighted median table
d$w <- runif(5000, 0.1, 5)
d$bmi <- rnorm(5000, 30, 3)
tabular.ade(x_vars=c('age', 'bmi'), xname=c('Age', 'BMI'),
cols=c('sex', 'ALL', 'treat'),
cnames=c('Sex', 'Treatment'), w='w', data=d, FUN=quantile_cell)

##########################################
# 5) Correlation table between age and bmi
tabular.ade(x_vars='age', xname='Age', y_vars='bmi', yname='BMI',
rows=c('dosis'), rnames=c('Dosis'), cols=c('sex', 'treat'),
cnames=c('Sex', 'Treatment'), data=d, FUN=corr_p_cell)

##########################################
# 6) Multiple statistics
tabular.ade(x_vars=c('N', 'MEAN', 'SD', 'SKEW', 'KURT', 'RANGE'),
y_vars=c('age', 'bmi'), yname=c('Age', 'BMI'),
cols=c('sex', 'ALL', 'treat'), cnames=c('Sex', 'Treatment'),
w='w', data=d, FUN=stat_cell)


}
\keyword{ table }


