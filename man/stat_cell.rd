\name{stat_cell}
\alias{stat_cell}
\title{
Diverse statistics Cell FUN
}
\description{
To calculate values of several statistics.
}
\usage{
       stat_cell(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min,
       digits = 3, digits2=1)
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
a vector of character strings with names of variables in data.frame for x,y and z.
Uses names of x or y as keywords, to choose the statistik.
}
  \item{n_min}{
Minimum n in the cell for useful calculation. Cells with n<n_min deliver no output.
}
\item{digits}{
Integer indicating the number of significant digits.
}
\item{digits2}{
Integer indicating the number of decimal places for percentages.
}
}
\details{
Keywords are:
\itemize{
      \item{ N:        number in this cell  }
      \item{ MIN:      minimum   }
      \item{ MAX:      maximum   }
      \item{ SUM:      sum   }
      \item{ MEAN:     mean   }
      \item{ SD:       standard deviation  }
      \item{ MSD:      mean, standard deviation  }
      \item{ VAR:      variance   }
      \item{ MEDIAN:   median   }
      \item{ MD:       mean deviation from the mean (*1.253)  }
      \item{ MAD:      median absolute deviation (*1.4826)  }
      \item{ IQR:      interquartile range  }
      \item{ MQQ:      median (Q1/Q3)  }
      \item{ PROP:     proportion   }
      \item{ RANGE:    range   }
      \item{ CV:       coefficient of variation   }
      \item{ MODE:     mode  }
      \item{ MISS:     number of missing values   }
      \item{ SKEW:     skewness   }
      \item{ KURT:     excess kurtosis }
      \item{ P1:       1th    Quantile  }
      \item{ P2.5:     2.5th  Quantile   }
      \item{ P5:       5th    Quantile   }
      \item{ P10:      10th   Quantile   }
      \item{ P25:      25th   Quantile   }
      \item{ P50:      50th   Quantile   }
      \item{ P75:      75th   Quantile   }
      \item{ P90:      90th   Quantile   }
      \item{ P95:      95th   Quantile   }
      \item{ P97.5:    97.5th Quantile   }
      \item{ P99:      99th   Quantile   }
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
   y_vars=c('N', 'MEAN', 'SD', 'SKEW', 'KURT'),
   rows=c('sex', 'ALL', 'decades', 'ALL'), rnames=c('Gender', 'Age decades'),
   data=d, FUN=stat_cell)
}
\keyword{ frequency }
\keyword{ median }
\keyword{ mean }
\keyword{ mode }
\keyword{ quantile }
\keyword{ sd }
