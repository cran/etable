\name{stat_cell}
\alias{stat_cell}

\title{
Diverse statistics cell function
}
\description{
Calculating values of several descriptive statistics.
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
A vector of character strings with names of variables in data.frame for x, y and z.
Use names of x or y as keywords, to choose a certain statistic.
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
      \item{ MCI:      mean, 95\% CI}
      \item{ VAR:      variance   }
      \item{ MEDIAN:   median   }
      \item{ MD:       mean deviation from the mean (*1.253)  }
      \item{ MAD:      median absolute deviation (*1.4826)  }
      \item{ IQR:      interquartile range  }
      \item{ MQQ:      median (Q1/Q3)  }
      \item{ PROP:     proportion   }
      \item{ POP:      proportion of level 2 (only binar)  }
      \item{ PCI:      proportion of level 2, 95\% CI  }
      \item{ RANGE:    range   }
      \item{ CV:       coefficient of variation   }
      \item{ MODE:     mode  }
      \item{ MISS:     number of missing values   }
      \item{ PNM:      proportion of non missing values   }
      \item{ COMB:     POP for binar and MQQ for continues  }
      \item{ SKEW:     skewness   }
      \item{ KURT:     excess kurtosis }
      \item{ GEO:      geometric mean }
      \item{ HARM:     harmonic mean }
      \item{ TM1:      truncated mean 1\% }
      \item{ TM5:      truncated mean 5\% }
      \item{ TM10:     truncated mean 10\% }
      \item{ TM25:     truncated mean 25\% }
      \item{ WM1:      winsorized mean 1\% }
      \item{ WM5:      winsorized mean 5\% }
      \item{ WM10:     winsorized mean 10\% }
      \item{ WM25:     winsorized mean 25\% }
      \item{ M1SD:     mean-SD, mean+SD  }
      \item{ M2SD:     mean-2SD, mean+2SD  }
      \item{ M3SD:     mean-3SD, mean+3SD  }
      \item{ MM1SD:    mean, mean-SD, mean+SD  }
      \item{ MM2SD:    mean, mean-2SD, mean+2SD  }
      \item{ MM3SD:    mean, mean-3SD, mean+3SD  }
      \item{ NORM50:   mean-0.675SD, mean+0.675SD  }
      \item{ NORM90:   mean-1.645SD, mean+1.645SD  }
      \item{ NORM95:   mean-1.96SD,  mean+1.96SD  }
      \item{ NORM99:   mean-2.576SD, mean+2.576SD  }
      \item{ P1:       1th    quantile  }
      \item{ P2.5:     2.5th  quantile   }
      \item{ P5:       5th    quantile   }
      \item{ P10:      10th   quantile   }
      \item{ P20:      20th   quantile   }
      \item{ P25:      25th   quantile   }
      \item{ P30:      30th   quantile   }
      \item{ P40:      40th   quantile   }
      \item{ P50:      50th   quantile   }
      \item{ P60:      60th   quantile   }
      \item{ P70:      70th   quantile   }
      \item{ P75:      75th   quantile   }
      \item{ P80:      80th   quantile   }
      \item{ P90:      90th   quantile   }
      \item{ P95:      95th   quantile   }
      \item{ P97.5:    97.5th quantile   }
      \item{ P99:      99th   quantile   }
}


}
\author{
Andreas Schulz <ades-s@web.de>
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