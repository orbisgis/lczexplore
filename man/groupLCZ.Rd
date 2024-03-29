% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupLCZ.R
\name{groupLCZ}
\alias{groupLCZ}
\title{Allows to group (Local Climate Zone) geographical classification levels into broader categories
to explore classification agreements}
\usage{
groupLCZ(sf, column, outCol = "grouped", ...)
}
\arguments{
\item{sf}{is the input files. It must be an sf file and contain an LCZ column (levels 1 to 10 and 101 to 107). It must contain the geom column.}

\item{column}{is the name of the column containing the LCZ to be grouped}

\item{outCol}{is the name of the colum in which the grouped LCZ will be put}

\item{...}{the names of the groups followed by the levels the regrouped for instance urban=c("1","2","3","4","5","6","7","8","9")}
}
\value{
a file containing the original geom and lcz, and a new grouped column
}
\description{
Allows to group (Local Climate Zone) geographical classification levels into broader categories
to explore classification agreements
}
\examples{
redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",
urban=c("1","2","3","4","5","6","7","8","9"),
industry="10", vegetation=c("101","102","103","104"),
impervious="105",pervious="106",water="107",
colors=c("red","black","green","grey","burlywood","blue"))

showLCZ(redonBDT,column="LCZ_PRIMARY", repr="standard")
showLCZ(redonBDTgrouped,column="grouped",repr="alter",
LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
colors=c("red","black","green","grey","burlywood","blue"),wf="BD TOPO")
}
