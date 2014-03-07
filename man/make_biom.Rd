\name{make_biom}
\alias{make_biom}
\title{Create a \code{\link{biom-class}} from \code{\link{matrix-class}} or \code{\link{data.frame}}.}
\usage{
  make_biom(data, sample_metadata = NULL,
  observation_metadata = NULL, id = NULL)
}
\arguments{
  \item{data}{(Required). \code{\link{matrix-class}} or
  \code{\link{data.frame}}. A contingency table.
  Observations / features / OTUs / species are rows,
  samples / sites / libraries are columns.}

  \item{sample_metadata}{(Optional). A
  \code{\link{matrix-class}} or \code{\link{data.frame}}
  with the number of rows equal to the number of samples in
  \code{data}. Sample covariates associated with the count
  data. This should look like the table returned by
  \code{\link{sample_metadata}} on a valid instance of the
  \code{\link{biom-class}}.}

  \item{observation_metadata}{(Optional). A
  \code{\link{matrix-class}} or \code{\link{data.frame}}
  with the number of rows equal to the number of features /
  species / OTUs / genes in \code{data}. This should look
  like the table returned by
  \code{\link{observation_metadata}} on a valid instance of
  the \code{\link{biom-class}}.}

  \item{id}{(Optional). Character string. Identifier for
  the project.}
}
\value{
  An object of \code{\link{biom-class}}.
}
\description{
  This function creates a valid instance of the
  \code{\link{biom-class}} from standard base-R objects
  like \code{\link{matrix-class}} or
  \code{\link{data.frame}}. This makes it possible to
  export any contingency table data represented in R to
  \href{http://biom-format.org/documentation/biom_format.html}{the
  biom-format}, regardless of its source. The object
  returned by this function is appropriate for writing to a
  \code{.biom} file using the \code{\link{write_biom}}
  function. The sparse biom-format is not (yet) supported.
}
\details{
  The BIOM file format (canonically pronounced biome) is
  designed to be a general-use format for representing
  biological sample by observation contingency tables. BIOM
  is a recognized standard for the
  \href{http://www.earthmicrobiome.org/}{Earth Microbiome
  Project} and is a \href{http://gensc.org/}{Genomics
  Standards Consortium} candidate project. Please see
  \href{http://biom-format.org/}{the biom-format home page}
  for more details.
}
\examples{
# import with default parameters, specify a file
biomfile = system.file("extdata", "rich_dense_otu_table.biom", package = "biom")
x = read_biom(biomfile)
data = biom_data(x)
data
smd = sample_metadata(x)
smd
omd = observation_metadata(x)
omd
# Make a new biom object from component data
y = make_biom(data, smd, omd)
# Won't be identical to x because of header info.
identical(x, y)
# The data components should be, though.
identical(observation_metadata(x), observation_metadata(y))
identical(sample_metadata(x), sample_metadata(y))
identical(biom_data(x), biom_data(y))
## Quickly show that writing and reading still identical.
# Define a temporary directory to write .biom files
tempdir = tempdir()
write_biom(x, biom_file=file.path(tempdir, "x.biom"))
write_biom(y, biom_file=file.path(tempdir, "y.biom"))
x1 = read_biom(file.path(tempdir, "x.biom"))
y1 = read_biom(file.path(tempdir, "y.biom"))
identical(observation_metadata(x1), observation_metadata(y1))
identical(sample_metadata(x1), sample_metadata(y1))
identical(biom_data(x1), biom_data(y1))
}
\references{
  \url{http://biom-format.org/}
}
\seealso{
  \code{\link{write_biom}}

  \code{\link{biom-class}}

  \code{\link{read_biom}}
}

