\name{convert_to_biom}
\alias{convert_to_biom}
\title{Convert dataframe objects to biom-format \code{biom-class} objects.}
\usage{
  convert_to_biom(data)
}
\arguments{
  \item{data}{(Required) Dataframe or matrix of count data.}
  \item{sample_metadata=NULL}{Dataframe of sample metadata.}
  \item{observation_metadata=NULL}{Dataframe of sample metadata.}
  \item{id=NULL}{Optional identifier for the project.}
\value{
  An object of \code{biom-class}.
}
\description{
  Wrapper to convert dataframe objects to \code{biom-class} objects. Currently cannot
  create sparse biom-format objects. The user can then export their previously
  prepared files using \code{write_biom}.
  \href{http://biom-format.org/documentation/biom_format.html}{the
  biom-format definition}.
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
# randomly create a count matrix
set.seed(101)
counts = matrix(sample(100,10*10),10,10)
rownames(counts) = paste("feature",1:10,sep="")
colnames(counts) = paste("sample",1:10,sep="")
# create a sample_metadata df
class = sample(0:1,10,replace=TRUE)
names(class) = colnames(counts)
class = as.data.frame(class)
# create a observation_metadata df
features = sample(c("bacteria_a","bacteria_b","bacteria_c"),10,replace = TRUE)
names(features) = rownames(counts)
features = as.data.frame(features)

biom_obj = convert_to_biom(counts,sample_metadata=class,observation_metadata=feature_meta)
# write(biom_obj,output)
}
\references{
  \url{http://biom-format.org/}
}
}

