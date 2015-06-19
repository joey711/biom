################################################################################
#' Build and return an instance of the biom-class.
#'
#' This is for instantiating a biom object within R (\code{\link{biom-class}}),
#' and assumes relevant data is already available in R. 
#' This is different than reading a biom file into R.
#' If you are instead interested in importing a biom file into R,
#' you should use the \code{\link{read_biom}} function. 
#' This function is made available (exported) so that 
#' advanced-users/developers
#' can easily represent analogous data in this structure if needed.
#' However, most users are expected to instead rely on the
#' \code{\link{read_biom}} function for data import, followed by 
#' accessor functions that extract R-friendly
#' subsets of the data stored in the biom-format derived list.
#'
#' \code{biom()} is a constructor method. This is the main method
#' suggested for constructing an experiment-level (\code{\link{biom-class}})
#' object from its component data.
#'
#' @usage biom(x)
#'
#' @param x (REQUIRED). A named list conforming to conventions arising from 
#'  the \code{\link{fromJSON}} function reading a biom-format file with 
#'  default settings. See \code{\link{read_biom}} for more details about
#'  data import and 
#'  \code{\link{biom-class}} for more details about accessor functions
#'  that extract R-friendly
#'  subsets of the data and metadata stored in \code{x}.
#'
#' @return An instance of the \code{\link{biom-class}}. 
#'
#' @seealso 
#' 
#' Function to create a biom object from R data,
#' \code{\link{make_biom}}.
#' 
#' Definition of the
#' \code{\link{biom-class}}. 
#' 
#' The \code{\link{read_biom}} import function.
#' 
#' Function to write a biom format file from a biom object,
#' \code{\link{write_biom}}
#'
#' Accessor functions like \code{\link{header}}.
#'
#' @docType methods
#' @aliases biom
#' @rdname biom-methods
#' @export
#' @examples #
#' # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' x = read_biom(biom_file)
#' show(x)
#' print(x)
#' header(x)
#' biom_data(x)
#' biom_shape(x)
#' nrow(x)
#' ncol(x)
#' observation_metadata(x)
#' sample_metadata(x)
setGeneric("biom", function(x) standardGeneric("biom"))
#' @aliases biom,list-method
#' @rdname biom-methods
setMethod("biom", c("list"), function(x){
	# Some instantiation checks chould go here, or wrap them in validity methods.
	# Depends on how strict the check should be.
	biom = new("biom", x)
	return(biom)
})
################################################################################
#' Create a \code{\link{biom-class}} from \code{\link{matrix-class}} or \code{\link{data.frame}}.
#'
#' This function creates a valid instance of the \code{\link{biom-class}}
#' from standard base-R objects like
#' \code{\link{matrix-class}} or \code{\link{data.frame}}. 
#' This makes it possible to export any contingency table data 
#' represented in R to
#' \href{http://biom-format.org/documentation/biom_format.html}{the biom-format},
#' regardless of its source.
#' The object returned by this function is appropriate for writing to
#' a \code{.biom} file using the \code{\link{write_biom}} function.
#' The sparse biom-format is not (yet) supported.
#'
#' The BIOM file format (canonically pronounced biome) is designed to be
#' a general-use format for representing biological sample by observation 
#' contingency tables. BIOM is a recognized standard for the 
#' \href{http://www.earthmicrobiome.org/}{Earth Microbiome Project} 
#' and is a \href{http://gensc.org/}{Genomics Standards Consortium} 
#' candidate project. Please see 
#' \href{http://biom-format.org/}{the biom-format home page}
#' for more details.
#'
#' @param data (Required). 
#'  \code{\link{matrix-class}} or \code{\link{data.frame}}.
#'  A contingency table.
#'  Observations / features / OTUs / species are rows,
#'  samples / sites / libraries are columns.
#'  
#' @param sample_metadata (Optional). 
#'  A \code{\link{matrix-class}} or \code{\link{data.frame}}
#'  with the number of rows equal to the number of samples in \code{data}.
#'  Sample covariates associated with the count data.
#'  This should look like the table returned by
#'  \code{\link{sample_metadata}} on a valid instance
#'  of the \code{\link{biom-class}}.
#'  
#' @param observation_metadata (Optional). 
#'  A \code{\link{matrix-class}} or \code{\link{data.frame}}
#'  with the number of rows equal to the number of 
#'  features / species / OTUs / genes in \code{data}.
#'  This should look like the table returned by
#'  \code{\link{observation_metadata}} on a valid instance
#'  of the \code{\link{biom-class}}.
#' 
#' @param id (Optional). Character string. Identifier for the project.
#'
#' @param matrix_element_type (Optional). Character string. Either 'int' or 'float'
#' 
#' @return An object of \code{\link{biom-class}}.
#'
#' @references \url{http://biom-format.org/}
#' 
#' @seealso
#' 
#' \code{\link{write_biom}} 
#' 
#' \code{\link{biom-class}} 
#' 
#' \code{\link{read_biom}} 
#'
#' @importFrom plyr alply
#'
#' @export
#'
#' @examples
#' # import with default parameters, specify a file
#' biomfile = system.file("extdata", "rich_dense_otu_table.biom", package = "biom")
#' x = read_biom(biomfile)
#' data = biom_data(x)
#' data
#' smd = sample_metadata(x)
#' smd
#' omd = observation_metadata(x)
#' omd
#' # Make a new biom object from component data
#' y = make_biom(data, smd, omd)
#' # Won't be identical to x because of header info.
#' identical(x, y)
#' # The data components should be, though.
#' identical(observation_metadata(x), observation_metadata(y))
#' identical(sample_metadata(x), sample_metadata(y))
#' identical(biom_data(x), biom_data(y))
#' ## Quickly show that writing and reading still identical.
#' # Define a temporary directory to write .biom files
#' tempdir = tempdir()
#' write_biom(x, biom_file=file.path(tempdir, "x.biom"))
#' write_biom(y, biom_file=file.path(tempdir, "y.biom"))
#' x1 = read_biom(file.path(tempdir, "x.biom"))
#' y1 = read_biom(file.path(tempdir, "y.biom"))
#' identical(observation_metadata(x1), observation_metadata(y1))
#' identical(sample_metadata(x1), sample_metadata(y1))
#' identical(biom_data(x1), biom_data(y1))
make_biom <- function(data, sample_metadata=NULL, observation_metadata=NULL, id=NULL, matrix_element_type="int"){
  # The observations / features / OTUs / rows "meta" data table
  if(!is.null(observation_metadata)){
    rows = mapply(list, SIMPLIFY=FALSE, id=as.list(rownames(data)),
                  metadata=alply(as.matrix(observation_metadata), 1, .expand=FALSE, .dims=TRUE))
  } else {
    rows = mapply(list, id=as.list(rownames(data)), metadata=NA, SIMPLIFY=FALSE)
  }
  # The samples / sites / columns "meta" data table
  if(!is.null(sample_metadata)){
    columns = mapply(list, SIMPLIFY=FALSE, id=as.list(colnames(data)),
                     metadata=alply(as.matrix(sample_metadata), 1, .expand=FALSE, .dims=TRUE)) 
  } else {
    columns = mapply(list, id=as.list(colnames(data)), metadata=NA, SIMPLIFY=FALSE)
  }
  # Convert the contingency table to a list
  datalist = as.list(as.data.frame(as(t(data), "matrix")))
  names(datalist) <- NULL
  # Define the list, instantiate as biom-format, and return
  # (Might eventually expose some of these list elements as function arguments)
  format_url = "http://biom-format.org/documentation/format_versions/biom-1.0.html"
  return(biom(list(id=id,
                   format = "Biological Observation Matrix 1.0.0-dev",
                   format_url = format_url,
                   type = "OTU table",
                   generated_by = sprintf("biom %s", packageVersion("biom")),
                   date = as.character(Sys.time()),
                   matrix_type = "dense",
                   matrix_element_type = matrix_element_type,
                   shape = dim(data),
                   rows = rows,
                   columns = columns,
                   data = datalist)
  ))
}
################################################################################
#' Method extensions to show for biom objects.
#'
#' See the general documentation of \code{\link[methods]{show}} method for
#' expected behavior. 
#'
#' @seealso \code{\link[methods]{show}}
#' @param object biom-class object
#' @export
#' @aliases show
#' @aliases biom-method
#' @docType methods
#' @rdname show-methods
#' @examples
#' # # # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' (x = read_biom(biom_file) )
#' show(x)
setMethod("show", "biom", function(object){
	cat("biom object. \n")
	cat("type:", object$type, "\n")
	cat("matrix_type:", object$matrix_type, "\n")
	cat(biom_shape(object)[1], "rows and", biom_shape(object)[2], "columns \n")
})
################################################################################
#' Extract the header from a \code{\link{biom-class}} object as a list. 
#'
#' @usage header(x)
#'
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#'
#' @return A list containing the header data.
#'  That is, all the required elements that are not
#'  the main data or index metadata.
#'  
#' @aliases header
#' @docType methods
#' @rdname header-methods
#' @export
#' @examples
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' x = read_biom(biom_file)
#' header(x)
setGeneric("header", function(x) standardGeneric("header"))
#' @aliases header,biom-method
#' @rdname header-methods
setMethod("header", c("biom"), function(x){
	biomheadkeys = c("id", "format", "format_url", "type", "generated_by", "date",
									 "matrix_type", "matrix_element_type", "shape")
	return(x[biomheadkeys])	
})
################################################################################
#' The matrix dimensions
#' of a \code{\link{biom-class}} object.
#'
#' @usage biom_shape(x)
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @return A length two \code{\link{integer-class}} vector
#'  indicating the \code{\link{nrow}} and \code{\link{ncol}} 
#'  of the main data matrix stored in \code{x}.
#'
#' @seealso 
#' 
#' \code{\link{biom-class}}
#' 
#' @export
#' @docType methods
#' @rdname biom_shape-methods
#' @examples
#' # # # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' (x = read_biom(biom_file) )
#' biom_shape(x)
setGeneric("biom_shape", function(x) standardGeneric("biom_shape"))
#' @aliases biom_shape,biom-method
#' @rdname biom_shape-methods
setMethod("biom_shape", c("biom"), function(x){
	return(as(c(nrow=x$shape[1], ncol=x$shape[2]), "integer"))
})
################################################################################
#' Access class of data in the matrix elements
#' of a \code{\link{biom-class}} object
#'
#' @usage matrix_element_type(x)
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @return A \code{\link{character-class}} string indicating 
#' the class of the data stored in the main observation matrix of \code{x}, 
#' with expected values \code{"int"}, \code{"float"}, \code{"unicode"}.
#'
#' @seealso 
#' 
#' \code{\link{biom-class}}
#' 
#' @export
#' @docType methods
#' @rdname matrix_element_type-methods
#' @examples
#' # # # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' (x = read_biom(biom_file) )
#' matrix_element_type(x)
setGeneric("matrix_element_type", function(x) standardGeneric("matrix_element_type"))
#' @aliases matrix_element_type,biom-method
#' @rdname matrix_element_type-methods
setMethod("matrix_element_type", c("biom"), function(x){
	return(x$matrix_element_type)
})
################################################################################
#' Method extensions to \code{\link[base]{nrow}}
#' for \code{\link{biom-class}} objects.
#'
#' See the general documentation of \code{\link[base]{nrow}} method for
#' expected behavior. 
#'
#' @usage nrow(x)
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @return The number of rows in \code{x}.
#'  A length 1 \code{\link{integer-class}}.
#'
#' @seealso 
#' 
#' \code{\link{ncol}}
#' 
#' \code{\link[base]{nrow}}
#' 
#' \code{\link{biom_shape}}
#' 
#' @export
#' @aliases nrow
#' @aliases biom-method
#' @docType methods
#' @rdname nrow-methods
#' @examples
#' # # # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' (x = read_biom(biom_file) )
#' nrow(x)
setMethod("nrow", c("biom"), function(x){
	return( biom_shape(x)["nrow"] )
})
################################################################################
#' Method extensions to \code{\link[base]{ncol}}
#' for \code{\link{biom-class}} objects.
#'
#' See the general documentation of \code{\link[base]{ncol}} method for
#' expected behavior. 
#' 
#' @usage ncol(x)
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @return The number of columns in \code{x}.
#'  A length 1 \code{\link{integer-class}}.
#'
#' @seealso 
#' 
#' \code{\link{nrow}}
#' 
#' \code{\link[base]{ncol}}
#' 
#' \code{\link{biom_shape}}
#' 
#' @export
#' @aliases ncol
#' @aliases biom-method
#' @docType methods
#' @rdname ncol-methods
#' @examples
#' # # # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' (x = read_biom(biom_file) )
#' ncol(x)
setMethod("ncol", c("biom"), function(x){
	return( biom_shape(x)["ncol"] )
})
################################################################################
#' Method extensions to \code{\link[base]{rownames}}
#' for \code{\link{biom-class}} objects.
#'
#' See the general documentation of \code{\link[base]{rownames}} method for
#' expected behavior. 
#' 
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @return The number of columns in \code{x}.
#'  A length 1 \code{\link{integer-class}}.
#'
#' @seealso 
#' 
#' \code{\link{nrow}}
#' 
#' \code{\link[base]{rownames}}
#' 
#' \code{\link{biom_shape}}
#' 
#' @export
#' @aliases rownames,biom-method
#' @docType methods
#' @rdname rownames-methods
#' @examples
#' # # # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' (x = read_biom(biom_file) )
#' rownames(x)
setMethod("rownames", c("biom"), function(x){
	sapply(x$rows, function(i) i$id)
})
################################################################################
#' Method extensions to \code{\link[base]{colnames}}
#' for \code{\link{biom-class}} objects.
#'
#' See the general documentation of \code{\link[base]{colnames}} method for
#' expected behavior. 
#' 
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @return The number of columns in \code{x}.
#'  A length 1 \code{\link{integer-class}}.
#'
#' @seealso 
#' 
#' \code{\link{nrow}}
#' 
#' \code{\link[base]{colnames}}
#' 
#' \code{\link{biom_shape}}
#' 
#' @export
#' @aliases colnames,biom-method
#' @docType methods
#' @rdname colnames-methods
#' @examples
#' # # # import with default parameters, specify a file
#' biom_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' (x = read_biom(biom_file) )
#' colnames(x)
setMethod("colnames", c("biom"), function(x){
	sapply(x$columns, function(i) i$id)
})
################################################################################
#' Access main data observation matrix data from \code{\link{biom-class}}. 
#' 
#' Retrieve and organize main data from \code{\link{biom-class}},
#' represented as a matrix with index names.
#'
#' @usage biom_data(x, rows, columns, parallel=FALSE)
#'
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @param rows (Optional). The subset of row indices described in the
#'  returned object. For large datasets, specifying the row subset here,
#'  rather than after creating the whole matrix first,
#'  can improve speed/efficiency.
#'  Can be vector of index numbers (\code{\link{numeric-class}}) or 
#'  index names (\code{\link{character-class}}).
#' @param columns (Optional). The subset of column indices described in the
#'  returned object. For large datasets, specifying the column subset here,
#'  rather than after creating the whole matrix first,
#'  can improve speed/efficiency.
#'  Can be vector of index numbers (\code{\link{numeric-class}}) or 
#'  index names (\code{\link{character-class}}).
#' @param parallel (Optional). Logical. Whether to perform the accession parsing
#'  using a parallel-computing backend supported by the \code{\link{plyr-package}}
#'  via the \code{\link[foreach]{foreach-package}}. Note: At the moment, the header
#'  accessor does not need nor does it support parallel-computed parsing.
#'  
#'  @return A matrix containing the main observation data, with index names.
#'   The type of data (numeric or character) 
#'   will depend on the results of \code{\link{matrix_element_type}(x)}.
#'   The class of the matrix returned will depend on the sparsity of the data,
#'   and whether it has numeric or character data.
#'   For now, only numeric data can be stored in a \code{\link{Matrix-class}},
#'   which will be stored sparsely, if possible.
#'   Character data will be returned as a vanilla \code{\link{matrix-class}}.
#'  
#' @aliases biom_data
#' @rdname biom_data-methods
#' @export
#' @examples 
#' min_dense_file   = system.file("extdata", "min_dense_otu_table.biom", package = "biom")
#' min_sparse_file  = system.file("extdata", "min_sparse_otu_table.biom", package = "biom")
#' rich_dense_file  = system.file("extdata", "rich_dense_otu_table.biom", package = "biom")
#' rich_sparse_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' min_dense_file   = system.file("extdata", "min_dense_otu_table.biom", package = "biom")
#' rich_dense_char  = system.file("extdata", "rich_dense_char.biom", package = "biom")
#' rich_sparse_char  = system.file("extdata", "rich_sparse_char.biom", package = "biom")
#' # Read the biom-format files
#' x1 = read_biom(min_dense_file)
#' x2 = read_biom(min_sparse_file)
#' x3 = read_biom(rich_dense_file)
#' x4 = read_biom(rich_sparse_file)
#' x5 = read_biom(rich_dense_char)
#' x6 = read_biom(rich_sparse_char)
#' # Extract the data matrices
#' biom_data(x1)
#' biom_data(x2)
#' biom_data(x3)
#' biom_data(x4)
#' biom_data(x5)
#' biom_data(x6)
setGeneric("biom_data", function(x, rows, columns, parallel=FALSE){
  standardGeneric("biom_data")
})
# All methods funnel toward signature biom,numeric,numeric
#' @aliases biom_data,biom,missing,missing-method
#' @rdname biom_data-methods
setMethod("biom_data", c("biom", "missing", "missing"), function(x, rows, columns, parallel){
  # Dispatch with full rows and cols
  biom_data(x, 1:nrow(x), 1:ncol(x), parallel)
})
#' @aliases biom_data,biom,character,ANY-method
#' @rdname biom_data-methods
setMethod("biom_data", c("biom", "character", "ANY"), function(x, rows, columns, parallel){
  rows = which(rownames(x) %in% rows)
  # Dispatch with specified numeric rows and pass cols
  biom_data(x, rows, columns)
})
#' @aliases biom_data,biom,ANY,character-method
#' @rdname biom_data-methods
setMethod("biom_data", c("biom", "ANY", "character"), function(x, rows, columns, parallel){
  columns = which(colnames(x) %in% columns)
  # Dispatch with specified numeric columns and pass rows
  biom_data(x, rows, columns)
})
#' @aliases biom_data,biom,numeric,missing-method
#' @rdname biom_data-methods
setMethod("biom_data", c("biom", "numeric", "missing"), function(x, rows, columns, parallel){
  # Dispatch with specified rows and full cols
  biom_data(x, rows, 1:ncol(x), parallel)
})
#' @aliases biom_data,biom,missing,numeric-method
#' @rdname biom_data-methods
setMethod("biom_data", c("biom", "missing", "numeric"), function(x, rows, columns, parallel){
  # Dispatch with full rows and specified cols
  biom_data(x, 1:nrow(x), columns, parallel)
})
#' @aliases biom_data,biom,numeric,numeric-method
#' @rdname biom_data-methods
#' @import Matrix
#' @importFrom plyr d_ply
#' @importFrom plyr ldply
#' @importFrom plyr laply
setMethod("biom_data", c("biom", "numeric", "numeric"), function(x, rows, columns, parallel){
  if( identical(length(rows), 0) ){
    stop("argument `rows` must have non-zero length.")
  }
  if( identical(length(columns), 0) ){
    stop("argument `columns` must have non-zero length.")
  }    
  # Begin matrix section
  if( identical(x$matrix_type, "dense") ){
    # Begin dense section
    # If matrix is stored as dense, create "vanilla" R matrix, m
    m = laply(x$data[rows], function(i) i[columns], .parallel=parallel) 
    if( length(rows) > 1L &
    		length(columns) > 1L &
    		matrix_element_type(x) %in% c("int", "float")
    	){
      # If either dimension is length-one, don't call coerce to "Matrix"
      # Note that laply() does still work in this case.
      # If both dimension lengths > 1 & data is numeric, 
      # attempt to coerce to Matrix-inherited class,
      # Mainly because it might still be sparse and this is a good way
      # to handle it in R. 
      m = Matrix(m)
    }
  } else {
    # Begin sparse section
    ## Initialize sparse matrix as either Matrix or matrix, depending on data class
    biom_shape = biom_shape(x)
    if(matrix_element_type(x) %in% c("int", "float")){
      # If data is numeric, initialize with Matrix (numeric data only)
      m = Matrix(0, nrow=nrow(x), ncol=ncol(x), sparse=TRUE)
      # Create an assignment data.frame
      adf = ldply(x$data)
    } else {
      # Else, matrix_element_type must be "unicode" for a unicode string.
      # Use a standard R character matrix
      m = matrix(NA_character_, nrow(x), ncol(x))
      # Create an assignment data.frame.
      # Is slightly more complicated for sparse JSON w/ character values
      adf = ldply(x$data, function(x){
                data.frame(r=x[[1]], c=x[[2]], data=x[[3]], stringsAsFactors=FALSE)
            })
    }
    colnames(adf) <- c("r", "c", "data")
    # indices start at 0 in biom sparse format, 
    # and are first two columns
    adf[, 1:2] <- adf[, 1:2] + 1
    # Subset to just indices that are in both arguments `rows` and `columns`
    adf = adf[(adf$r %in% rows & adf$c %in% columns), ]
    # Fill in data values in matrix, m.
    # Vectorized for speed using matrix indexing.
    # See help("Extract") for details about matrix indexing. Diff than 2-vec index.
    m[as(adf[, 1:2], "matrix")] <- adf[, 3]
    # Subset this biggest-size m to just `rows` and `columns`
    m = m[rows, columns]
  # End sparse section
  }   
  # Add row and column names
  if( identical(length(rows), 1L) | identical(length(columns), 1L) ){
    # If either dimension is length-one
    # Try naming by colnames first, then rownames
    if( identical(length(rows), 1L) ){
      names(m) <- sapply(x$columns[columns], function(i) i$id )
    } else {
      names(m) <- sapply(x$rows[rows], function(i) i$id )
    }
  } else {
    # Else, both dimensions are longer than 1,
    # can assume is a matrix and assign names to both dimensions
    rownames(m) <- sapply(x$rows[rows], function(i) i$id )
    colnames(m) <- sapply(x$columns[columns], function(i) i$id )
  }
  return(m)
})
################################################################################
#' Access meta data from \code{\link{biom-class}}. 
#' 
#' Retrieve and organize meta data from \code{\link{biom-class}},
#' represented as a \code{\link{data.frame}} (if possible, or a list) 
#' with proper index names.
#'
#' @usage sample_metadata(x, columns, parallel=FALSE)
#'
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @param columns (Optional). The subset of column indices described in the
#'  returned object. For large datasets, specifying the column subset here,
#'  rather than after creating the whole matrix first,
#'  can improve speed/efficiency.
#'  Can be vector of index numbers (\code{\link{numeric-class}}) or 
#'  index names (\code{\link{character-class}}).
#' @param parallel (Optional). Logical. Whether to perform the accession parsing
#'  using a parallel-computing backend supported by the \code{\link{plyr-package}}
#'  via the \code{\link[foreach]{foreach-package}}. 
#'  
#' @return A \code{\link{data.frame}} or \code{\link{list}} containing 
#'  the meta data, with index names. The precise form of the object returned
#'  depends on the metadata stored in \code{x}. A \code{data.frame} is
#'  created if possible.
#'  
#' @aliases sample_metadata
#' @rdname sample_metadata-methods
#' @export
#' @examples 
#' min_dense_file   = system.file("extdata", "min_dense_otu_table.biom", package = "biom")
#' min_sparse_file  = system.file("extdata", "min_sparse_otu_table.biom", package = "biom")
#' rich_dense_file  = system.file("extdata", "rich_dense_otu_table.biom", package = "biom")
#' rich_sparse_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' min_dense_file   = system.file("extdata", "min_dense_otu_table.biom", package = "biom")
#' rich_dense_char  = system.file("extdata", "rich_dense_char.biom", package = "biom")
#' rich_sparse_char  = system.file("extdata", "rich_sparse_char.biom", package = "biom")
#' # Read the biom-format files
#' x1 = read_biom(min_dense_file)
#' x2 = read_biom(min_sparse_file)
#' x3 = read_biom(rich_dense_file)
#' x4 = read_biom(rich_sparse_file)
#' x5 = read_biom(rich_dense_char)
#' x6 = read_biom(rich_sparse_char)
#' # Extract metadata
#' sample_metadata(x1)
#' sample_metadata(x2)
#' sample_metadata(x3)
#' sample_metadata(x3, 1:4)
#' sample_metadata(x4)
#' sample_metadata(x5)
#' sample_metadata(x6)
setGeneric("sample_metadata", function(x, columns, parallel=FALSE){
	standardGeneric("sample_metadata")
})
# All methods funnel toward signature biom,numeric
#' @aliases sample_metadata,biom,missing-method
#' @rdname sample_metadata-methods
setMethod("sample_metadata", c("biom", "missing"), function(x, columns, parallel=FALSE){
	# Dispatch with full rows and cols
	sample_metadata(x, 1:ncol(x), parallel)
})
#' @aliases sample_metadata,biom,character-method
#' @rdname sample_metadata-methods
setMethod("sample_metadata", c("biom", "character"), function(x, columns, parallel=FALSE){
	columns = which(sapply(x$columns, function(j) j$id) %in% columns)
	if( length(columns)==0 ){
		stop("The column ID names you provided do not match the column IDs in x")
	}
	# Dispatch with specified numeric columns
	sample_metadata(x, columns, parallel)
})
#' @rdname sample_metadata-methods
#' @aliases sample_metadata,biom,numeric-method
setMethod("sample_metadata", c("biom", "numeric"), function(x, columns, parallel=FALSE){
	if( any(columns > ncol(x)) ){
		warning(paste0("column indices ",
									 paste0(columns[columns > ncol(x)], collapse=" "),
									 " are greater than available columns in data. They were ignored."))
		columns = columns[columns <= ncol(x)]
	}
	return(extract_metadata(x, "columns", columns, parallel))
})
################################################################################
#' Access observation (row) meta data from \code{\link{biom-class}}. 
#' 
#' Retrieve and organize meta data from \code{\link{biom-class}},
#' represented as a \code{\link{data.frame}} (if possible)
#' or a list, with proper index names.
#'
#' @usage observation_metadata(x, rows, parallel=FALSE)
#'
#' @param x (Required). An instance of the \code{\link{biom-class}}.
#' @param rows (Optional). The subset of row indices described in the
#'  returned object. For large datasets, specifying the row subset here,
#'  -- rather than first creating the complete data object --
#'  can improve speed/efficiency.
#'  This parameter can be vector of index numbers (\code{\link{numeric-class}}) or 
#'  index names (\code{\link{character-class}}).
#' @param parallel (Optional). Logical. Whether to perform the accession parsing
#'  using a parallel-computing backend supported by the \code{\link{plyr-package}}
#'  via the \code{\link[foreach]{foreach-package}}. 
#'  
#' @return A \code{\link{data.frame}} or \code{\link{list}} containing 
#'  the meta data, with index names. The precise form of the object returned
#'  depends on the metadata stored in \code{x}. A \code{data.frame} is
#'  created if possible.
#'  
#' @aliases observation_metadata
#' @rdname observation_metadata-methods
#' @export
#' @examples 
#' min_dense_file   = system.file("extdata", "min_dense_otu_table.biom", package = "biom")
#' min_sparse_file  = system.file("extdata", "min_sparse_otu_table.biom", package = "biom")
#' rich_dense_file  = system.file("extdata", "rich_dense_otu_table.biom", package = "biom")
#' rich_sparse_file = system.file("extdata", "rich_sparse_otu_table.biom", package = "biom")
#' min_dense_file   = system.file("extdata", "min_dense_otu_table.biom", package = "biom")
#' rich_dense_char  = system.file("extdata", "rich_dense_char.biom", package = "biom")
#' rich_sparse_char  = system.file("extdata", "rich_sparse_char.biom", package = "biom")
#' # Read the biom-format files
#' x1 = read_biom(min_dense_file)
#' x2 = read_biom(min_sparse_file)
#' x3 = read_biom(rich_dense_file)
#' x4 = read_biom(rich_sparse_file)
#' x5 = read_biom(rich_dense_char)
#' x6 = read_biom(rich_sparse_char)
#' # Extract metadata
#' observation_metadata(x1)
#' observation_metadata(x2)
#' observation_metadata(x3)
#' observation_metadata(x3, 2:4)
#' observation_metadata(x3, 2)
#' observation_metadata(x3, c("GG_OTU_3", "GG_OTU_4", "whoops"))
#' observation_metadata(x4)
#' observation_metadata(x5)
#' observation_metadata(x6)
setGeneric("observation_metadata", function(x, rows, parallel=FALSE){
	standardGeneric("observation_metadata")
})
# All methods funnel toward signature biom,numeric
#' @aliases observation_metadata,biom,missing-method
#' @rdname observation_metadata-methods
setMethod("observation_metadata", c("biom", "missing"), function(x, rows, parallel=FALSE){
	# Dispatch with full rows and cols
	observation_metadata(x, 1:nrow(x), parallel)
})
#' @aliases observation_metadata,biom,character-method
#' @rdname observation_metadata-methods
setMethod("observation_metadata", c("biom", "character"), function(x, rows, parallel=FALSE){
	rows = which(sapply(x$rows, function(j) j$id) %in% rows)
	if( length(rows)==0 ){
		stop("The row ID names you provided do not match the row IDs in x")
	}
	# Dispatch with specified numeric rows
	observation_metadata(x, rows, parallel)
})
#' @rdname observation_metadata-methods
#' @aliases observation_metadata,biom,numeric-method
setMethod("observation_metadata", c("biom", "numeric"), function(x, rows, parallel=FALSE){
	if( any(rows > nrow(x)) ){
		warning(paste0("Row indices ",
									 paste0(rows[rows > nrow(x)], collapse=" "),
									 " are greater than available rows in data. They were ignored."))
		rows = rows[rows <= nrow(x)]
	}
	return(extract_metadata(x, "rows", rows, parallel))
})
################################################################################
# Generic internal function for extracting metadata from either rows or columns
#' @importFrom plyr ldply
#' @importFrom plyr llply
#' @keywords internal
extract_metadata = function(x, indextype, indices, parallel=FALSE){
	# Immediately extract just those index indicated by `index` argument
	metalist = x[[indextype]][indices]
	# Extract metadata elements as a list, for checking dimensions, NULL, etc.
	rx = llply(metalist, function(i) unlist(i$metadata), .parallel=parallel)
	if( all(sapply(rx, is.null)) ){
		# If there is no metadata (all NULL),
		# then set metadata to NULL, representing empty.
		metadata = NULL
	} else {
		# Else, extract names and metadata (both required)
		# Extract names
		metaids = sapply(metalist, function(i) i$id)
		# Test if length of metadata entries is same for all indices.
		rxlengths = sapply(rx, length)
		if( all( rxlengths == rxlengths[1]) ){
			# If so, can parse it as data.frame with ldply
			# return a data.frame with colnames
			metadata = ldply(rx, .parallel=parallel)
			rownames(metadata) <- metaids
		} else {
			# Else, should keep it as a list. But should name the entries
			metadata = rx
			names(metadata) <- metaids
		}
	}
	return(metadata)
}
################################################################################
# Generic internal function for generating the count matrix.
#' @keywords internal
generate_matrix <- function(x){
  indptr  = x$sample$matrix$indptr+1
  indices = x$sample$matrix$indices+1
  data    = x$sample$matrix$data
  nr = length(x$observation$ids)
 
  counts = sapply(2:length(indptr),function(i){
    x = rep(0,nr)
    seq = indptr[i-1]:(indptr[i]-1)
    x[indices[seq]] = data[seq]
    x
    })
  rownames(counts) = x$observation$ids
  colnames(counts) = x$sample$ids
  # I wish this next line wasn't necessary
  lapply(1:nrow(counts),function(i){
    counts[i,]
    })
}
################################################################################
# Generic internal function for generating the metadata.
#' @keywords internal
generate_metadata <- function(x){
  metadata = x$metadata
  metadata = lapply(1:length(x$ids),function(i){
      id_metadata = lapply(metadata,function(j){
        if(length(dim(j))>1){ as.vector(j[,i,drop=FALSE]) }
        else{ j[i] }
          })
      list(id = x$ids[i],metadata=id_metadata)
    })
  return(metadata)
}
################################################################################
# Generic internal function for generating a named list. 
#' @keywords internal
namedList <- function(...) {
    L <- list(...)
    snm <- sapply(substitute(list(...)),deparse)[-1]
    if (is.null(nm <- names(L))) nm <- snm
    if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
    setNames(L,nm)
}
################################################################################
