#' @title Data Bank Initialization Function
#' @description Initializes various data structures for storing and managing data within a complex systems analysis framework.
#
#' @param type A character string specifying the type of data structure to initialize: "array", "vector", "matrix", or
#' "neighborhood memories". Each structure serves different requirements for data storage and processing in the model.
#' @param dimensions An integer vector specifying the dimensions of the data structure. The interpretation of dimensions
#' varies based on the type:
#' - For "array", it defines the dimensions of the array.
#' - For "vector", only the first element (length) is used.
#' - For "matrix", the first two elements specify the number of rows and columns.
#' - For "neighborhood memories", the first two elements define the number of rows and columns,
#'   the third element specifies the number of nearest neighbors, and the fourth element details the number
#'   of signature components per neighbor.
#'
#' @return db Returns the initialized data structure, filled with NA values. Depending on the 'type', the structure
#' can be an array, vector, matrix, or a specialized data frame designed for neighborhood memories which incorporates
#' extensive details about interactions within defined neighborhoods.
#'
#' @examples
#' # Initialize a matrix with 3 rows and 5 columns.
#' matrix_db <- dataBank("matrix", c(3, 5))
#' print(matrix_db)
#'
#' # Initialize a neighborhood memory structure correctly with sufficient column names.
#' dimensions_nm <- c(4, 40, 3, 5) # 4 rows, 40 columns, 3 neighbors, 5 signature components per neighbor
#' nm_db <- dataBank("neighborhood memories", dimensions_nm)
#' print(nm_db)


#= Initializing Data Structures
dataBank <- function(type,dimensions) {
  if (type=="array") {
    db <- array(NA,dim = dimensions)
  } else if (type=="vector") {
    db <- vector(mode = "double",length = dimensions[1])
    db <- rep(NA,length(db))
  } else if (type=="matrix") {
    db <- matrix(data = NA,nrow=dimensions[1],ncol=dimensions[2])
  } else if (type=="neighborhood memories") {
    if(dimensions[2] != 1+4*dimensions[3]+(dimensions[4]-1)*dimensions[3]+dimensions[4]*dimensions[3]){
      stop("The dimensions 2 is wrong!")
    } else{
      db <- as.data.frame(matrix(data = NA,nrow=dimensions[1],ncol=dimensions[2]))
      colnames(db) <- c("i",rep("nn-times",dimensions[3]),rep("nn-dists",dimensions[3]),
                        rep("nn-weights",dimensions[3]),rep("nn-patt",dimensions[3]),
                        paste(rep(paste("Sig-Comp.", 1:(dimensions[4]-1)),dimensions[3]),
                              rep(1:dimensions[3], each = dimensions[4]-1),
                              sep = " of NN"),
                        paste(rep(paste("Coord.", 1:dimensions[4]),dimensions[3]),
                              rep(1:dimensions[3], each = dimensions[4]),
                              sep = " of NN"))
    }
  } 
  return(db)
}