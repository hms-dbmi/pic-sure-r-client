#' picsure: A Client Library for PIC-SURE Networks.
#'
#' This package is used to connect to a PIC-SURE network and provide
#' connection services required by all resource-specific libraries/packages.
#'
#' @docType package
#' @name picsure
#'
NULL


#' Get the details of a PIC-SURE resource.
#'
#' @param connection A PIC-SURE connection object.
#' @param resourceUUID The UUID identity of a Resource hosted via the PIC-SURE connection.
#' @param verbose (optional) set to TRUE to get debugging output
#' @return A string containing details of the requested Resource.
resource.details <- function(connection, resourceUUID, verbose=FALSE) {
  message("not finished")
  stop()
}


#' Get a new instance reference of a PIC-SURE resource.
#'
#' @param connection A PIC-SURE connection object.
#' @param resourceUUID The UUID identity of a Resource hosted via the PIC-SURE connection.
#' @param verbose (optional) set to TRUE to get debugging output
#' @return An object which provides access to the requested Resource.
get.resource <- function(connection, resourceUUID, verbose=FALSE) {
  message("not finished")
  stop()
}
