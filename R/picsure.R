#' picsure: A Client Library for PIC-SURE Networks.
#'
#' This package is used to connect to a PIC-SURE network and provide
#' connection services required by all resource-specific libraries/packages.
#'
#' @docType package
#' @name picsure
#'
NULL

#' Create a connection to a PIC-SURE network.
#'
#' @param url The url of the PIC-SURE endpoint.
#' @param token A security token to access the endpoint.
#' @param verbose (optional) set to TRUE to get debugging output
#'
#' @examples
#'
#'#   myconn = picsure::connect(url="http://your.pic-sure.server/PIC-SURE/", token="your-security-token")
#'
#' @export
connect <- function(url, token, psama_url=FALSE, verbose=FALSE) {
  deprecationMessage()
  result = PicSureConnection$new(url, token, psama_url)
  class(result) <- "PicSure_Connection"
  return (result)
}

#' Create a connection to a PIC-SURE All-in-one Local network.
#'
#' @param token A security token to access the endpoint.
#' @param verbose (optional) set to TRUE to get debugging output
#'
#' @examples
#'
#'#   myconn = picsure::connect(token="your-security-token")
#'
#' @export
connect_local <- function(token, verbose=FALSE) {
  deprecationMessage()
  result = PicSureConnection$new("http://wildfly:8080/pic-sure-api-2/PICSURE/", 
                                 token, 
                                 "http://wildfly:8080/pic-sure-auth-services/auth/")
  class(result) <- "PicSure_Connection"
  return (result)
}

#' Get a list of resource UUIDs for the passed PIC-SURE connection.
#'
#' @param connection An established PIC-SURE connection object.
#' @param verbose (optional) set to TRUE to get debugging output
#' @examples
#'
#'#   myconn = picsure::connect(url="http://your.pic-sure.server/PIC-SURE/", token="your-security-token")
#'#   picsure::list.resources(myconn)
#'
#' @export
list.resources <- function(connection, verbose=FALSE) {
  if (class(connection) == "PicSure_Connection") {
    return (connection$list())
  } else {
    message("Invalid connection was passed to list.resources() function")
    stop()
  }
}


#' Get the details of a PIC-SURE resource.
#'
#' @param connection A PIC-SURE connection object.
#' @param resourceUUID The UUID identity of a Resource hosted via the PIC-SURE connection.
#' @param verbose (optional) set to TRUE to get debugging output
#' @return A string containing details of the requested Resource.
#' @examples
#'
#'#   myconn = picsure::connect(url="http://your.pic-sure.server/PIC-SURE/", token="your-security-token")
#'#   resources = picsure::list.resources(myconn)
#'#   picsure::resource.details(connection=myconn, resources[0])
#'
#' @export
resource.details <- function(connection, resourceUUID, verbose=FALSE) {
  if (class(connection) == "PicSure_Connection") {
    return (connection$about(resourceUUID))
  } else {
    message("Invalid connection was passed to resource.details() function")
    stop()
  }
}

deprecationMessage = function() {
  message("This branch of the package is deprecated and will not be supported past August 31st, 2023. Please build this package from the main branch of 'hms-dbmi/pic-sure-r-adapter-hpds' for the updated functionality. If you have questions, feel free to contact our helpdesk: http://avillachlabsupport.hms.harvard.edu/. For example usage of the latest version of the package, see the examples here: https://github.com/hms-dbmi/Access-to-Data-using-PIC-SURE-API .")
}
