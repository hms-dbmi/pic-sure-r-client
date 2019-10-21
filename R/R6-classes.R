#' R6 class used to establish connections to a PIC-SURE network.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for connecting to PIC-SURE network.
#' @format \code{\link{PicSureClient}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-client}
#'   \item{\code{new()}}{This method is used to create object of this class.}
#'
#'   \item{\code{version()}}{This method prints the current package version.}
#'   \item{\code{connect(url, token)}}{This method returns a new \code{PicSureConnection} object configured to connect to a PIC-SURE Network at the given \code{url} and using the provided \code{token} for security.}}
PicSureClient <- R6::R6Class("PicSureClient",
                      portable = FALSE,
                      lock_objects = FALSE,
                      public = list(
                        version = function() {
                          "Prints the version of the PIC-SURE Client Library"
                          print("PIC-SURE R Client Library (version 0.1.0)")
                        },
                        connect = function(url, token) {
                          "Connects to a PIC-SURE network."
                          return(PicSureConnection$new(url, token))
                        }
                      )
)


#' R6 class that represents an establish connection to a PIC-SURE network.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for connecting to PIC-SURE network.
#' @format \code{\link{PicSureConnection}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-client}
#'   \item{\code{new(url, token)}}{This method is used to create object of this class.}
#'
#'   \item{\code{about(resourceId = FALSE)}}{This method... }
#'   \item{\code{list()}}{This method... }
#'   \item{\code{getInfo()}}{This method... }
#'   \item{\code{getResources()}}{This method... }
#'   \item{\code{INTERNAL_api_obj(url, token)}}{This is an internal method used to retreve a \code{PicSureConnectionAPI} object which handles all communication/authentication with the PIC-SURE Network for a Resource-level package.}}
PicSureConnection <- R6::R6Class("PicSureConnection",
                                 portable = FALSE,
                                 lock_objects = FALSE,
                                 public = list(
                                   initialize = function(url, token) {
                                     # TODO: trim and make sure URL ends in "/"
                                     endpoint <- str_trim(url)
                                     if (str_detect(endpoint, "/$") == FALSE) {
                                       endpoint <- paste(endpoint, "/", sep="")
                                     }
                                     self$url <- endpoint
                                     self$token <- token
                                   },
                                   about = function(resourceId = FALSE) {
                                     # TODO: implement this
                                     urlstr = paste(self$url, "info", sep="")
                                     if (resourceId != FALSE) {
                                       urlstr = paste(urlstr, resourceId, sep="/")
                                     } else {
                                       urlstr = paste(urlstr, "resources", sep="/")
                                     }
                                     request = GET(urlstr, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                     if (request$status_code != 200) {
                                       writeLines("ERROR: HTTP response was bad")
                                       print(request)
                                       return('{"results":{}, error":"True"}')
                                     } else {
                                       return(content(request, "text"))
                                     }
                                   },
                                   list = function() {
                                     writeLines("Connection$list()\n")
                                     entries <- jsonlite::fromJSON(self$about(), auto_unbox=TRUE)
                                     for (rec in entries) {
                                       writeLines(c(rec$uuid, " -> ", rec$name), sep="")
                                     }
                                   },
                                   getInfo = function() {
                                     writeLines("Connection$getInfo()\n")
                                   },
                                   getResources = function() {
                                     writeLines("Connection$getResources()\n")
                                   },
                                   INTERNAL_api_obj = function() {
                                     return(PicSureConnectionAPI$new(self$url, self$token))
                                   }
                                 )
)

#' R6 class used by other Resource-oriented packages to communicate in a standard
#' way with a Resource hosted on a PIC-SURE network.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for connecting to PIC-SURE network.
#' @format \code{\link{PicSureConnectionAPI}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-client}
#'   \item{\code{new(url, token)}}{This method is used to create object of this class.}
#'
#'   \item{\code{info(resource_uuid)}}{This method is used by other packages...}
#'   \item{\code{search(resource_uuid, query)}}{This method is used by other packages...}
#'   \item{\code{asynchQuery(resource_uuid, query)}}{This method is used by other packages...}
#'   \item{\code{synchQuery(resource_uuid, query)}}{This method is used by other packages...}
#'   \item{\code{queryStatus(resource_uuid, query_uuid)}}{This method is used by other packages...}
#'   \item{\code{queryResult(resource_uuid, query_uuid)}}{This method is used by other packages...}}
PicSureConnectionAPI <- R6::R6Class("PicSureConnectionAPI",
                                    portable = FALSE,
                                    lock_objects = FALSE,
                                    public = list(
                                      initialize = function(url, token) {
                                        # TODO: trim and make sure URL ends in "/"
                                        self$url <- url
                                        self$token <- token
                                      },
                                      info = function(resource_uuid) { writeLines(resource_uuid) },
                                      search = function(resource_uuid, query) {
                                        full_url = paste(self$url, "search", "/", resource_uuid, sep="")
                                        if (query == FALSE) {
                                          query <- list()
                                          query$query <- ""
                                          query = jsonlite::toJSON(query, auto_unbox=TRUE)
                                        }
                                        request = POST(full_url, body=query, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                        if (request$status_code != 200) {
                                          writeLines("ERROR: HTTP response was bad")
                                          print(request)
                                          return('{"results":{}, error":"True"}')
                                        } else {
                                          return(content(request, text))
                                        }
                                      },
                                      asynchQuery = function(resource_uuid, query) {
                                        writeLines(c(resource_uuid, query))
                                      },
                                      synchQuery = function(resource_uuid, query) {
                                        full_url = paste(self$url, "query/sync/", resource_uuid, sep="")
                                        if (query == FALSE) {
                                          query <- list()
                                          query$query <- ""
                                          query = jsonlite::toJSON(query, auto_unbox=TRUE)
                                        }
                                        request = POST(full_url, body=query, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                        if (request$status_code != 200) {
                                          writeLines("ERROR: HTTP response was bad")
                                          print(request)
                                          return('{"results":{}, error":"True"}')
                                        } else {
                                          return(content(request, text))
                                        }
                                      },
                                      queryStatus = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) },
                                      queryResult = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) }
                                    )
)
