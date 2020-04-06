library(jsonlite)
library(stringr)
library(httr)
library(urltools)


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
#' @import stringr
#' @import httr
#' @import urltools
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
                                     url_df = urltools::url_parse(url)
                                     url_df$path <- str_trim(url_df$path)
                                     if (isFALSE(str_detect(url_df$path, "/$"))) {
                                       url_df$path <- paste(url_df$path, "/", sep="")
                                     }
                                     self$url_picsure = urltools::url_compose(url_df)
                                     temp_path = str_split(url_df$path, "/")
                                     url_len = length(temp_path)
                                     temp_path[[1]][[url_len]] = "psama"
                                     url_df$path = str_flatten(temp_path[[1]], collapse="/")
                                     self$url_psama = urltools::url_compose(url_df)
                                     self$token <- token
                                     self$resource_uuids = self$list()
                                     print(self$resource_uuids)
                                   },
                                   about = function(resourceId = FALSE) {
                                     urlstr = paste(self$url_picsure, "info", sep="")
                                     if (!(isFALSE(resourceId))) {
                                       urlstr = paste(urlstr, resourceId, sep="/")
                                     } else {
                                       urlstr = paste(urlstr, "resources", sep="/")
                                     }
                                     request = GET(urlstr, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                     if (request$status_code != 200) {
                                       if (request$status_code == 401) {
                                         stop("ERROR: Bad security credentials.")
                                       } else {
                                         print(request)
                                         stop("ERROR: HTTP(S) Failed")
                                       }
                                     } else {
                                       return(content(request, type="text", encoding = "UTF-8"))
                                     }
                                   },
                                   list = function() {
                                     entries <- jsonlite::fromJSON(self$about())
                                     return (entries)
                                   },
                                   INTERNAL_api_obj = function() {
                                     return(PicSureConnectionAPI$new(self$url_picsure, self$url_psama, self$token))
                                   }
                                 )
)

#' R6 class used by other Resource-oriented packages to communicate in a standard
#' way with a Resource hosted on a PIC-SURE network.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import stringr
#' @import httr
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for connecting to PIC-SURE network.
#' @format \code{\link{PicSureConnectionAPI}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-client}
#'   \item{\code{new(url, token)}}{This method is used to create object of this class.}
#'
#'   \item{\code{profile()}}{This method is used by other packages...}
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
                                      initialize = function(url_picsure, url_psama, token) {
                                        self$url_picsure = url_picsure
                                        self$url_psama = url_psama
                                        self$token = token
                                      },
                                      profile = function() {
                                        #if (resource_uuid == FALSE) {
                                        #  writeLines("ERROR: no resource_uuid was given when retreving PSAMA profile")
                                        #} else {
                                          temp_url = paste(self$url_psama, "user/me", sep="")
                                          request = GET(temp_url, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                          if (request$status_code != 200) {
                                            writeLines("ERROR: HTTP response was bad when retreving PSAMA profile")
                                            print(request)
                                            return('{"results":{}, "error":"True"}')
                                          } else {
                                            ret_val = jsonlite::fromJSON(content(request, type="text", encoding="UTF-8"))
                                            # add queryTemplate info to profile info
                                            temp_url = paste(self$url_psama, "user/me/queryTemplate", sep="")
                                            request = GET(temp_url, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                            if (request$status_code != 200) {
                                              writeLines("ERROR: HTTP response was bad when retreving PSAMA queryTemplate")
                                              print(request)
                                              return(jsonlite::toJSON(ret_val))
                                            } else {
                                              # put template into profile() response
                                              ret_val$queryTemplate = jsonlite::fromJSON(content(request, type="text", encoding="UTF-8"))$queryTemplate
                                              return(jsonlite::toJSON(ret_val))
                                            }
                                          }
                                        #}
                                      },
                                      info = function(resource_uuid = FALSE) {
                                        urlstr = paste(self$url_picsure, "info", sep="")
                                        if (!(isFALSE(resourceId))) {
                                          urlstr = paste(urlstr, resourceId, sep="/")
                                        } else {
                                          urlstr = paste(urlstr, "resources", sep="/")
                                        }
                                        request = GET(urlstr, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                        if (request$status_code != 200) {
                                          writeLines("ERROR: HTTP response was bad")
                                          print(request)
                                          return('{"results":{}, "error":"True"}')
                                        } else {
                                          return(content(request, type="text", encoding="UTF-8"))
                                        }
                                      },
                                      search = function(resource_uuid, query) {
                                        full_url = paste(self$url_picsure, "search", "/", resource_uuid, sep="")
                                        if (isFALSE(query)) {
                                          query <- list()
                                          query$query <- ""
                                          query = jsonlite::toJSON(query, auto_unbox=TRUE)
                                        }
                                        request = POST(full_url, body=query, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                        if (request$status_code != 200) {
                                          writeLines("ERROR: HTTP response was bad")
                                          print(request)
                                          return('{"results":{}, "error":"True"}')
                                        } else {
                                          return(content(request, type="text", encoding="UTF-8"))
                                        }
                                      },
                                      asynchQuery = function(resource_uuid, query) {
                                        writeLines(c(resource_uuid, query))
                                      },
                                      synchQuery = function(resource_uuid, query) {
                                        full_url = paste(self$url_picsure, "query/sync/", sep="")
                                        if (isFALSE(query)) {
                                          query <- list()
                                          query$query <- ""
                                          query = jsonlite::toJSON(query, auto_unbox=TRUE)
                                        }
                                        request = POST(full_url, body=query, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                        if (request$status_code != 200) {
                                          writeLines("ERROR: HTTP response was bad")
                                          print(request)
                                          return('{"results":{}, "error":"True"}')
                                        } else {
                                          return(content(request, type="text", encoding="UTF-8"))
                                        }
                                      },
                                      queryStatus = function(resource_uuid, query_uuid) { 
                                        urlstr = paste(self$url_picsure, "query/", query_uuid, "/status", sep="")
                                        request = POST(urlstr, body="{}", content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                        if (request$status_code != 200) {
                                          writeLines("ERROR: HTTP response was bad")
                                          print(request)
                                          return('{"results":{}, "error":"True"}')
                                        } else {
                                          return(content(request, type="text", encoding="UTF-8"))
                                        }
                                      },
                                      queryResult = function(resource_uuid, query_uuid) { 
                                        urlstr = paste(self$url_picsure, "query/", query_uuid, "/result", sep="")
                                        request = POST(urlstr, body="{}", content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                        if (request$status_code != 200) {
                                          writeLines("ERROR: HTTP response was bad")
                                          print(request)
                                          return('{"results":{}, "error":"True"}')
                                        } else {
                                          return(content(request, type="text", encoding="UTF-8"))
                                        }
                                      }
                                    )
)
