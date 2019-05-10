# PIC-SURE Client Library


#' export
Client <- R6::R6Class("PicSureClient", 
                             portable = FALSE,
                             lock_objects = FALSE,                                 
                             public = list(
                               version = function() {
                                 print("PIC-SURE R Client Library (version 0.1.0)")
                               },
                               help = function() {
                                 writeLines("        [HELP] PicSureClient::Client$new()")
                                 writeLines("            $version()                      gives version information for library")
                                 writeLines("            $connect(<url>, <token>)        returns a connection object")
                             },
                               connect = function(url, token) {
                                 return(PicSureConnection$new(url, token))
                               }
                             )
)


#' export
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
                                  help = function() {
                                    writeLines("      [HELP] PicSureClient$connect(url, token)")
                                    writeLines("      $list()                         Prints a list of available resources")
                                    writeLines("      $about(resource_uuid)           Prints details about a specific resource")
                                    writeLines("")
                                    writeLines("      [Connect to Resource]")
                                    writeLines("      To connect to a resource load its associated resource code library")
                                    writeLines("      and then pass the API connection object (this object) to the")
                                    writeLines("      the library's Client object like this:")
                                    writeLines("")
                                    writeLines("            myPicSureClient <- PicSureClient::Client$new()")
                                    writeLines("            myPicSureConn <- myPicSureClient$connect(url, token)")
                                    writeLines("            myResourceAdapter <- PicSureHpdsLib::Adapter(myPicSureConn)")
                                    writeLines("            myResource <- myResourceAdapter$useResource(resource_uuid)")
                                    writeLines("            myResource$help()")
                                    writeLines("")
                                    writeLines("            * The above example connects to a HPDS resource.  Each resource has")
                                    writeLines("              a specific type which has its own adapter library.  Libraries will")
                                    writeLines("              follow the naming convention: \"PicSureXyzLib\" where \"Xyz\"")
                                    writeLines("              specifies the adapter's storage format.")
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

#' export
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
