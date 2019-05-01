# PIC-SURE library


#' export
PicSureClient <- R6::R6Class("PicSureClient", 
                             portable = FALSE,
                             lock_objects = FALSE,                                 
                             public = list(
                               version = function() {
                                 print("PIC-SURE R Client Library (version 0.1.0)")
                               },
                               help = function() {
                                 writeLines("\n")
                                 writeLines("        [HELP] PicSureClient$new()")
                                 writeLines("            .version()                      gives version information for library")
                                 writeLines("            .connect(<url>, <token>)        returns a connection object")
                                 writeLines("\n")
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
                                    self$url <- url
                                    self$token <- token
                                  },
                                  help = function() {
                                    writeLines("      [HELP] PicSureClient.Client.connect(url, token)")
                                    writeLines("      .list()                         Prints a list of available resources")
                                    writeLines("      .about(resource_uuid)           Prints details about a specific resource")
                                    writeLines("")
                                    writeLines("      [Connect to Resource]")
                                    writeLines("      To connect to a resource load its associated resource code library")
                                    writeLines("      and then pass the API connection object (this object) to the")
                                    writeLines("      the library's Client object like this:")
                                    writeLines("")
                                    writeLines("            myPicSureClient <- PicSureClient$new()")
                                    writeLines("            myPicSureConn <- myPicSureClient$connect(url, token)")
                                    writeLines("            myResourceAdapter <- PicSureHpdsAdapter(myPicSureConn)")
                                    writeLines("            myResource <- myResourceAdapter$useResource(resource_uuid)")
                                    writeLines("            myResource$help()")
                                    writeLines("")
                                    writeLines("            * The above example connects to a HPDS resource.  Each resource has")
                                    writeLines("              a specific type which has its own adapter library.  Libraries will")
                                    writeLines("              follow the naming convention: \"PicSureXyzLib\" where \"Xyz\"")
                                    writeLines("              specifies the adapter's storage format.")
                                    writeLines("")
                                  },
                                  about = function(resourceId = NA) {
                                    # TODO: implement this
                                    writeLines("Connection$about()\n")
                                  },
                                  list = function() {
                                    writeLines("Connection$list()\n")
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
                                      search = function(resource_uuid, query) { writeLines(c(resource_uuid, query)) },
                                      asynchQuery = function(resource_uuid, query) { writeLines(c(resource_uuid, query)) },
                                      synchQuery = function(resource_uuid, query) { writeLines(c(resource_uuid, query)) },
                                      queryStatus = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) },
                                      queryResult = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) }
                                    )
)
