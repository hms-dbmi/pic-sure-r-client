
# SIMPLE TEST SCRIPT

myurl <- "https://copdgene-dev.hms.harvard.edu/picsure"
mytoken <- ""


# make a connection to the PIC-SURE Network
myconnection <- picsure::connect(url=myurl, token=mytoken)
myconnection

# get a listing of the UUID's of all resources on the specified PIC-SURE Network
myresourcelist <- picsure::list.resources(connection=myconnection)
myresourcelist

# get the details of the first resource discovered
picsure::resource.details(connection=myconnection, resourceUUID=myresourcelist[[1]])




