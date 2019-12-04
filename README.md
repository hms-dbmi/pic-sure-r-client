# pic-sure-r-client
Research users may use PIC-SURE API Base Client Library to connect to a PIC-SURE API and list resource instances and their metadata.

All operations on PIC-SURE rely upon two component libraries: The Connection Library, and a Datasource Adapter Library. The connection library manages network communication and authentication. It also provides basic functionality for PIC-SURE Resource Libraries to perform low level interactions with Resource Implementations.
## Installation
To use the R Connector Library, first install the necessary packages:

```R
library(devtools)

if (file.exists(Sys.getenv("TAR")) == FALSE)  {
    Sys.setenv(TAR = "/bin/tar")
}

devtools::install_github("hms-dbmi/pic-sure-r-client", force = TRUE)
devtools::install_github("hms-dbmi/pic-sure-r-adapter-hpds", force = TRUE)
```
## Usage
TBD
## Supported R Versions
TBD
## Additional Resources
* [PIC-SURE HPDS R Client] (https://github.com/hms-dbmi/pic-sure-r-adapter-hpds "PIC-SURE HPDS R Client")
