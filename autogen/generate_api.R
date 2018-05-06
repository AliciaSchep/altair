#
library(altair)
library(autopyr)
r_api <- generate_r_api(alt, prefix = "alt")
r_api <- gsub("Undefined", "alt$Undefined", r_api)
r_file_path <- file.path(rprojroot::find_package_root_file(), "R","r_api.R")
cat(r_api, file = r_file_path)
