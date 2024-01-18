getFreeMemoryGB <- function() {
  osName <- Sys.info()[["sysname"]]
  if (osName == "Windows") {
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    return(as.integer(x))
  } else if (osName == 'Linux') {
    x <- system2('free', args='-g', stdout=TRUE)
    x <- strsplit(x[2], " +")[[1]][4]
    return(as.integer(x))
  } else {
    stop("Only supported on Windows and Linux")
  }
}