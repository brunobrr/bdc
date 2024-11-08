#' Internal function: Install gnparser software
#'
#' Installs the latest version of the gnparser software, compatible with Windows, macOS, and Linux.
#'
#' @importFrom here here
#' @importFrom rvest html_element html_text2
#' @importFrom xml2 read_html
#'
#' @return None
#' @noRd
#' @examples
#' \dontrun{
#' bdc_install_gnparser()
#' }
bdc_install_gnparser <- function() {
  
  # Get the latest version of gnparser
  get_latest_gnparser_version <- function() {
    url <- "https://github.com/gnames/gnparser/releases"
    
    # Reads the content of the releases page
    page <- xml2::read_html(url)
    
    # Extracts the first tag containing the version, using a CSS selector
    version <- page %>%
      rvest::html_element(".Link--primary") %>%
      rvest::html_text2()
    
    version <- gsub("v", "", version)
    return(version)
  }
  
  # Obtains the latest version of gnparser
  latest_version <- get_latest_gnparser_version()
  message("Latest version of gnparser: ", latest_version)
  
  # Identifies the operating system
  os <- tolower(Sys.info()["sysname"])
  
  # Sets the target directory path and the executable file for each operating system
  target_dir <- switch(os,
                       linux = "~/bin",
                       # darwin = "/usr/local/bin",
                       darwin = "/opt/homebrew/bin",
                       windows = Sys.getenv("AppData"))
  
  exec_file <- ifelse(os == "windows", "gnparser.exe", "gnparser")
  exec_path <- file.path(target_dir, exec_file)
  
  # Checks if the latest version executable file already exists
  if (file.exists(exec_path) && grepl(latest_version, exec_path)) {
    message("The latest version of gnparser is already installed in: ", target_dir)
    return(invisible(NULL)) # Exits the function if the latest version is already installed
  }
  
  
  # Defines download URLs for each operating system
  link <- "https://github.com/gnames/gnparser/releases/download/"
  gnparser_urls <- list(
    linux = paste0(link, "v", latest_version, "/gnparser-v", 
                   latest_version, "-linux-arm.tar.gz"),
    darwin = paste0(link, "v", latest_version, "/gnparser-v", 
                    latest_version, "-mac-arm.tar.gz"),
    windows = paste0(link, "v", latest_version, "/gnparser-v", 
                     latest_version, "-win-arm.zip")
  )
  
  
  # Checks if the operating system is supported
  if (!os %in% names(gnparser_urls)) {
    stop("Unsupported operating system.")
  }
  
  # Sets the destination filename
  destfile <-
    switch(
      os,
      linux = file.path(tempdir(), "gnparser.tar.gz"),
      darwin = file.path(tempdir(), "gnparser.tar.gz"),
      windows = file.path(tempdir(), "gnparser.zip")
    )
  
  # Downloads the appropriate file
  download.file(url = gnparser_urls[[os]], destfile = destfile)
  
  # Extracts the file in a temporary directory
  temp_extract_dir <- tempdir()
  if (os == "windows") {
    unzip(destfile, exdir = temp_extract_dir)
  } else {
    untar(destfile, exdir = temp_extract_dir)
  }
  
  # Moves the executable to the target directory
  file.copy(file.path(temp_extract_dir, exec_file), target_dir, overwrite = TRUE)
  
  # Checks if the executable file was installed correctly
  if (file.exists(exec_path)) {
    message("gnparser successfully installed in: ", target_dir)
  } else {
    warning("gnparser installation failed.")
  }
}