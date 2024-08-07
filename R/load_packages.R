#' @description Read packages from config/packages.csv, filtering to those with valid package names
#' @return A data.frame with 2 columns: name, url
get_packages <- function(path = "config/packages.csv") {
  packages <- read.csv(path,
                       header = FALSE,
                       col.names = c("name", "url"))
  packages <- packages[is_valid_name(packages$name),]
}

#' @description Get CRAN database. Uses cache if available.
#' @param use_cache Optional Logical. Default TRUE.
#' @return A data.frame of packages with columns "author","package","title","description","license",
#' "date/publication","maintainer","reverse_depends"
get_cran_db <- function(packages, use_cache = TRUE) {
  cran_db <- NULL
  if (use_cache & file.exists("cache/cran_db.rds")) {
    cached <- readRDS("cache/cran_db.rds")
    cran_db <- cached$cran_db
    if (!identical(cached$package_names, packages$name)) {
      cran_db <- NULL
    }
  }
  if (is.null(cran_db)) {
    logger::log_info("Package list has changed, or cache not available. Fetching fresh CRAN db.")
    cran_db <- tools::CRAN_package_db()
    cran_db <- cran_db[cran_db$Package %in% packages$name,]
    logger::log_info("Filtering CRAN database")
    cran_db <- cranly::clean_CRAN_db(cran_db)[, c("author",
                                                  "package",
                                                  "title",
                                                  "description",
                                                  "license",
                                                  "date/publication",
                                                  "maintainer",
                                                  "reverse_depends")]
    logger::log_info("Updating cache")
    if (!dir.exists("cache")) {
      dir.create("cache")
    }
    saveRDS(list(package_names = packages$name, cran_db = cran_db), "cache/cran_db.rds")
  }
  cran_db
}
