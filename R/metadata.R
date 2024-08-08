get_field_safely <- function(parsed, field) {
  if (parsed$has_fields(field)) {
    return(parsed$get_field(field))
  } else {
    return(NA)
  }
}

compile_metadata <- function(gh_data, cran_data = NULL, description_contents = NULL) {
  if (!("github_metadata" %in% class(gh_data))) {
    stop("gh_data must be of type 'github_metadata'")
  }
  on_cran <- !is.null(cran_data)
  if (!on_cran) {
    if (is.null(description_contents)) {
      stop("If CRAN metadata is NULL, must provide DESCRIPTION contents")
    }
    parsed <- desc::desc(text = description_contents)
    title <- get_field_safely(parsed, "Title")
    description <- get_field_safely(parsed, "Description")
    license <- get_field_safely(parsed, "License")
    authors <- format(parsed$get_author(), include = c("given", "family"))
    maintainer <- parsed$get_maintainer()
    date_published <- NA
  } else {
    if (!("cran_metadata" %in% class(cran_data))) {
      stop("cran_data must be of type 'cran_metadata'")
    }
    title <- cran_data$title
    description <- cran_data$description
    license <- cran_data$license
    authors <- paste(unlist(cran_data$author), collapse = ", ")
    maintainer <- cran_data$maintainer
    date_published <- cran_data$date_published
  }
  ret <- list(name = gh_data$name,
              title = title,
              description = stringr::str_remove_all(description, "[\r\n]"),
              license = license,
              authors = paste(authors, collapse = ","),
              maintainer = maintainer,
              date_published = date_published,
              has_tests = gh_data$has_tests,
              has_vignettes = gh_data$has_vignettes,
              num_contributors = gh_data$num_contributors,
              on_cran = on_cran
  )
  class(ret) <- append("htahub_metadata", class(ret))
  ret
}

cran_metadata <- function(cran_db, name) {
  rows <- cran_db[cran_db$package == name,]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  row <- rows[1, ]
  ret <- list(name = row$package,
              title = row$title,
              description = row$description,
              license = row$license,
              authors = paste(unlist(row$author), collapse = ", "),
              maintainer = row$maintainer,
              date_published = row[["date/publication"]]
  )
  class(ret) <- append("cran_metadata", class(ret))
  ret
}

github_metadata <- function(name, owner, repo) {
  files_meta <- api_get_endpoint(owner = owner, repo = repo, endpoint = "contents")

  if (is.null(files_meta)) {
    logger::log_info(sprintf("Excluding package %s because not found on Github", name))
    return(NULL)
  }

  filenames <- sapply(files_meta, function(x) x$name)
  if (!has_description(filenames)) {
    logger::log_info(sprintf("Excluding package %s because it does not have a DECSRIPTION file", name))
    return(NULL)
  }

  extra_meta <- api_get_endpoints(owner = owner, repo = repo, endpoint = c("contributors", "commits"))

  logger::log_info(sprintf("Github metadata retrieved for package %s", name))
  ret <- list(has_tests = has_tests(filenames),
              has_vignettes = has_vignettes(filenames),
              name = name,
              url = paste("https://github.com", owner, repo, sep = "/"),
              num_contributors = length(extra_meta$contributors))
  class(ret) <- append("github_metadata", class(ret))
  ret
}
