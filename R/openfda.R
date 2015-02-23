library(jsonlite)
library(memoise)
library(magrittr)
library(ggplot2)

#' @importFrom jsonlite fromJSON
#' @importFrom memoise memoise
#' @importFrom ggplot2 qplot
#' @importFrom magrittr %>%
NULL

#' Pipe operator for chaining together operations.
#' 
#' Imported from magrittr, use this to chain together
#' operations in a natural way.
#' 
#' @examples
#' 
#' # instead of
#' a(b(c("hello")), "bob")
#' 
#' # we can write:
#' 
#' c("hello") \%>\% b() \%>\% a("bob")
#' 
#' # this also allows for currying common arguments:
#' 
#' my_query <- 
#'    fda_query("/drug/event.json") \%>\%
#'    fda_api_key("ABC") 
#' 
#' @aliases chain_query
#' @rdname chain_query
#' @name %>%
#' @export
NULL

#' Make a copy of query (keeps the class).
copy_query = function(query) {
  query = lapply(query, FUN=identity)
  class(query) = "fda_query"
  query
}

FDA_DEBUG = TRUE

#' Turn off/on API debugging.
#' 
#' When set to TRUE, this will print additional debugging information from
#' the API, such as URLs being fetched.
#' @export
fda_debug <- function(on) {
  FDA_DEBUG <<- on
}

fetch_ <- memoize(fromJSON)

#' Fetch the given URL as JSON.
#' 
#' This uses jsonlite to fetch the URL.  The result is coerced into
#' a data frame.
#' 
#' @return data.frame
#' @export
fda_fetch <- function(url, catch_errors=TRUE) {
  if (FDA_DEBUG) {
    cat("Fetching:", url, "\n")
  }
  
  if (catch_errors) {
    result = try(fetch_(url), silent=catch_errors)
    if (class(result) == "try-error") {
      return(data.frame(results=c()))
    } else {
      result
    }
  } 
  
  fromJSON(url)
}

#' Create a new query.
#'
#' Queries can be extended with filters (using fda_filter).
#' They should be terminated using either \code{search} or
#' \code{count(field)}.
#' @param base The API endpoint to query: \itemize{
#'   \item /drug/event.json
#'   \item /drug/enforcement.json
#'   \item /device/event.json
#'   \item /food/event.json
#'  }
#'  
#' @return fda_query
#' @export
fda_query <- function(base) {
  q <- list()
  q$base = base
  q$limit = FALSE
  q$key = FALSE
  q$count = FALSE
  q$filters = vector("character")
  q$operation = function(q) {
    print("No operation defined (try fda_count or fda_search)!")
    q
  }
  class(q) = "fda_query"
  q
}

#' @export
print.fda_query <- function(q) {
  print(paste("fda_query(", fda_url(q), ")", sep=""))
}

#' Apply a filter to a query
#'
#' @param name The field to filter on (e.g. "patient.patientonsetage")
#' @param value A value or range to filter on (e.g. "[1+TO+40]")
#' 
#' @return fda_query
#' @export
fda_filter <- function(q, name, value) {
  q = copy_query(q)
  q$filters = c(q$filters, paste(name, value, sep=":"))
  q
}

#' Set the number of results desired by this query.
#' 
#' The default number of results returned by the API
#' is 1 (for search requests) and 100 (for count requests).
#' 
#' @return fda_query
#' @export
fda_limit <- function(q, limit) {
  q = copy_query(q)
  q$limit = limit
  q
}

#' Attach an API key to the query.
#' 
#' All requests made to the API will have this API key
#' specified.
#' 
#' @return fda_query
#' @export
fda_api_key <- function(q, key) {
  q = copy_query(q)
  q$key = key
  q
}

#' Return the URL that will be fetched for the current query.
#'
#' @return character The url that will be fetched
#' @export
fda_url <- function(q) {
  q = copy_query(q)
  search <- paste("search", paste(q$filters, collapse="+AND+"), sep="=")
  
  args = c(search);

  if (q$key != FALSE) {
    args = c(args, paste("api_key", q$key, sep="="))
  }
  
  if (q$limit != FALSE) {
    args = c(args, paste("limit", q$limit, sep="="))
  }
  
  if (q$count != FALSE) {
    args = c(args, paste("count", q$count, sep="="))
  }
  
  args = paste(args, collapse="&", sep="")
  
  url = paste("https://api.fda.gov", q$base, sep="")
  url = paste(url, args, sep="?")
  return(url)
}

#' Plot a count query.
#' 
#' This is just a convenience function which takes the 
#' data frame output from fda_exec and gives it to qplot.

#' @return ggplot2
#' @export
fda_plot <- function(df) {
  qplot(term, count, data=df)
}

extract_ <- function(obj, path) {
  if (length(path) == 0) {
    return(obj)
  }
  field = path[1]
  rest = path[-1]
  obj = obj[[field]]
  extract(obj, rest)
}

#' Fetch a (nested field) from a list or dataframe.
#' 
#' @export
extract_field <- function(obj, path) {
  path = unlist(strsplit(path, ".", fixed=TRUE))
  print(path)
  extract_(obj, path)
}

#' Fetch search results.
#' 
#' When combined with \code{fda_exec}, this query will 
#' return a data frame consisting of the matching records.
#' 
#' @param field The field to extract.  This should be a 
#'   dot (.) delimited path to follow to extract a field:
#'   e.g. 'a.b.c.d', would perform the equivalent of 
#'   running a[[b]][[c]][[d]]
#' @return fda_query
#' @export
fda_search <- function(q, field=FALSE) {
  q = copy_query(q)
  q$count = FALSE
  q$operation = function(q) {
    url = fda_url(q);
    result = fda_fetch(url)$result
    if (field == FALSE) {
      return(result)
    } else {
      return(extract_field(result, field))
    }
  }
  
  q
}


#' Count results from a given field.
#' 
#' Returns a new query which will count the given field when
#' handed to \code{fda_exec}
#' 
#' @return fda_query
#' @export
fda_count <- function(q, field) {
  q = copy_query(q)
  q$limit = FALSE
  q$count = field
  
  #' Executes a count operation for a query
  q$operation <- function(q) {
    url = fda_url(q)
    json = fda_fetch(url)
    json$result
  }
  
  q
}

#' Execute a query.
#' 
#' This actually sends a request to the OpenFDA API servers
#' which incorporates any filters or keys specified in the query
#' and returns the result as a data frame.
#' 
#' @return data.frame containing API results
#' @export
fda_exec <- function(q) {
  q$operation(q)
}

#' openfda: A package for interfacing to the OpenFDA API
#' 
#' This package provides a simple wrapper around the OpenFDA API.
#' 
#' It uses the \code{magrittr} piping interface to simplify 
#' building complex queries.  
#' 
#' @examples
#' # Queries generally have the following format
#' fda_query(endpoint) %>%
#'    fda_filter(field, value) %>%
#'    fda_count(field) OR fda_search() %>%
#'    fda_exec()
#'
#' @docType package
#' @name openfda
NULL

