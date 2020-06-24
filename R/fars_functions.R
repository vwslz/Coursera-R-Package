utils::globalVariables(c("year", "MONTH", "n", "STATE"))

#' Read data from a file at given path
#'
#' @param filename A string
#' @return The data frame from file at \code{filename}
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @note The function will stop if the file does not exist.
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
  filename = paste(getwd(), "/inst/extdata/", filename, sep = "")
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Construct the filename for a given year
#'
#' @param year A number
#' @return The string coresponding to \code{year}
#' @examples
#' \dontrun{make_filename(2018)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Get data for given year(s)
#'
#' @param years An list of number
#' @return The datafrome coresponding to given \code{years}
#' @importFrom dplyr mutate select %>%
#' @note The year could be invalid if file does not exist.
#' @examples
#' \dontrun{fars_read_years(c(2013, 2015))}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize data for given year(s)
#'
#' @param years An list of number
#' @return The data frame summarize data from given \code{years}
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{fars_summarize_years(c(2013,2014))}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Display accidents for a given state in a given year
#'
#' @param state.num A number
#' @param year A number
#' @return The plot of accidents for given \code{state.num} in \code{year}
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @note if state.num not exist in data, the function will stop.
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
