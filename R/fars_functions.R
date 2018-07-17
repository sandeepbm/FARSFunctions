#' Read and convert file to data frame
#'
#' This function reads a csv file and converts to a tibble data frame
#'
#' @param filename Name of the input csv file
#' @return If input file doesnt exist, this function will stop execution
#' and return error else it will return tibble data frame object of input file
#'
#' @examples 
#' fars_read("accident_2018.csv.bz2")
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @export
fars_read <- function(filename) {
      if(!file.exists(filename))
            stop("file '", filename, "' does not exist")
      data <- suppressMessages({
            readr::read_csv(filename, progress = FALSE)
      })
      dplyr::tbl_df(data)
}

#' Generate file name from input numeric value
#'
#' This function reads a numeric value and creates a file name string
#'
#' @param year Numeric value of year
#' @return This function returns a string value of file name
#'
#' @examples 
#' make_filename(2018)
#' 
#' @export
make_filename <- function(year) {
      year <- as.integer(year)
      sprintf("accident_%d.csv.bz2", year)
}

#' Consolidate month & year data from multiple input files
#'
#' This function extracts and consolidates month and year fields from
#' multiple input files where each input file contains data for 1 year
#'
#' @param years A numeric vector of year values for which data is required
#' @return If input file doesnt exist for any corresponding input year a warning 
#' will be returned indicating that the input year is invalid. 
#' For all valid years, a list containing month and year fields of each input 
#' year is returned.
#'
#' @examples 
#' fars_read_years(c(2017,2018))
#' fars_read_years(2018)
#' 
#' @importFrom dplyr mutate select
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

#' Summarize month & year data from multiple input files
#'
#' This function summarizes consolidated month and year fields from
#' multiple input files where each input file contains data for 1 year
#'
#' @param years A numeric vector of year values for which data is required
#' @return This functions returns a tibble data frame giving counts summarized
#' by month and year, where the intersection of a row representing month
#' and column representing year, gives summarized counts for that particular 
#' month and year
#'
#' @examples 
#' fars_summarize_years(c(2017,2018))
#' fars_summarize_years(2018)
#' 
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' 
#' @export
fars_summarize_years <- function(years) {
      dat_list <- fars_read_years(years)
      dplyr::bind_rows(dat_list) %>% 
            dplyr::group_by(year, MONTH) %>% 
            dplyr::summarize(n = n()) %>%
            tidyr::spread(year, n)
}

#' Plot of accident coordinates for given state and year on state map
#'
#' This function plots coordinates represented by latitude and longitude for
#' a given input state and year
#'
#' @param state.num A numeric value representing state in FARS dataset
#' @param year A numeric value of year
#' @return This functions returns a map of the input state with accident
#' cooridnates plotted onto the map.
#' Latitude greator than 90 and longitude 900 is not plotted
#' If input state is not found in dataset, error is returned with message as
#' "invalid state number <state number>"
#' If no accidents are present in dataset for given state, a message is returned
#' as "no accidents to plot"
#'
#' @examples 
#' fars_map_state(1,2018)
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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

