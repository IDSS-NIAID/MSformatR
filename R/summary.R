#' summarizeMS
#' Main function to run MSformatR
#'
#' @param input Character vector specifying the location of the data to be summarized. By default all MA output in the current working directory will be used. Use of unix-style path seperators, /, is required. R will check the system and replace them with the correct path separator if running on Windows (see Details for additional information).
#' @param format Character value specifying the source of the raw data. Default is 'peaks11' (see Details).
#' @param qmd Character value specifying the path to supporting quarto documents. Default value is the current working directory (see Details).
#'
#' @details
#' When values are not supplied for function parameters, `summarizeMS` will look in the current working directory for a file called "config.yml" to supply these parameters. If no yaml config file is found, the default values specified here will be used instead.
#'
#' The path(s) for `input` can be specified using one of three formats:
#'
#' - An array of input directories (example yaml entry: `input: ["data/A", "data/B", "data/C"]`)
#' - A single path (example yaml entry: `input: "data/raw_results"`)
#' - A path to a directory containing multiple files to be summarized together. This should end in an `*`. (example yaml entry: `input: "data/*"`)
#'
#' Supported formats:
#'
#' - "peaks11" (default): html output from Peaks 11. Expected values for input are directory paths, each of which should contain multiple html files, csv files, and a directory of images.
#'
#' If no qmd directory is provided, MSformatR will look for supporting quarto documents in the current working directory. If the required supporting files are not found, it will attempt to download them from the internet.
#'
#' @export
#' @import rlang
#' @importFrom yaml read_yaml
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_sub
#' @importFrom readr read_csv
#'
#' @importFrom utils download.file
#'
#' @importFrom htmltools div
#' @importFrom reactable reactable
#' @importFrom reactable colDef
#' @importFrom quarto quarto_render
summarizeMS <- function(input = NULL, format = NULL, qmd = NULL)
{
  ##########
  # config #
  ##########

  config <- list(input  = input,
                 format = format,
                 qmd    = qmd)


  # look for config file and fill in any missing information
  if(file.exists('config.yml'))
  {
    tmp <- read_yaml('config.yml')

    for(i in names(config))
    {
      if(is.null(config[[i]]) & !is.null(tmp[[i]]))
        config[[i]] <- tmp[[i]]
    }
  }


  ##### Default configurations #####
  if(is.null(config$input))
    config$input <- file.path(getwd(), '*')

  if(is.null(config$format))
    config$format <- 'peaks11'

  if(is.null(config$qmd))
    config$qmd <- getwd()

  # look for required supporting files - if not found, use installed version
  if(!file.exists(file.path(config$qmd, 'summary.qmd')))
    file.copy(system.file('quarto/summary.qmd', package = 'MSformatR'),
              file.path(config$qmd, 'summary.qmd'))


  ##### Determine input files #####

  if(str_sub(config$input[1], start = -1) == "*")
  {
    if(length(config$input) > 1)
      stop("Expected values for input are a vector of paths, a single path, or a single path with a wild card at the end of the path, '*'.")

    config$input <- gsub('*', '', config$input, fixed = TRUE) %>%
      strsplit('/') %>%
      unlist() %>%
      paste(collapse = .Platform$file.sep) %>%
      list.dirs(recursive = FALSE)
  }


  #################
  # Process input #
  #################
  for(i in config$input)
  {

    proteins <- file.path(i, 'db.proteins.csv') %>%
      read_csv() %>%
      filter(!grepl('#CONTAM', .data$Accession, fixed = TRUE))

    peptides <- file.path(i, 'db.protein-peptides.csv') %>%
      read_csv() %>%
      filter(!grepl('#CONTAM', .data$Accession, fixed = TRUE))

    summary_table <- reactable(proteins, details = function(i) {
      retval <- filter(peptides, .data$Accession == proteins$Accession[i])

      div(style = "padding: 1rem",
          reactable(retval,
                    columns = list(Accession = colDef(width = 250),
                                   Peptide = colDef(width = 250),
                                   `Source File` = colDef(width = 200),
                                   PTM = colDef(width = 300),
                                   AScore = colDef(width = 250))))
    }, columns = list(Accession = colDef(width = 250),
                      PTM = colDef(width = 300),
                      Description = colDef(width = 500)))

    save(summary_table, file = file.path(i, 'summary.RData'))

    #######################
    # Generate html files #
    #######################

    file.path(config$qmd, 'summary.qmd') %>%
      quarto_render(execute_dir = i)

    file.copy(file.path(config$qmd, 'summary.html'),
              file.path(i, 'summary.html'), overwrite = TRUE)
    file.copy(file.path(config$qmd, 'summary_files'),
              i, recursive = TRUE)
  }

}
