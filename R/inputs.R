# inputs.R
# methods for processing MS input


#####################
# Check File Inputs #
#####################

#' check_file_inputs
#' Validate values from config$input - called by summarizeMS
#'
#' @param input Character vector specifying location of input data
#' @param format Character value specifying format of input data
#'
#' @return Returns an updated character vector for input
#' @export
check_file_inputs <- function(input, format)
{
  if(format == 'peaks11')
  {
    return(check_file_inputs.peaks(input))
  }

  stop("Unrecognized format")
}


# method for checking file inputs from PEAKS
#' @rdname check_file_inputs
#' @export
#' @importFrom stringr str_sub
check_file_inputs.peaks <- function(input)
{

  ##### Determine input files #####

  if(str_sub(input[1], start = -1) == "*")
  {
    if(length(input) > 1)
      stop("Expected values for input are a vector of paths, a single path, or a single path with a wild card at the end of the path, '*'.")

    input <- gsub('*', '', input, fixed = TRUE) %>%
      strsplit('/') %>%
      unlist() %>%
      paste(collapse = .Platform$file.sep) %>%
      list.dirs(recursive = FALSE)
  }

  return(input)
}


#######################
# Process File Inputs #
#######################

#' process_file_inputs
#' Process data - called by summarizeMS
#'
#' @param input Character vector specifying location of input data
#' @param format Character value specifying format of input data
#'
#' @return A list containing processed data
#' @export
process_file_inputs <- function(input, format)
{
  if(format %in% 'peaks11')
  {
    return(process_file_inputs.peaks(input))
  }

  stop("Unrecognized format")
}

# method for processing inputs from PEAKS
#' @rdname process_file_inputs
#' @export
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#' @importFrom htmltools div
#' @importFrom reactable reactable
#' @importFrom reactable colDef
process_file_inputs.peaks <- function(input)
{
  # read proteins
  proteins <- file.path(input, 'db.proteins.csv') %>%
    read_csv() %>%
    filter(!grepl('#CONTAM', .data$Accession, fixed = TRUE))

  # read in peptides
  peptides <- file.path(input, 'db.protein-peptides.csv') %>%
    read_csv() %>%
    filter(!grepl('#CONTAM', .data$Accession, fixed = TRUE))

  # generate nested summary table
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

  # quarto will need this information when rendering the web pages
  save(summary_table, file = file.path(input, 'summary.RData'))

  list(proteins = proteins,
       peptieds = peptides,
       summary_table = summary_table) %>%
    return()
}