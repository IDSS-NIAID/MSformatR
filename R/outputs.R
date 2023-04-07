# outputs.R
# methods for processing and outputting MS data

#' generate_html_output
#' Generate html summary files for MS data
#'
#' @param input Location of input files. `summary.html` and related files will also be stored in this location.
#' @param qmd Location of the qmd file to use. By default, `inst/quarto/summary.qmd` will be used.
#'
#' @details Generates an html summary file using the RData file generated and saved in `process_file_inputs` and `qmd`
#' @export
#' @importFrom stringr str_replace
#' @importFrom stringr fixed
#' @importFrom quarto quarto_render
generate_html_output <- function(input, qmd)
{
  # render the quarto document
  quarto_render(qmd, execute_dir = input)

  # figure out which files to move
  out.html <- str_replace(qmd, fixed('.qmd'), '.html')
  out.files <- str_replace(qmd, fixed('.qmd'), '_files')

  # copy output to correct location
  file.copy(out.html, input, overwrite = TRUE)
  file.copy(out.files, input, recursive = TRUE)

  # clean up output files
  file.remove(out.html)
  unlink(out.files, recursive = TRUE)

  # exit quietly
  invisible(NULL)
}