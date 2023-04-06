# summary.R
# main script to run MSformattR

library(yaml)

library(dplyr)
library(readr)
library(stringr)

library(reactable)

##########
# config #
##########

if(file.exists('config.yml'))
{
  config <- read_yaml('config.yml')
}else{
  config <- list()
}


##### Default configurations #####

if(is.null(config$input))
  config$input <- '*'

if(is.null(config$format))
  config$format <- 'peaks11'

if(is.null(config$root))
{
  config$root <- try(system('git rev-parse --show-toplevel', intern = TRUE))

  if(class(config$root) == 'try-error')
    config$root <- getwd()

  # look for required supporting files - if not found, try downloading it
  if(!file.exists(file.path(config$root, 'summary.qmd')))
    download.file('https://raw.githubusercontent.com/IDSS-NIAID/MSformatR/main/summary.qmd',
                  'summary.qmd')
}

##### Determine input files #####

if(str_sub(config$input[1], start = -1) == "*")
{
  if(length(config$input) > 1)
    stop("Expected values for input are a vector of paths, a single path, or a single path with a wild card at the end of the path, '*'.")

  config$input <- gsub('*', '', config$input, fixed = TRUE) %>%
    strsplit('/') %>%
    unlist() %>%
    file.path() %>%
    {file.path(config$root, .)} %>%
    list.dirs(recursive = FALSE)
}


#################
# Process input #
#################
for(i in config$input)
{

proteins <- file.path(i, 'db.proteins.csv') %>%
  read_csv() %>%
  filter(!grepl('#CONTAM', Accession, fixed = TRUE))

peptides <- file.path(i, 'db.protein-peptides.csv') %>%
  read_csv() %>%
  filter(!grepl('#CONTAM', Accession, fixed = TRUE))

summary_table <- reactable(proteins, details = function(i) {
  retval <- filter(peptides, Accession == proteins$Accession[i])

  htmltools::div(style = "padding: 1rem",
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

file.path(config$root, 'summary.qmd') %>%
  quarto_render(execute_dir = i)

file.copy('summary.html', file.path(i, 'summary.html'), overwrite = TRUE)
file.copy('summary_files', i, recursive = TRUE)

}
