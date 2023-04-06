## MSformatR

MSformatR contains tools for basic processing and summarizing of raw MS output.

### Config file details

The config file, `config.yml`, will control the general behavior of MSformatR when generating the summary files and should be located in the current working directory when MSformatR is run.

Settable parameters:

* `input`: This specifies the location of the data to be summarized. See the `format` section below for format-specific details. By default, MSformatR will look for all relevant files in the current working directory (i.e. `input: *`). Use of unix-style path seperators, `/`, is required. R will check the system and replace them with the correct path seperator if running on Windows. One of three values is expected:
    * An array of input directories (e.g. `input: ["data/A", "data/B", "data/C"]`)
    * A single path (e.g. `input: "data/raw_results"`)
    * A path to a directory containing multiple files to be summarized together. This should end in an `*`. (e.g. `input: "data/*"`)
* `format`: This specifies the source of the raw data. Default is `peaks11`. Supported formats:
    * `peaks11` - html output from Peaks 11. Expected values for `input` are directory paths, each of which should contain multiple html files, csv files, and a directory of images.