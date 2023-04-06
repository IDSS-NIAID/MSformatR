## MSformatR

MSformatR contains tools for basic processing and summarizing of raw MS output.

### Installation

To install, open an R session, and execute the following command:

```
remotes::install_github('IDSS-NIAID/MSformatR', ref = 'dev')
```

### Simple run

For the most basic usage, start up an R session and set the working directory to the location containing Peaks 11 html output directories and execute the following command:

```
MSformatR::summarizeMS()
```

Alternately, you could open a terminal window and navigate to the directory containing one or more Peaks 11 html output directories. From the terminal window, execute the following command:

```
R -e 'MSformatR::summarizeMS()'
```

### Config file details

The config file, `config.yml`, will control the general behavior of MSformatR when generating the summary files and should be located in the current working directory when MSformatR is run.

Settable parameters:

* `input`: This specifies the location of the data to be summarized. See the `format` section below for format-specific details. By default, MSformatR will look for all relevant files in the current working directory (i.e. `input: *`). Use of unix-style path seperators, `/`, is required. R will check the system and replace them with the correct path seperator if running on Windows. `input` is assumed to be either the same as or a subdirectory of `root`. One of three values is expected:
    * An array of input directories (e.g. `input: ["data/A", "data/B", "data/C"]`)
    * A single path (e.g. `input: "data/raw_results"`)
    * A path to a directory containing multiple files to be summarized together. This should end in an `*`. (e.g. `input: "data/*"`)
* `format`: This specifies the source of the raw data. Default is `peaks11`. Supported formats:
    * `peaks11` - html output from Peaks 11. Expected values for `input` are directory paths, each of which should contain multiple html files, csv files, and a directory of images.
* `root`: This specifies the directory in which to look for supporting files. If no directory is provided, MSformatR will check if the current working directory is inside of this git repository and use the root directory of the git repo. Otherwise, it will use the current working directory. If the required supporting files are not found in `root`, it will attempt to download them from the internet.