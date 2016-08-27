# createRandomisationDoc -------------------------------------------------------
#' Create block randomisation list documents
#'
#' Create block randomisation list documents in three possible formats:
#' HTML, PDF (requires LaTeX) and Microsoft Word
#'
#' @param patients data frame containing the patient's data as returned by
#'   \code{\link{createTestdata}}
#' @param strataVars List of strata variables, each of which is defined in
#'   the form of a vector of possible (character) values.
#' @param treatments named vector of character defining the possible treatments.
#'   The names are used as acronyms, the values as the full treatment names
#' @param header list with elements \code{title, author, date} defining the values to
#'   be used in the title, author and date field, respectively, in the header of
#'   the document
#' @param format vector of elements from \code{c("html", "word", "pdf")}
#'   defining the types of documents to be created
#' @param newpage if \code{TRUE} and \code{"pdf"} is in \code{format} each
#'   stratum will appear on its own page in the created PDF document
#' @param file.rmd full path to the RMarkdown file that is rendered to create
#'   the randomisation list documents. By default the file \code{randomList.Rmd}
#'   in the subfolder \code{extdata} of the package installation directory
#'   (returned by \code{system.path(package = "blockrand2")} is used. You may
#'   want to make a copy of this file, modify it and set \code{file.rmd} to the
#'   path of that new file to override the appearence of the created output.
#' @param outdir full path to the output directory to which the created document
#'   files are written. By default the files are written to a subfolder
#'   \code{blockrand2} in the current R session's temporary directory
#' @return Named vector of character containing the full paths to the created
#'   files as values and the file extensions (\code{html, docx, pdf}) as names
#' @export
#' @examples
#' \dontrun{
#'
#' # Define title of the study, the author and the date
#' header <- list(
#'   title = "Randomisation lists for the study: *Jokes against craziness*",
#'   author = "Hauke Sonnenberg",
#'   date = "2016-08-27"
#' )
#'
#' # Define stratum variables
#' strataVars <- list(
#'   sex = c("male", "female"),
#'   crazyness = c("weak", "medium", "strong")
#' )
#'
#' # Define the treatments
#' treatments <- c(
#'   joke = "Tell funny jokes",
#'   nojoke = "Keep serious!"
#' )
#'
#' # Create some patient's testdata using the stratum levels
#' patients <- createTestdata(strataVars, 40)
#'
#' # Create the randomisation list documents.
#' # The full path to the output directory is returned invisibly.
#' files <- createRandomisationDoc(
#'   patients = patients,
#'   strataVars = strataVars,
#'   treatments = treatments,
#'   header = header,
#'   newpage = TRUE
#' )
#'
#' # Show the paths of the created files
#' files
#'
#' # Open the html file in the default browser
#' getOption("browser")(files["html"])
#'
#' # Open the pdf file in the default PDF viewer
#' system(paste(getOption("pdfviewer"), files["pdf"]))
#' }
createRandomisationDoc <- function
(
  patients,
  strataVars,
  treatments,
  header = list(
    title = "Randomisierungsliste zur Studie: title",
    author = "author",
    date = Sys.Date()
  ),
  format = c("html", "word", "pdf"),
  newpage = FALSE,
  file.rmd = system.file("extdata", "randomList.Rmd", package = "blockrand2"),
  outdir = file.path(tempdir(), "blockrand2")
)
{
  # Group and count by stratum
  (byStratum <- stratify(patients, strataVars))

  # Create random sequences of treatments (A, B)
  sequences <- createRandomSequences(
    x = names(treatments),
    counts = byStratum$n,
    names.strata = byStratum$stratum
  )

  # Append a column "treatments" showing the sequences of treatments as comma
  # separated lists
  byStratum$treatments <- sapply(sequences, paste, collapse = ",")

  # Append a column "treatment" to patients
  patientsWithTreatments <- assignTreatments(patients, byStratum, sequences)

  if (! file.exists(outdir)) {
    dir.create(outdir)
  }

  rmarkdown::render(
    input = file.rmd,
    output_format = paste0(format, "_document"),
    output_dir = outdir,
    params = list(
      param_title = header$title,
      param_author = header$author,
      param_date = header$date
    )
  )

  filebase <- gsub("\\.Rmd$", "", basename(file.rmd), ignore.case = TRUE)

  pattern <- paste0("^", filebase, "\\.")

  files <- dir(outdir, pattern, full.names = TRUE)

  structure(files, names = gsub(pattern, "", basename(files)))
}

# assignTreatments -------------------------------------------------------------
assignTreatments <- function(patients, byStratum, sequences)
{
  # Merge stratum column and reorder by patient
  patients <- merge(patients, byStratum[, seq_len(ncol(byStratum) - 2)])
  patients <- patients[order(patients$patient), ]

  # Group by stratum and assign treatments
  strataList <- lapply(names(sequences), function(stratum) {
    cbind(
      patients[patients$stratum == stratum, ],
      treatment = sequences[[stratum]],
      stringsAsFactors = FALSE
    )
  })

  # "rbind" data frames and reorder by patient
  patients <- do.call(rbind, strataList)
  patients <- patients[order(patients$patient), ]

  # Clear row names
  rownames(patients) <- NULL

  patients
}

# moveColumnsFirst -------------------------------------------------------------
moveColumnsFirst <- function(x, columns)
{
  x[, c(columns, setdiff(names(x), columns))]
}

# mergeAll ---------------------------------------------------------------------
mergeAll <- function(L)
{
  for (i in seq_along(L)) {

    if (i == 1) {
      out <- L[[i]]
    } else {
      out <- merge(out, L[[i]])
    }
  }

  out
}

# treatmentsByStratum ----------------------------------------------------------
treatmentsByStratum <- function(patientsWithTreatments, treatments)
{
  countValue <- function(x, value) {
    sum(x == value)
  }

  lapply(names(treatments), function(treatment) {
    out <- aggregate(
      treatment ~ stratum,
      data = patientsWithTreatments,
      FUN = countValue,
      value = treatment
    )
    names(out)[2] <- paste0("n.", treatment)
    out
  })
}

# noRowNameTable ---------------------------------------------------------------
noRowNameTable <- function(x, ...)
{
  knitr::kable(x, row.names = FALSE, ...)
}

# newPageIf --------------------------------------------------------------------
newPageIf <- function(x)
{
  cat("\n", ifelse(x, "\\newpage", ""), "\n")
}

# therapyString ----------------------------------------------------------------
therapyString <- function(acronym, treatments)
{
  paste0(acronym, ": ", treatments[acronym])
}
