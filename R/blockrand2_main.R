# createTestdata ---------------------------------------------------------------
#' Create testdata for clinical studies
#'
#' @param strataVars List of strata variables, each of which is defined in
#'   the form of a vector of possible (character) values.
#' @param n Number of records to be created
#' @param n.per.stratum optional. Vector of integer numbers defining the number
#'   of records for each possible stratum. The length of this vector must be
#'   equal to the number of possible strata (resulting from the product of
#'   lengths of vectors in \code{strataVars})
#' @param format.patient Passed as argument \code{format} to \code{sprintf()}
#'   when creating a unique id for each patient
#' @param offset.patient integer number to be added to the generated patient
#'   numbers \code{1:n}. Defaults to \code{0}.
#' @export
#' @return Data frame with a column \code{patient} containing the patient's ID
#' and one column for each stratum, named according to the element names in
#' \code{strataVars}
#' @examples
#' strataVars <- list(
#'  sex = c("female", "male"),
#'  medication = c("yes", "no")
#' )
#'
#' createTestdata(strataVars, n = 20)
#'
#' # Define the number of patients in each stratum (there are four possible
#' # strata: length(strataVars$sex) * length(strataVars$medication))
#' createTestdata(strataVars, n.per.stratum = c(5, 6, 3, 4))
createTestdata <- function
(
  strataVars,
  n = if (is.null(n.per.stratum)) 30 else NULL,
  n.per.stratum = NULL,
  format.patient = "P%03d",
  offset.patient = 0
)
{
  if (! is.null(n.per.stratum)) {

    combis <- createCombinations(strataVars)

    n.counts <- length(n.per.stratum)
    n.combis <- nrow(combis)

    if (n.counts != n.combis) {

      stop(sprintf(
        paste("The length of n.per.stratum (= %d) must be equal to the number",
              "of possible strata (= %d)!"),
        n.counts, n.combis
      ), call. = FALSE)
    }

    sum.counts <- sum(n.per.stratum)

    if (! is.null(n) && sum.counts != n) {

      warning(sprintf(
        "n (= %d) is overriden by sum(n.per.stratum) (= %d).",
        n, sum.counts
      ), call. = FALSE)
    }

    n <- sum.counts

    properties <- combis[rep(seq_len(n.combis), times = n.per.stratum), ]

    properties <- properties[sample(nrow(properties)), ]

    rownames(properties) <- NULL
  }
  else {
    properties <- data.frame(
      lapply(strataVars, sample, size = n, replace = TRUE)
    )
  }

  cbind(
    patient = sprintf(format.patient, seq_len(n) + offset.patient),
    properties,
    stringsAsFactors = FALSE
  )
}

# readRealData -----------------------------------------------------------------
readRealData <- function(file)
{
  # Adapt this function to your needs!
  read.table(file)
}

# stratify ---------------------------------------------------------------------
#' Group and count data by combinations of strata variables
#'
#' Group data by combinations of stratum values and count the records falling
#' into each combination.
#'
#' @param x Data frame with stratum values in columns
#' @param strataVars List of strata variables. By default it is created from all
#'   but the first columns of \code{x}.
#' @param column.n Column name to be used in the output for the column
#'   containing the number of records falling into the combination of stratum
#'   values.
#' @param column.stratum Column name to be used in the output for the column
#'   containing the stratum identifier
#' @param format.stratum format string to be used in \code{format} to create a
#'   unique id for each stratum
#' @export
#' @return Data frame with each row representing a combination of stratum values
#' @examples
#' # Define stratifying variables
#' strataVars <- list(sex = c("female", "male"), medication = c("yes", "no"))
#'
#' # Create some random testdata
#' x <- createTestdata(strataVars)
#'
#' # Count records in each stratum
#' stratify(x, strataVars)
#' stratify(x, strataVars, column.n = "n.patients")
#'
#' # Create testdata with defined numbers of records in each stratum
#' n.per.stratum <- 1:4
#' y <- createTestdata(strataVars, n.per.stratum = n.per.stratum)
#'
#' # Count records in each stratum
#' (counts <- stratify(y, strataVars))
#'
#' # Check if the numbers of records in each stratum are as expected
#' all(counts$n == n.per.stratum)
stratify <- function
(
  x,
  strataVars = toStrataVars(x, exclude = names(x)[1]),
  column.n = "n",
  column.stratum = "stratum",
  format.stratum = "S%02d"
)
{
  stopifnot(is.data.frame(x))
  stopifnot(is.list(strataVars))

  names.x <- names(x)
  names.strataVars <- names(strataVars)

  stopifnot(all(names.strataVars %in% names.x))
  stopifnot(all(names.strataVars != column.n))

  # Add a helper column "column.n" to the left of x
  x <- cbind(1, x)
  names(x)[1] <- column.n

  # Calculate the number of patients per stratum
  grouped <- aggregate(toFormula(column.n, names.strataVars), x, length)

  # Check the result for plausibility
  stopifnot(sum(grouped[[column.n]]) == nrow(x))

  # Create all possible combinations of strata variables
  combis <- createCombinations(strataVars)

  combis <- cbind(
    stratum = sprintf(format.stratum, seq_len(nrow(combis))),
    combis,
    stringsAsFactors = FALSE
  )

  # Merge the number of patients for each combination of strata variables
  out <- merge(combis, grouped, all.x = TRUE)

  # replace NAs with 0
  out[is.na(out)] <- 0

  columns <- c(column.stratum, setdiff(names(out), column.stratum))

  out[order(out[[column.stratum]]), columns]
}

# toStrataVars -----------------------------------------------------------------
toStrataVars <- function
(
  x,
  include = setdiff(names(x), exclude),
  exclude = NULL
)
{
  stopifnot(is.data.frame(x))
  stopifnot(all(include %in% names(x)))

  out <- lapply(include, function(column) {
    values <- x[[column]]
    if (is.factor(values)) {
      levels(values)
    } else {
      sort(unique(values))
    }
  })

  structure(out, names = include)
}

# toFormula --------------------------------------------------------------------
toFormula <- function(y, x)
{
  formula(paste(y, "~", paste(x, collapse = " + ")))
}

# createCombinations -----------------------------------------------------------
#' Create all possible combinations of values
#'
#' Create all possible combinations of values given in a list of value vectors
#'
#' @param x List of vectors containing the values to be combined. The
#'   names of the list elements will appear as column names in the output data
#'   frame.
#' @export
#' @return Data frame with each row representing a possible combination of
#'   values.
#' @examples
#' createCombinations(list(
#'   sex = c("male", "female"),
#'   smokes = c("yes", "sometimes", "no"),
#'   cancer = c("yes", "no")
#' ))
createCombinations <- function(x)
{
  names.x <- names(x)

  # Create a list of data frames each of which has exactly one column containing
  # the values of one list element of x
  L <- lapply(seq_along(x), function(i) {
    structure(data.frame(x[[i]]), names = names.x[i])
  })

  # Return the result data frame with reverted column order
  revertColumnOrder(mergeAll(L))
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

# revertColumnOrder ------------------------------------------------------------
revertColumnOrder <- function(x)
{
  stopifnot(length(dim(x)) == 2)

  x[, rev(seq_len(ncol(x)))]
}

# createRandomSequences --------------------------------------------------------
#'Create random sequences of blockwise equally distributed values
#'
#'Create random sequences of blockwise equally distributed values. The
#'blocksizes are chosen according to the number of values to be created.
#'
#'@param x vector of values to be chosen from
#'@param counts vector of integers defining the lengths of vectors of values to
#'  be created
#'@param names.strata vector of character to be used as element names in the
#'  output list
#'@export
#'@return list of vectors of values out of \code{x} with lengths according to
#'  the values given in \code{counts}
#'@examples
#'# Define stratum levels
#'strata <- list(
#'  sex = c("male", "female"),
#'  medication = c("yes", "no"),
#'  lesion = c("low", "high")
#')
#'
#'# Create some testdata using the stratum levels and group and count by stratum
#'byStratum <- stratify(createTestdata(strata, 50))
#'
#'# Create random sequences of treatments (A, B)
#'sequences <- createRandomSequences(
#'  x = c("A", "B"),
#'  counts = byStratum$n,
#'  names.strata = byStratum$stratum
#')
#'
#'# Append a column "treatments" showing the sequences of treatments as comma
#'# separated lists
#'byStratum$treatments <- sapply(sequences, paste, collapse = ",")
#'
#'# Show the result
#'byStratum
createRandomSequences <- function
(
  x, counts, names.strata = NULL
)
{
  out <- lapply(counts, function(count) {
    randomSequence(
      x = x,
      length.out = count,
      block.size = blocksize(count, length(x))
    )
  })

  structure(out, names = names.strata)
}

# blocksize  -------------------------------------------------------------------
#'Get a suitable blocksize for a given number of subjects
#'
#'Get a suitable blocksize for a given number of subjects
#'@param x number of patients in stratum
#'@param N number of treatments
#'@export
#'@return integer vector of length one representing the blcksize
#'@examples
#'# Blocksize for 10 subjects and 2 treatments
#'blocksize(10, 2)
#'
#'# Blocksize for 10 subjects and 3 treatments
#'blocksize(10, 3)
blocksize <- function(x, N)
{
  N * (round(x/N) + (x == 1))
}

# randomSequence ---------------------------------------------------------------
randomSequence <- function(x, length.out = length(x), block.size = length.out)
{
  sizes <- sampleWithSum(block.size, length.out, warn = FALSE)

  out <- lapply(sizes, equalDistribution, x = x, do.stop = FALSE)

  unlist(out)
}

# sampleWithSum ----------------------------------------------------------------
sampleWithSum <- function(x, sum.out = sum(x), force = TRUE, warn = TRUE)
{
  out <- integer()
  oneSize <- (length(x) == 1)

  while (sum(out) < sum.out) {
    out <- c(out, ifelse(oneSize, x, sample(x, 1)))
  }

  if (force && sum(out) > sum.out) {

    n <- length(out)

    lastValue <- sum.out - sum(out[-n])

    if (warn && ! lastValue %in% x) {
      warning(sprintf(
        paste("The last value (%d) is not an element of x (%s).",
              "It has been chosen so that the sum of output values is %d"),
        lastValue, paste(x, collapse = ", "), sum.out
      ))
    }

    out[n] <- lastValue
  }

  out
}

# equalDistribution ------------------------------------------------------------
#' Create a random sequence of equally distributed values
#'
#' @param x Vector of values to be chosen from
#' @param length.out Length of the output vector to be created
#' @param do.stop If \code{TRUE} (default) an error is thrown if
#'   \code{length.out} is not a multiple of the length of \code{x}. Otherwise
#'   no error is thrown.
#' @export
#' @return Vector of \code{length.out} values all of which are elements of
#'   \code{x}. If \code{length.out} is a multiple of the lenght of \code{x} the
#'   values are equally distributed. Otherwise (and if \code{do.stop} is
#'   \code{FALSE} so that no error is thrown) the frequencies of the values
#'   differ by one at most.
#' @examples
#' y1 <- equalDistribution(x = LETTERS[1:2], 10)
#'
#' # Check the distribution with table
#' table(y1)
#'
#' # Do all values occur with the same frequency?
#' all(diff(table(y1)) == 0)
#'
#' y2 <- equalDistribution(x = LETTERS[1:3], length.out = 11, do.stop = FALSE)
#'
#' # Do the frequencies differ by one at most?
#' all(diff(table(y2)) <= 1)
equalDistribution <- function
(
  x,
  length.out = length(x),
  do.stop = TRUE
)
{
  length.x <- length(x)

  # How often will each element of x appear in the output?
  times <- length.out %/% length.x

  # Is the output length a multiple of the number of elements in x?
  if (times * length.x != length.out) {

    if (do.stop) {
      stop(sprintf(
        "length.out (%d) must be a multiple of the length of x (%d)!",
        length.out, length.x
      ))
    } else {
      # let's create too many instead of too few elements.
      times <- times + 1
    }
  }

  # Repeat the (shuffled) elements of x, each "times" times
  out <- rep(shuffle(x), times)

  # If the output vector is too long, cut off the last elements
  length(out) <- length.out

  # Finally, shuffle the output values
  shuffle(out)
}

# shuffle ----------------------------------------------------------------------
shuffle <- function # random order of elements
### Bring the elements of x into a random order
(
  x
)
{
  x[sample(seq_along(x))]
}

# printSequences ---------------------------------------------------------------
printSequences <- function(sequences, stratumFrequency, Data)
{
  stopifnot(is.list(sequences))

  names.columns <- names(stratumFrequency)
  names.strata <- names(sequences)

  stopifnot("stratum" %in% names.columns)
  stopifnot("n" %in% names.columns)

  for (stratum in names.strata) {

    stratumInfo <- stratumFrequency[stratumFrequency[["stratum"]] == stratum, ]

    cat(sprintf("Stratum '%s' (%s):\n", stratum, toStratumString(stratumInfo)))

    if (nrow(stratumInfo) > 0 && stratumInfo[["n"]] > 0) {

      out <- merge(Data, stratumInfo)

      out <- out[order(out[["patient"]]), ]

      out[["treatment"]] <- sequences[[stratum]]

      row.names(out) <- NULL

      print(out[, c("patient", "treatment")])

    } else {

      cat("No patients in this stratum.\n")
    }

    cat("\n")
  }
}

# toStratumString --------------------------------------------------------------
#'Create a string describing the stratum from a one-row data frame
#'
#'Create a string describing the stratum from a one-row data frame
#'
#'@param stratumInfo data frame with one row and stratum information in all but
#'  the very first (\code{stratum}) and the very last (\code{n}) column.
#'@export
#'@return character string of the form attribute1 = value1, attribute2 = value2,
#'  etc.
#'@examples
#'stratumInfo <- data.frame(
#'  stratum = "S01",
#'  sex = "male",
#'  pretreatment = "no",
#'  n = 5
#')
#'
#'toStratumString(stratumInfo)
toStratumString <- function(stratumInfo)
{
  out <- stratumInfo[, -c(1, ncol(stratumInfo))]

  paste(names(out), "=" , columnsToCharacter(out), collapse = ", ")
}

# columnsToCharacter -----------------------------------------------------------
columnsToCharacter <- function(x)
{
  for (i in seq_len(ncol(x))) {
    x[[i]] <- as.character(x[[i]])
  }

  x
}

# test_blocksize ---------------------------------------------------------------
test_blocksize <- function()
{
  stopifnot(blocksize(1, 3) == 3)
}
