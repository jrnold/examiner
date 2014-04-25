#' examiner.
#'
#' @name examiner
#' @docType package
#' @import yaml
#' @import stringr
NULL

beginenv <- function(x) str_c("\\begin{", x, "}")
endenv <- function(x) str_c("\\end{", x, "}")
ltxnewenv <- function(x, begin = "", end = "") {
    str_c("\\newenvironment{", x, "}{", begin, "}{", end, "}")
}

.EXAMINER_LATEX_HEADER <-
    c("\\usepackage{amsthm,amsmath,enumitem}",
      "\\theoremstyle{definition}\\newtheorem{problem}{Problem}",
      ltxnewenv("problemtext", "\\par", ""),
      ltxnewenv("problemset", "\\par", ""),
      ltxnewenv("problems", "\\par", ""),
      ltxnewenv("problemsettext", "\\par", ""),
      "\\newlist{answers}{enumerate}{1}",
      "\\setlist[answers]{label=(\\alph*)}")

#' Create a LaTeX header for \code{examiner} output
#'
#' This renders the contents of \code{opts_examiner$get('header')}.
#'
#' @return A character vector with the header
#' @export
examiner_latex_header <- function() {
    str_c(.EXMINER_LATEX_HEADER, collapse = "\n")
}

#' Create a problem object
#'
#' @param x \code{character} vector with the prompt for the problem.
#' @param answers A \code{list} of \code{problem} or \code{problemset} objects.
#' @param correct Indices of answers which are correct.
#' @param first Number of observations at the beginning of \code{answers} which will not be shuffled.
#' @param last Number of observations at the end of \code{answers} which will not be shuffled.
#' @param randomizable Can the answers be shuffled?
#' @return An object of class \code{problem}.
#' @export
problem <- function(x,
                    answers = character(),
                    solution = "",
                    correct = 1L, first = 0L, last = 0L,
                    randomizable = TRUE) {
    n <- length(answers)
    first <- min(c(first, n - last + 1))
    last <- min(c(last, n))

    firsti <- seq_len(first)
    lasti <- pmin(rev(length(x) - seq_len(last) + 1),
                  length(answers))
    answers <-
        data.frame(text = answers,
                   correct = seq_along(answers) == correct,
                   fixed = seq_along(answers) %in% c(firsti, lasti),
                   stringsAsFactors = FALSE)
    .Data <-
        list(text = text,
             answers = answers,
             correct = correct,
             first = first,
             last = last,
             solution = as.character(solution),
             randomizable = as.logical(randomizable)[1])
    class(.Data) <- c("problem", "list")
    .Data
}

shuffle_answers <- function(x) {
    indices <- seq_len(nrow(x))[x[["fixed"]]]
    iorder <- sample(indices, length(indices))
    x[indices, ] <- x[iorder, , drop = FALSE]
    attr(x, "order") <- iorder
}

format_answer <- function(text, correct, ...) {
    str_c("\\item", text, sep = " ")
}

format_answerlist <- function(x) {
    str_c(mlply(x[["answers"]], format_answer), collapse = "\n")
}

format_problem <- function(x) {
    str_c(beginenv("problem"),
          beginenv("problemtext"),
          str_c(x[["text"]], sep = "\n"),
          endenv("problemtext"),
          beginenv("answers"),
          x[["answers"]],
          endenv("answers"),
          endenv("problem"),
          sep = "\n")
}

#' @export
format.problem <- function(x, randomize = FALSE) {
    if (x[["randomizable"]] && as.logical(randomize)) {
        x[["answers"]] <- shuffle_answers(x[["answers"]])
    }
    x[["answers"]] <- format_answerlist(x[["answers"]])
    format_problem(x)
}

#' Create a problemset object
#'
#' @param x \code{character} vector with the prompt for the problemset.
#' @param problems list of problems or other problemsets
#' @return A \code{problemset} object
#' @export
problemset <- function(problems, pretext = "", posttext = "") {
    if (! inherits(problems, "list")) {
        stop("problems must be a list")
    }
    if (!all(sapply(problems, inherits, what = "examiner"))) {
        stop("problems must be a list of problem or problemset objects")
    }
    .Data <- list(problems, pretext = pretext, posttext = posttext)
    class(.Data) <- c("problemset", "list")
    .Data
}

format_problems <- function(problems, ...) {
    str_c(ldply(problems, format, ...), collapse = "\n")
}

format_problemset <- function(x) {
    str_c(beginenv("problemset"),
          beginenv("problemsetpretext"),
          str_c(as.character(x[["pretext"]]), collapse = "\n"),
          endenv("problemsettext"),
          beginenv("problems"),
          x[["problems"]],
          endenv("problems"),
          beginenv("problemsetposttext"),
          str_c(as.character(x[["posttext"]]), collapse = "\n"),
          endenv("problemsetposttext"),
          endenv("problemset"),
          "\n", sep = "\n")
}

#' @export
format.problemset <- function(x, randomize = FALSE, ...) {
    x[["problems"]] <- format_problems(x, randomize = randomize)
    format_problemset(x)
}

#' Create problemset from a yaml file
#'
#' @param input Name of the input file.
#' @return A \code{problemset} object.
#' @export
problemset_from_yaml <- function(input) {
    .Data <- yaml.load_file(input)
    problems <- llply(.Data[["problems"]], function(x) do.call(problem, x))
    .Data[["problems"]] <- problems
    do.call(problemset, .Data)
}
