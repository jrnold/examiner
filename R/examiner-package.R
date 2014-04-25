#' examiner.
#'
#' @name examiner
#' @docType package
#' @import yaml
#' @import stringr
#' @import whisker
#' @import plyr
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
      ltxnewenv("problemsetpretext", "\\par", ""),
      ltxnewenv("problemsetposttext", "\\par", ""),
      "\\newlist{answers}{enumerate}{1}",
      "\\setlist[answers]{label=(\\alph*)}")

#' Create a LaTeX header for \code{examiner} output
#'
#' This renders the contents of \code{opts_examiner$get('header')}.
#'
#' @return A character vector with the header
#' @export
examiner_latex_header <- function() {
    str_c(.EXAMINER_LATEX_HEADER, collapse = "\n")
}


#' Create an answerlist object
#' @param x A data frame with colums: text, correct, and fixed.
#' @return An object of class \code{answerlist}.
#' @export
answerlist <- function(x) {
    x <- as.data.frame(x)
    class(x) <- c("answerlist", class(x))
    x
}

#' @export
format.answerlist <- function(x, show_solutions = FALSE, ...) {
    format_answer <- function(text, correct, ...) {
        str_c("\\item", text, sep = " ")
    }
    str_c(maply(x, format_answer, .expand = FALSE, .drop = TRUE), 
          collapse = "\n")
}


#' Create a problem object
#'
#' @param text \code{character} vector with the prompt for the problem.
#' @param answers A \code{list} of \code{problem} or \code{problemset} objects.
#' @param correct Indices of answers which are correct.
#' @param first Number of observations at the beginning of \code{answers} which will not be shuffled.
#' @param last Number of observations at the end of \code{answers} which will not be shuffled.
#' @param randomizable Can the answers be shuffled?
#' @return An object of class \code{problem}.
#' @keywords internal 
problem <- function(text = "",
                    answers = character(),
                    solution = "",
                    correct = 1L, first = 0L, last = 0L,
                    randomizable = TRUE) {
    n <- length(answers)
    first <- min(c(first, n - last + 1))
    last <- min(c(last, n))

    firsti <- seq_len(first)
    lasti <- pmin(rev(length(answers) - seq_len(last) + 1),
                  length(answers))
    answers <-
        answerlist(data.frame(text = answers,
                              correct = seq_along(answers) == correct,
                              fixed = seq_along(answers) %in% c(firsti, lasti),
                              stringsAsFactors = FALSE))
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
    indices <- seq_len(nrow(x))[!x[["fixed"]]]
    iorder <- sample(indices, length(indices))
    x[indices, ] <- x[iorder, , drop = FALSE]
    attr(x, "order") <- iorder
    x
}

format_problem <- function(x, show_solutions = FALSE) {
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
format.problem <- function(x, randomize = FALSE, show_solutions = FALSE, ...) {
    if (x[["randomizable"]] && as.logical(randomize)) {
        x[["answers"]] <- shuffle_answers(x[["answers"]])
    }
    x[["answers"]] <- format(x[["answers"]], show_solutions = show_solutions)
    format_problem(x, show_solutions = show_solutions)
}

#' Create a problemset object
#'
#' @param problems list of problems
#' @param pretext \code{character} vector with text to go before the questions.
#' @param posttext \code{character} vector with text to go after the questions.
#' @return A \code{problemset} object
#' @export
problemset <- function(problems, pretext = "", posttext = "") {
    if (! inherits(problems, "list") ||
        !all(sapply(problems, inherits, what = "problem"))) {
        stop("problems must be a list of problem objects")
    }
    .Data <- list(problems = problems, pretext = pretext,
                  posttext = posttext)
    class(.Data) <- c("problemset", "list")
    .Data
}

format_problems <- function(problems, ...) {
    str_c(laply(problems, format, ...), collapse = "\n")
}

format_problemset <- function(x, show_solutions = FALSE) {
    str_c(beginenv("problemset"),
          beginenv("problemsetpretext"),
          str_c(as.character(x[["pretext"]]), collapse = "\n"),
          endenv("problemsetpretext"),
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
format.problemset <- function(x, randomize = FALSE, show_solutions = FALSE, ...) {
    x[["problems"]] <- format_problems(x[["problems"]], show_solutions = show_solutions, randomize = randomize)
    format_problemset(x, show_solutions = show_solutions)
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
