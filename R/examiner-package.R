#' examiner.
#'
#' @name examiner
#' @docType package
#' @import yaml
#' @import stringr
NULL

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
problem <- function(x, answers,
                    correct = 1L, first = 0L, last = 0L,
                    randomizable = TRUE) {
    n <- length(answers)
    first <- min(c(first, n - last + 1))
    last <- min(c(last, n))
    structure(as.character(x),
              answers = as.character(answers),
              correct = correct,
              first = first,
              last = last,
              randomizable = as.logical(randomizable)[1],
              class = c("problem", "examiner", "character"))
}

.randomize <- function(x, first = 0, last = 0) {
    firsti <- seq_len(first)
    if (last > 0) {
        lasti <- rev(length(x) - seq_len(last) + 1)
    } else {
        lasti <- integer(0)
    }
    middle <- setdiff(seq_along(x), c(firsti, lasti))
    if (length(middle) > 1) {
        randorder <- sample(middle, length(middle))        
    } else {
        randorder <- middle
    }
    indices <- c(firsti, randorder, lasti)
    y <- x[indices]
    attr(y, "order") <- indices
    y
}

#' @export
format.problem <- function(x, randomize = FALSE, format = "tex", ...) {
    answers <- attr(x, "answers")
    if (attr(x, "randomizable") && as.logical(randomize)) {
        answers <- .randomize(answers,
                              attr(x, "first"),
                              attr(x, "last"))
    }
    answerstr <- str_c("\\item", answers,
                       sep = " ",
                       collapse = "\n")
    str_c("\\begin{problem}",
          str_c(as.character(x), collapse = "\n\n"),          
          "\\begin{answers}",
          answerstr,
          "\\end{answers}",
          "\\end{problem}\n",
          sep = "\n")
}

#' Create a problemset object
#'
#' @param x \code{character} vector with the prompt for the problemset.
#' @param problems list of problems or other problemsets
#' @return A \code{problemset} object
#' @export
problemset <- function(x, problems) {
    if (! inherits(problems, "list")) {
        stop("problems must be a list")
    }
    if (!all(sapply(problems, inherits, what = "examiner"))) {
        stop("problems must be a list of problem or problemset objects")
    }

    structure(as.character(x),
              problems = problems,
              class = c("problemset", "examiner", "character"))
}

#' @export
format.problemset <- function(x, randomize = FALSE, ...) {
    problemstr <-
        str_c(sapply(unname(attr(x, "problems")),
                     function(xi) {
                         format(xi, randomize = randomize)
                     }), collapse = "\n")
    str_c("\\begin{problemset}",
          str_c(as.character(x), collapse = "\n\n"),
          "\\begin{problems}",
          problemstr,
          "\\end{problems}",
          "\\end{problemset}\n",
          sep = "\n")
          
}

list2problem <- function(x) {
    text <- x[["text"]]
    x[["text"]] <- NULL
    do.call(problem, c(list(x = text), x))
}

list2problemset <- function(x) {
    problemset(x[["text"]],
               lapply(x[["problems"]], list2examiner))
}

list2examiner <- function(x) {
    if ("answers" %in% names(x)) {
        list2problem(x)
    } else {
        list2problemset(x)
    }
}

#' Create problemset from yaml file
#'
#' @param input Name of the input file.
#' @return A \code{problemset} object.
#' @export
problemset_from_yaml <- function(input) {
    list2problemset(yaml.load_file(input))
}
