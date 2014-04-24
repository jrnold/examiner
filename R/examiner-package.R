#' examiner.
#'
#' @name examiner
#' @docType package
#' @import yaml
#' @import stringr
NULL

beginenv <- function(x) str_c("\\begin{", x, "}")
endenv <- function(x) str_c("\\end{", x, "}")
ltxnewenv <- function(x, begin = "", end = "") str_c("\\newenvironment{", x, "}{", begin, "}{", end, "}")

# --------------------
# From knitr
new_defaults = function(value = list()) {
  defaults = value

  get = function(name, default = FALSE, drop = TRUE) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (drop && length(name) == 1) defaults[[name]] else {
        setNames(defaults[name], name)
      }
    }
  }
  set = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
  restore = function(target = value) defaults <<- target

  list(get = get, set = set, merge = merge, restore = restore)
}
# -------------------------

#' \code{examiner} options
#'
#' This acts like \code{\link[knitr]{opts_knitr}} in \pkg{knitr}.
#' @export
opts_examiner <- new_defaults(list(
    answers_env = "answers",
    problem_env = "problem",
    problem_text_env = "problemtext",
    problemset_env = "problemset",
    problems_env = "problems",
    problemset_text_env = "problemsettext",
    header = c("\\usepackage{amsthm,amsmath,enumitem}",
        "\\theoremstyle{definition}\\newtheorem{problem}{Problem}",
        ltxnewenv("problemtext", "\\par", ""),
        ltxnewenv("problemset", "\\par", ""),
        ltxnewenv("problems", "\\par", ""),
        ltxnewenv("problemsettext", "\\par", ""),
        "\\newlist{answers}{enumerate}{1}",
        "\\setlist[answers]{label=(\\alph*)}")
    ))

#' Create a LaTeX header for \code{examiner} output
#'
#' This renders the contents of \code{opts_examiner$get('header')}.
#'
#' @return A character vector with the header
#' @export
examiner_latex_header <- function() {
    str_c(opts_examiner$get('header'), collapse = "\n")
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

#' Shuffle a vector
#'
#' Shuffle a vector, optionally holding some of the first and last observations constant.
#'
#' @param x vector to shuffle
#' @param first Do not shuffle the first \code{first} elements constant.
#' @param last Do not shuffle last \code{last} elements constant.
#' @return The suffled vector.
shuffle <- function(x, first = 0, last = 0) {
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
        answers <- shuffle(answers,
                           attr(x, "first"),
                           attr(x, "last"))
    }
    answerstr <- str_c("\\item", answers,
                       sep = " ",
                       collapse = "\n")
    str_c(beginenv(opts_examiner$get('problem_env')),
          beginenv(opts_examiner$get('problem_text_env')),
          str_c(as.character(x), collapse = "\n\n"),
          endenv(opts_examiner$get('problem_text_env')),          
          beginenv(opts_examiner$get('answers_env')),
          answerstr,
          endenv(opts_examiner$get('answers_env')),
          endenv(opts_examiner$get('problem_env')),
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
    str_c(beginenv(opts_examiner$get('problemset_env')),
          beginenv(opts_examiner$get('problemset_text_env')),
          str_c(as.character(x), collapse = "\n\n"),
          endenv(opts_examiner$get('problemset_text_env')),
          beginenv(opts_examiner$get('problems_env')),
          problemstr,
          endenv(opts_examiner$get('problems_env')),
          endenv(opts_examiner$get('problemset_env')),
          "\n", sep = "\n")
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
