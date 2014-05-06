#' examiner package
#'
#' A package to assist in the creation of multiple choice exams.
#' See the vignettes for examples.
#'
#' @name examiner
#' @docType package
#' @import yaml
#' @import stringr
#' @import whisker
#' @import plyr
#' @import methods
#' @export Counter
NULL

beginenv <- function(x) str_c("\\begin{", x, "}")
endenv <- function(x) str_c("\\end{", x, "}")
ltxnewenv <- function(x, begin = "", end = "") {
    str_c("\\newenvironment{", x, "}{", begin, "}{", end, "}")
}

shuffle <- function(x) {
    x[sample(seq_along(x), length(x), replace = FALSE)]
}

#' Counter class
#'
#' @examples
#' cnt <- Counter()
#' show(cnt)
#' cnt$add()
#' show(cnt)
#' cnt$add()
#' cnt$add()
#' show(cnt)
#' cnt$subtract()
#' show(cnt)
#' cnt$add(5)
#' show(cnt)
#' cnt$reset()
#' show(cnt)
#' @export
Counter <-
    setRefClass("Counter",
                fields = list(i = "integer"),
                methods = list(
                    add = function(x = 1L) {
                        i <<- i + as.integer(x)
                    },
                    subtract = function(x = 1L) {
                        i <<- i - as.integer(x)
                    },
                    reset = function(x = 0L) {
                        i <<- x
                    },
                    initialize = function(i = 0L) {
                        i <<- as.integer(i)
                    },
                    show = function() {
                        cat(sprintf("Counter: %d\n", i))
                    }
                    ))

#' Examiner Options
#'
#' This environment stores the options used in the \pkg{examiner} package.
#' @export
examiner_opts <- new.env(hash = TRUE)
examiner_opts$tpl_problem <-
    str_c("\\begin{minipage}{\\textwidth}",
          "\\noindent",
          "\\begin{problem}",
          "\\begin{problemtext}",
          "{{{text}}}",
          "\\end{problemtext}",
          "{{{answers}}}",
          "{{#show_solutions}}",
          "\\begin{solution}",
          "{{{solution}}}",
          "\\end{solution}",
          "{{/show_solutions}}",
          "\\end{problem}",
          "\\end{minipage}", sep = "\n")

examiner_opts$tpl_answerlist <-
    str_c("\\begin{answers}",
          "{{#answers}}",
          str_c("\\item",
                "{{#show_solutions}}",
                "{{#correct}}\\begin{correctanswer} {{{text}}} \\end{correctanswer} {{/correct}}",
                "{{^correct}}\\begin{wronganswer} {{{text}}} \\end{wronganswer} {{/correct}}",
                "{{/show_solutions}}",
                "{{^show_solutions}}",
                "{{{text}}}",
                "{{/show_solutions}}",
                sep = " "),
          "{{/answers}}",
          "\\end{answers}",
          sep = "\n")


examiner_opts$tpl_problemset <-
    str_c(
        "\\begin{problemset}",
        "\\begin{problemsetpretext}",
        "{{{pretext}}}",
        "\\end{problemsetpretext}",
        "\\begin{problems}",
        "{{#problems}}",
        "\\noindent {{{.}}}",
        "{{/problems}}",
        "\\end{problems}",
        "\\begin{problemsetposttext}",
        "{{{posttext}}}",
        "\\end{problemsetposttext}",
        "\\end{problemset}",
        sep = "\n")

examiner_opts$tpl_problemblock <-
    str_c(
        "\\begin{problemblock}",
        "\\begin{problemblockpretext}",
        "{{{pretext}}}",
        "\\end{problemblockpretext}",
        "\\begin{problems}",
        "{{#problems}}",
        "{{{.}}}",
        "{{/problems}}",
        "\\end{problems}",
        "\\begin{problemblockpretext}",
        "{{{posttext}}}",
        "\\end{problemblockpretext}",
        "\\end{problemblock}",
        sep = "\n")

examiner_opts$latex_header <-
    c("\\usepackage{amsthm,amsmath,enumitem}",
      "\\theoremstyle{definition}\\newtheorem{problem}{Problem}",
      ltxnewenv("problemset", "", ""),
      ltxnewenv("problemsetpretext", "\\par", ""),
      ltxnewenv("problemsetposttext", "\\par", ""),
      ltxnewenv("problems", "", ""),
      ltxnewenv("problemtext", "", ""),
      ltxnewenv("solution", "\\par \\color{blue}", ""),
      ltxnewenv("problemblock", "", ""),
      ltxnewenv("correctanswer", "\\color{blue} (*) ", ""),      
      ltxnewenv("wronganswer", "", ""),
      ltxnewenv("problemblockpretext", "\\par", ""),
      ltxnewenv("problemblockposttext", "\\par", ""),
      "\\newlist{answers}{enumerate}{1}",
      "\\setlist[answers]{label=(\\alph*),noitemsep,nosep}")

#' Create a LaTeX header for \code{examiner} output
#'
#' This renders the contents of \code{opts_examiner$get('header')}.
#'
#' @return A character vector with the header
#' @export
examiner_latex_header <- function() {
    str_c(examiner_opts[["latex_header"]], collapse = "\n")
}


#' Create an \code{answerlist} object
#'
#' @details
#' The class \code{"answerlist"} is a \code{"data.frame"} with columns:
#' \tabular{lll}{
#' text \tab character \tab Answer text \cr
#' corect \tab logical \tab Is the answer correct? \cr
#' fixed \tab logical \tab Can the answer be shuffled
#' }
#' 
#' @param x A data frame with colums: text, correct, and fixed. In \code{format}, the object.
#' @return An object of class \code{answerlist}.
#' @export
answerlist <- function(x) {
    x <- as.data.frame(x)
    class(x) <- c("answerlist", class(x))
    x
}

#' @export
#' @param show_solutions Whether to the show the solution
#' @param tpl_answerlist The template to use to render the object.
#' @param format_i A function used to format the answer number.
#' @param .debug Show the data used in the template.
#' @param ... Passed to the template.
#' @rdname answerlist
format.answerlist <- function(x, show_solutions = FALSE, 
                              tpl_answerlist = examiner_opts$tpl_answerlist,
                              format_i = identity,
                              .debug = FALSE,
                              ...) {
    x$i <- seq_len(nrow(x))
    x$i_fmt <- format_i(x$i)
    data <- c(list(answers = unname(rowSplit(x)),
                   show_solutions = show_solutions),
                   list(...))
    if (.debug) print(data)
    whisker.render(tpl_answerlist,
                   data = data)
}

shuffle_answers <- function(x) {
    indices <- seq_len(nrow(x))[!x[["fixed"]]]
    iorder <- sample(indices, length(indices))
    x[indices, ] <- x[iorder, , drop = FALSE]
    attr(x, "order") <- iorder
    x
}

#' Create a \code{problem} object
#'
#' @details
#' The class \code{"problem"} is a \code{"list"} with elements,
#' \describe{
#' \item{\code{text}}{\code{"character"}. The text for the problem.}
#' \item{\code{answers}}{\code{"answerlist"}. Possible answers to the problem.}
#' \item{\code{solution}}{\code{"character"}. Text with discussion of the answer to problem.}
#' \item{\code{randomizable}}{\code{"logical"}. Whether the answers can be shuffled.}
#' }
#'
#' @param text \code{character} vector with the prompt for the problem.
#' @param answers A \code{list} of \code{problem} or \code{problemset} objects.
#' @param correct Indices of answers which are correct.
#' @param first Number of observations at the beginning of \code{answers} which will not be shuffled.
#' @param last Number of observations at the end of \code{answers} which will not be shuffled.
#' @param solution Text for the solution to the problem.
#' @param randomizable Can the answers be shuffled?
#' @return An object of class \code{problem}.
#' @export
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
        answerlist(data.frame(text = sapply(answers, as.character), # used to convert heterog type list to character vector
                              correct = seq_along(answers) == correct,
                              fixed = seq_along(answers) %in% c(firsti, lasti),
                              stringsAsFactors = FALSE))
    .Data <-
        list(text = text,
             answers = answers,
             solution = as.character(solution),
             randomizable = as.logical(randomizable)[1])
    class(.Data) <- c("problem", "list")
    .Data
}

#' @export
#' @param x The object
#' @param shuffle_answers Shuffle answers?
#' @param show_solutions Display the solutions to the answers?
#' @param tpl_problem Whisker template to use when rendering the object.
#' @param counter A counter to keep track of the number of problems.
#' @param cnt_problem_1 A counter for the top level \code{problem} and \code{problemset} objects.
#' @param cnt_problem_2 \code{NULL} if the problem is not in a \code{problemblock}. Otherwise, the number within that \code{problemblock}.
#' @param format_cnt_problem_0 A function used to format \code{cnt_problem_0}.
#' @param format_cnt_problem_1 A function used to format \code{cnt_problem_1}.
#' @param format_cnt_problem_2 A function used to format \code{cnt_problem_2}.
#' @param .debug Useful for debugging. Shows the data that is passed to the template.
#' @param ... Used by the template and passed to \code{format.answerlist}.
#' @rdname problem
format.problem <- function(x,
                           shuffle_answers = FALSE,
                           show_solutions = FALSE,
                           tpl_problem = examiner_opts$tpl_problem,
                           counter = Counter(),
                           cnt_problem_2 = NULL,
                           cnt_problem_1 = 1L,
                           format_cnt_problem_1 = identity,
                           format_cnt_problem_2 = identity,
                           format_cnt_problem_0 = identity,
                           .debug = FALSE, ...) {
    x <- as.list(x)
    if (x[["randomizable"]] && as.logical(shuffle_answers)) {
        x[["answers"]] <- shuffle_answers(x[["answers"]])
    }
    counter$add()
    cnt_problem_0 <- counter$i
    cnt_problem_0_fmt <- format_cnt_problem_0(cnt_problem_0)
    cnt_problem_1_fmt <- format_cnt_problem_1(cnt_problem_1)
    cnt_problem_2_fmt <- format_cnt_problem_2(cnt_problem_2)
    x[["answers"]] <-
        format(x[["answers"]], show_solutions = show_solutions,
               cnt_problem_2 = cnt_problem_2,
               cnt_problem_1 = cnt_problem_1,
               cnt_problem_0 = cnt_problem_0,
               cnt_problem_1_fmt = cnt_problem_1_fmt,
               cnt_problem_2_fmt = cnt_problem_2_fmt,
               cnt_problem_0_fmt = cnt_problem_0_fmt, ...)
    data <- x
    data[["show_solutions"]] <- show_solutions
    data[["cnt_problem_1"]] <- cnt_problem_1
    data[["cnt_problem_2"]] <- cnt_problem_2
    data[["cnt_problem_0"]] <- cnt_problem_0
    data[["cnt_problem_1_fmt"]] <- cnt_problem_1_fmt
    data[["cnt_problem_2_fmt"]] <- cnt_problem_2_fmt
    data[["cnt_problem_0_fmt"]] <- cnt_problem_0_fmt
    data <- c(data, list(...))
    whisker.render(tpl_problem, data = data)
}

#' \code{problemblock} object
#'
#' @details
#' A \code{"problemblock"} object is a \code{"list"} with elements
#' \describe{
#' \item{\code{problems}}{The problems. A \code{"list"} of \code{"problem"} objects.}
#' \item{\code{pretext}}{The text to go before the problems.}
#' \item{\code{posttext}}{The text to go after the problems.}
#' }
#' 
#' @param problems list of problems
#' @param pretext \code{character} vector with text to go before the problems.
#' @param posttext \code{character} vector with text to go after the problems.
#' @param randomizable \code{logical} Whether the problems within the block can be shuffled.
#' @return A \code{problemblock} object.
#' @export
problemblock <- function(problems, pretext = "", posttext = "", randomizable = FALSE) {
    if (! inherits(problems, "list") ||
        !all(sapply(problems, inherits, what = "problem"))) {
        stop("problems must be a list of problem objects")
    }
    .Data <- list(problems = problems, pretext = pretext,
                  posttext = posttext, randomizable = FALSE)
    class(.Data) <- c("problemblock", "list")
    .Data
}

#' @export
#' @param x The object
#' @param shuffle_problems Shuffle problems?
#' @param shuffle_answers Shuffle answers?
#' @param show_solutions Display the solutions to the answers?
#' @param tpl_problemblock Whisker template to use when rendering the object.
#' @param format_cnt_problem_1 A function used to format 
#' @param cnt_problem_1 Problem number. This will usually be set by \code{format.problem}.
#' @param counter Counter object used for keeping track of the total number of problems in a problemset.
#' Usually used internally by \code{format.problem}.
#' @param .debug Useful for debugging. Shows the data that is passed to the template.
#' @param ... Used by the template and passed to \code{format.problem} for each problem.
#' @rdname problemblock
format.problemblock <- function(x,
                                shuffle_problems = FALSE,
                                shuffle_answers = FALSE,
                                show_solutions = FALSE,
                                tpl_problemblock = examiner_opts$tpl_problemblock,
                                format_cnt_problem_1 = identity,
                                cnt_problem_1 = 1L,
                                counter = Counter(),
                                .debug = FALSE,
                                ...) {
    data <- as.list(x)
    problems <- data[["problems"]]
    if (data[["randomizable"]] && shuffle_problems) {
        if (shuffle_problems) {
            problems <- shuffle(problems)
        }
    }
    for (i in seq_along(problems)) {
        problems[[i]] <-
            format(problems[[i]],
                   show_solutions = show_solutions,
                   shuffle_answers = shuffle_answers,
                   cnt_problem_1 = cnt_problem_1,
                   cnt_problem_2 = i,
                   counter = counter,
                   ...)
    }
    data[["problems"]] <- problems
    data[["cnt_problem_1"]] <- cnt_problem_1
    data[["cnt_problem_1_fmt"]] <- format_cnt_problem_1(cnt_problem_1)
    data <- c(data, list(...))
    if (.debug) print(data)
    whisker.render(tpl_problemblock, data = data)
}


#' \code{problemset} object
#'
#' @details
#' A \code{"problemset"} object is a \code{"list"} with elements
#' \describe{
#' \item{\code{problems}}{The problems. A \code{"list"} of \code{"problem"} objects.}
#' \item{\code{pretext}}{The text to go before the problems.}
#' \item{\code{posttext}}{The text to go after the problems.}
#' }
#' 
#' 
#' @param problems list of problems
#' @param pretext \code{character} vector with text to go before the problems.
#' @param posttext \code{character} vector with text to go after the problems.
#' @return A \code{problemset} object.
#' @export
problemset <- function(problems, pretext = "", posttext = "") {
    if (! inherits(problems, "list") ||
        !all(sapply(problems,
                    function(x) {
                        inherits(x, "problem") || inherits(x, "problemblock")
                    }))) {
        stop("problems must be a list of problem or problemblock objects")
    }
    .Data <- list(problems = problems, pretext = pretext,
                  posttext = posttext)
    class(.Data) <- c("problemset", "list")
    .Data
}

#' @rdname problemset
#'
#' @export
#' @param x The object.
#' @param shuffle_problems Shuffle problems?
#' @param shuffle_answers Shuffle answers?
#' @param show_solutions Display the solutions to the answers?
#' @param tpl_problemset Whisker template to use when rendering the object.
#' @param .debug Useful for debugging. Shows the data that is passed to the template.
#' @param ... Used by the template and passed to \code{format.problem} and \code{format.problemblock}.
#' @export
format.problemset <- function(x, shuffle_problems = FALSE, shuffle_answers = FALSE,
                              show_solutions = FALSE,
                              tpl_problemset = examiner_opts$tpl_problemset,
                              .debug = FALSE,
                              ...) {
    data <- as.list(x)
    problems <- data[["problems"]]
    counter <- Counter()
    if (shuffle_problems) {
        problems <- shuffle(problems)
    }
    for (i in seq_along(problems)) {
        problems[[i]] <- format(problems[[i]],
                                show_solutions = show_solutions,
                                shuffle_answers = shuffle_answers,
                                cnt_problem_1 = i,
                                counter = counter,
                                ...)
    }
    data[["problems"]] <- problems
    data <- c(data, list(...))
    if (.debug) print(data)
    whisker.render(tpl_problemset, data = data)
}

list2problem <- function(x) {
    if ("problems" %in% names(x)) {
        x[["problems"]] <- llply(x[["problems"]], function(x) do.call(problem, x))
        do.call(problemblock, x)
    } else {
        do.call(problem, x)
    }
}

#' Create problemset from yaml file
#'
#' @param input Name of the input file.
#' @return A \code{problemset} object.
#' @export
problemset_from_yaml <- function(input) {
    problemset_from_list(yaml.load_file(input))
}

#' Create problemset from yaml file
#'
#' @param x A \code{list}
#' @return A \code{problemset} object.
#' @export
problemset_from_list <- function(x) {
    x <- as.list(x)
    problems <- llply(x[["problems"]], list2problem)
    x[["problems"]] <- problems
    do.call(problemset, x)
}

roman <- function(x) tolower(as.roman(x))

Roman <- function(x) as.roman(x)

alph <- function(x) {
    i <- ((x - 1) %% 26) + 1
    n <- ((x - 1) %/% 26) + 1
    unname(mapply(function(ltr, n) paste0(rep(ltr, n), collapse = ""),
                  letters[i], n))
}

Alph <- function(x) {
    i <- ((x - 1) %% 26) + 1
    n <- ((x - 1) %/% 26) + 1
    unname(mapply(function(ltr, n) paste0(rep(ltr, n), collapse = ""),
                  LETTERS[i], n))
}


