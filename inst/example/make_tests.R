library("knitr")
library("tools")

## Input filename
IN <- "exam.Rnw"
## Number of exams to create
N <- 4

# Random seeds to use
SEEDS <- c(35752986, 97418519, 65506498, 10538337)
versions <- LETTERS[seq_len(N)]

latex <- function(file) {
    system2("xelatex",
            c("-interaction", "nonstopmode", "-file-line-error", file))
}

tex_to_pdf <- function(infile) {
    latex(infile)
    latex(infile)
}

compile_test <- function(file, version, seed) {
    VERSION <- version
    SEED <- seed
    IS_SOLUTION <- FALSE
    texfile <- sprintf("%s_%s_test.tex",
                       file_path_sans_ext(file), VERSION)
    knit(file, output=texfile)
    tex_to_pdf(texfile)
}

compile_solutions <- function(file, version, seed) {
    VERSION <- version
    SEED <- seed
    IS_SOLUTION <- TRUE
    texfile <- sprintf("%s_%s_solutions.tex",
                       file_path_sans_ext(file), VERSION)
    knit(file, output=texfile)
    tex_to_pdf(texfile)    
}


for (i in seq_len(N)) {
    compile_test(IN, versions[i], SEEDS[i])
    compile_solutions(IN, versions[i], SEEDS[i])
}
