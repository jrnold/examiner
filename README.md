# Examiner

An lightweight, flexible R package for generating multiple choice exams.

# Usage

Questions are written and stored in a yaml file,

```
## ---
## pretext: |
##   Who would cross the Bridge of Death must answer me these 
##   questions three, 'ere the other side he see.
## posttext: "Arrrrrrrrrrrrrgh!"
## problems:
##   - text: What is your name?
##     correct: 2
##     answers:
##       - Sir Launcelot of Camelot
##       - Arthur, King of the Britons
##       - Sir Robin of Camelot
##       - Sir Bedevere
##       - Sir Galahad of Camelot
##   - text: What is your quest?
##     correct: 1
##     answers:
##       - To seek the Holy Grail
##       - To kill ferocious bunnies
##       - To invent silly walks
##       - To find the meaning of life
##   - text: What is the airspeed of an unladen swallow?
##     correct: 5
##     answers:
##       - 25 mph
##       - 20 mph
##       - 30 mph
##       - 40 mph
##       - "African or European?"
```


Problems can then be loaded and formatted into LaTeX code.
This is intended to be used with knitr or Sweave.

```r
problems <- problemset_from_yaml(system.file("yaml/questions.yaml", package = "examiner"))
```

```
## Error: could not find function "problemset_from_yaml"
```

```r
format(problems)
```

```
## Error: object 'problems' not found
```

There are options to shuffle problems, and answers, and to show the solutions.
See the package vignette for more examples.

**examiner** uses templates to format the questions, which allows flexibility in the formatting.
For example, to produce markdown,

```r
tpl_problem <- str_c("{{{text}}}", "\n", "{{{answers}}}", sep = "\n")
```

```
## Error: could not find function "str_c"
```

```r

tpl_answerlist <- str_c("{{#answers}}", "1. {{{text}}}", "{{/answers}}", sep = "\n")
```

```
## Error: could not find function "str_c"
```

```r

tpl_problemset <- str_c("{{{pretext}}}\n", "{{#problems}}", "{{{.}}}", "{{/problems}}", 
    "\n{{{posttext}}}", sep = "\n")
```

```
## Error: could not find function "str_c"
```

```r

tpl_problemblock <- str_c("{{{pretext}}}\n", "{{#problems}}", "{{{.}}}", "{{/problems}}", 
    "{{{posttext}}}\n", sep = "\n")
```

```
## Error: could not find function "str_c"
```

```r

problems <- problemset_from_yaml(system.file("yaml/questions2.yaml", package = "examiner"))
```

```
## Error: could not find function "problemset_from_yaml"
```

```r
cat(format(problems, tpl_answerlist = tpl_answerlist, tpl_problemset = tpl_problemset, 
    tpl_problemblock = tpl_problemblock, tpl_problem = tpl_problem))
```

```
## Error: object 'problems' not found
```



# Comparison to Alternatives

The package [exams](http://cran.r-project.org/web/packages/exams/index.html) is a more mature and full featured package that uses R to generate exams.
**exams** handles more question types and also allows output into HTML.
**examiner** is lighter weight, more flexible due to its use of templates, and uses some newer features like **knitr**.
In general, I'd probably recommend using **exams** for now; **examiner** is something of a personal project that has been extended into a package.
However, in the future **examiner** may be developed into a more full featured exam generation package.

There are several exam packages in LaTeX, some of which allow for randomization and shuffling of questions; see the CTAN topic [exams](http://www.ctan.org/topic/exam).
For whatever reason, I found them more complicated to deal with than using another language and templates to generate the output.
It was easier to customize and extend using R, than writing LaTeX macros.
Most importantly, a templating approach allows for non-LaTeX output.

<!--  LocalWords:  knitr LaTeX CTAN templating
 -->
