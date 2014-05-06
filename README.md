

# Examiner

An lightweight, flexible R package for generating multiple choice exams.

This package is still **alpha**.
It came out of code I used to write multiple-choice exams for a course I was teaching.
It could change at any time, but I thought it may be useful to others, so I packaged it and put it here.

# Usage

Questions are written and stored in a yaml file,

```
---
pretext: |
  Who would cross the Bridge of Death must answer me these 
  questions three, 'ere the other side he see.
posttext: "Arrrrrrrrrrrrrgh!"
problems:
  - text: What is your name?
    correct: 2
    answers:
      - Sir Launcelot of Camelot
      - Arthur, King of the Britons
      - Sir Robin of Camelot
      - Sir Bedevere
      - Sir Galahad of Camelot
  - text: What is your quest?
    correct: 1
    answers:
      - To seek the Holy Grail
      - To kill ferocious bunnies
      - To invent silly walks
      - To find the meaning of life
  - text: What is the airspeed of an unladen swallow?
    correct: 5
    answers:
      - 25 mph
      - 20 mph
      - 30 mph
      - 40 mph
      - "African or European?"
```


Problems can then be loaded and formatted into LaTeX code.
This is intended to be used with knitr or Sweave.

```r
library("examiner")
```

```
Loading required package: methods
```

```r
problems <- problemset_from_yaml(system.file("yaml/questions.yaml", package = "examiner"))
cat(format(problems))
```

```
\begin{problemset}
\begin{problemsetpretext}
Who would cross the Bridge of Death must answer me these 
questions three, 'ere the other side he see.

\end{problemsetpretext}
\begin{problems}
\noindent \begin{minipage}{\textwidth}
\noindent
\begin{problem}
\begin{problemtext}
What is your name?
\end{problemtext}
\begin{answers}
\item   Sir Launcelot of Camelot 
\item   Arthur, King of the Britons 
\item   Sir Robin of Camelot 
\item   Sir Bedevere 
\item   Sir Galahad of Camelot 
\end{answers}
\end{problem}
\end{minipage}
\noindent \begin{minipage}{\textwidth}
\noindent
\begin{problem}
\begin{problemtext}
What is your quest?
\end{problemtext}
\begin{answers}
\item   To seek the Holy Grail 
\item   To kill ferocious bunnies 
\item   To invent silly walks 
\item   To find the meaning of life 
\end{answers}
\end{problem}
\end{minipage}
\noindent \begin{minipage}{\textwidth}
\noindent
\begin{problem}
\begin{problemtext}
What is the airspeed of an unladen swallow?
\end{problemtext}
\begin{answers}
\item   25 mph 
\item   20 mph 
\item   30 mph 
\item   40 mph 
\item   African or European? 
\end{answers}
\end{problem}
\end{minipage}
\end{problems}
\begin{problemsetposttext}
Arrrrrrrrrrrrrgh!
\end{problemsetposttext}
\end{problemset}
```


There are options to shuffle problems, and answers, and to show the solutions.
The appearance can be customized by redefining the LaTeX environments.
See the package vignettes for more examples.

**examiner** uses templates to format the questions, which allows flexibility in the formatting.
For example, to produce markdown,

```r
tpl_problem <- str_c("{{{text}}}", "\n", "{{{answers}}}", sep = "\n")

tpl_answerlist <- str_c("{{#answers}}", "1. {{{text}}}", "{{/answers}}", sep = "\n")

tpl_problemset <- str_c("{{{pretext}}}\n", "{{#problems}}", "{{{.}}}", "{{/problems}}", 
    "\n{{{posttext}}}", sep = "\n")

tpl_problemblock <- str_c("{{{pretext}}}\n", "{{#problems}}", "{{{.}}}", "{{/problems}}", 
    "{{{posttext}}}\n", sep = "\n")

problems <- problemset_from_yaml(system.file("yaml/questions.yaml", package = "examiner"))
cat(format(problems, tpl_answerlist = tpl_answerlist, tpl_problemset = tpl_problemset, 
    tpl_problemblock = tpl_problemblock, tpl_problem = tpl_problem))
```

Who would cross the Bridge of Death must answer me these 
questions three, 'ere the other side he see.


What is your name?


1. Sir Launcelot of Camelot
1. Arthur, King of the Britons
1. Sir Robin of Camelot
1. Sir Bedevere
1. Sir Galahad of Camelot

What is your quest?


1. To seek the Holy Grail
1. To kill ferocious bunnies
1. To invent silly walks
1. To find the meaning of life

What is the airspeed of an unladen swallow?


1. 25 mph
1. 20 mph
1. 30 mph
1. 40 mph
1. African or European?

Arrrrrrrrrrrrrgh!



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
