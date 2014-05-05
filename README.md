# Examiner

An lightweight, flexible R package for generating multiple choice exams.


## Comparison to Alternatives

The package [exams](http://cran.r-project.org/web/packages/exams/index.html) is a more mature and full featured package that uses R to generate exams.
**exams** handles more question types and also allows output into HTML.
**examiner** is lighter weight, more flexible due to its use of templates, and uses some newer features like **knitr** to keep its code-base low.
In general, I'd probably recommend using **exams** for now; **examiner** is something of a personal project that has been extended into a package.
However, in the future **examiner** may be developed into a more full featured exam generation package.

There are several exam packages in LaTeX, some of which allow for randomization and shuffling of questions; see the CTAN topic [exams](http://www.ctan.org/topic/exam).
For whatever reason, I found them more complicated to deal with than using another language and templates to generate the output.
It was easier to customize and extend using R, than writing LaTeX macros.
Most importantly, a templating approach allows for non-LaTeX output.

<!--  LocalWords:  knitr LaTeX CTAN templating
 -->
