<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{exam3}
-->

# Final: The Bridge of Death




\begin{problemset}
\begin{problemsetpretext}

\end{problemsetpretext}
\begin{problems}
\begin{problem}
\begin{problemtext}
What is your name?
\end{problemtext}
\begin{answers}
\item

Sir Launcelot of Camelot
\item

Arthur, King of the Britons
\item

Sir Robin of Camelot
\item

Sir Bedevere
\item

Sir Galahad of Camelot

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What is your quest?
\end{problemtext}
\begin{answers}
\item

To seek the Holy Grail
\item

To kill ferocious bunnies
\item

To develop silly walks
\item

To find the meaning of life
\item

I have no quest

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What is your favorite color?
\end{problemtext}
\begin{answers}
\item

blue
\item

green
\item

red
\item

yellow
\item

purple

\end{answers}
\end{problem}
\begin{problemblock}
\begin{problemblockpretext}

\end{problemblockpretext}
\begin{problems}
\begin{problem}
\begin{problemtext}
What is your name?
\end{problemtext}
\begin{answers}
\item

Sir Launcelot of Camelot
\item

Arthur, King of the Britons
\item

Patsy
\item

Sir Robin of Camelot
\item

Sir Bedevere
\item

Sir Galahad of Camelot

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What is your quest?
\end{problemtext}
\begin{answers}
\item

To seek the Holy Grail
\item

To kill ferocious bunnies
\item

To develop silly walks
\item

To find the meaning of life

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What is the capital of Assyria?
\end{problemtext}
\begin{answers}
\item

Ninevah
\item

Jerusalem
\item

Paris
\item

Damascus
\item

I don't know that!

\end{answers}
\end{problem}
\end{problems}
\begin{problemblockpretext}

\end{problemblockpretext}
\end{problemblock}
\begin{problemblock}
\begin{problemblockpretext}

\end{problemblockpretext}
\begin{problems}
\begin{problem}
\begin{problemtext}
What is your name?
\end{problemtext}
\begin{answers}
\item

Sir Launcelot of Camelot
\item

Arthur, King of the Britons
\item

Sir Robin of Camelot
\item

Sir Bedevere
\item

Sir Galahad of Camelot

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What is your quest?
\end{problemtext}
\begin{answers}
\item

To seek the Holy Grail
\item

To kill ferocious bunnies
\item

To develop silly walks
\item

To find the meaning of life

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What your favorite color?
\end{problemtext}
\begin{answers}
\item

blue
\item

green
\item

red
\item

yellow
\item

purple

\end{answers}
\end{problem}
\end{problems}
\begin{problemblockpretext}

\end{problemblockpretext}
\end{problemblock}
\begin{problemblock}
\begin{problemblockpretext}

\end{problemblockpretext}
\begin{problems}
\begin{problem}
\begin{problemtext}
What is your name?
\end{problemtext}
\begin{answers}
\item

Sir Launcelot of Camelot
\item

Arthur, King of the Britons
\item

Sir Robin of Camelot
\item

Sir Bedevere
\item

Sir Galahad of Camelot

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What is your quest?
\end{problemtext}
\begin{answers}
\item

To seek the Holy Grail
\item

To kill ferocious bunnies
\item

To develop silly walks
\item

To find the meaning of life

\end{answers}
\end{problem}
\begin{problem}
\begin{problemtext}
What is the airspeed of an unladen swallow?
\end{problemtext}
\begin{answers}
\item

25 mph
\item

20 mph
\item

30 mph
\item

40 mph
\item

African or European?

\end{answers}
\end{problem}
\end{problems}
\begin{problemblockpretext}

\end{problemblockpretext}
\end{problemblock}
\end{problems}
\begin{problemsetposttext}

\end{problemsetposttext}
\end{problemset}



```r
for (i in ls(examiner_opts)) {
    print(i)
    print(examiner_opts[[i]])
}
```

```
## [1] "format_ia"
## function (x) 
## x
## <bytecode: 0x4c626e8>
## <environment: namespace:base>
## [1] "format_N0"
## function (x) 
## x
## <bytecode: 0x4c628e0>
## <environment: namespace:base>
## [1] "format_N1"
## function (x) 
## x
## <bytecode: 0x4c62838>
## <environment: namespace:base>
## [1] "format_N2"
## function (x) 
## x
## <bytecode: 0x4c62790>
## <environment: namespace:base>
## [1] "latex_header"
##  [1] "\\usepackage{amsthm,amsmath,enumitem}"                   
##  [2] "\\theoremstyle{definition}\\newtheorem{problem}{Problem}"
##  [3] "\\newenvironment{problemset}{\\par}{}"                   
##  [4] "\\newenvironment{problemsetpretext}{\\par}{}"            
##  [5] "\\newenvironment{problemsetposttext}{\\par}{}"           
##  [6] "\\newenvironment{problems}{\\par}{}"                     
##  [7] "\\newenvironment{problemtext}{\\par}{}"                  
##  [8] "\\newenvironment{solution}{\\par}{}"                     
##  [9] "\\newenvironment{problemblock}{\\par}{}"                 
## [10] "\\newenvironment{problemblockpretext}{\\par}{}"          
## [11] "\\newenvironment{problemblockposttext}{\\par}{}"         
## [12] "\\newlist{answers}{enumerate}{1}"                        
## [13] "\\setlist[answers]{label=(\\alph*)}"                     
## [1] "tpl_answers"
## [1] "{{#answers}}\n    1. {{{text}}}\n{{/answers}}"
## [1] "tpl_problem"
## [1] "{{{text}}}\n\n\n{{{answers}}}"
## [1] "tpl_problemblock"
## [1] "    {{{pretext}}}\n\n    {{#problems}}\n    1. {{{.}}}\n    {{/problems}}\n    {{{posttext}}}\n"
## [1] "tpl_problemset"
## [1] "{{{pretext}}}\n\n{{#problems}}\n1. {{{.}}}\n{{/problems}}\n\n{{{posttext}}}"
```

