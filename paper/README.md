# README
This is the folder with the LaTeX source for the CS292C final project.

## Dependencies 
* Requires Markdown package (install with `tlmgr install markdown`)

## Compilation
For one-time compilation:

```
latexmk -xelatex --shell-escape main.tex
```

For automatic re-compilation when input file changes:

```
latexmk -pvc -xelatex --shell-escape --silent main.tex
```

Replace `-xelatex` with `-pdf` if xelatex is not installed and use pdflatex instead. 

