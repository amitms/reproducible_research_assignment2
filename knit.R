file<-"./README.md"
if (!nzchar) stop('Select a file first')
library(knitr)
library(markdown)
library(tools)
md_file <- knit(file)
html_file <- paste(file_path_sans_ext(md_file), '.html', sep = '')
markdownToHTML(md_file, html_file)