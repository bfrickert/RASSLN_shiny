##RASSLN_shiny
Companion to [RASSLN](https://github.com/bfrickert/RASSLN). It process that `full.tsv` file created by that project and performs natural language processing and a logistic regression to *predict outcomes of WWE matches based on opponent move sets*.

It also includes code to create a [Shiny R](http://shiny.rstudio.com/) data app to interact with the model and see what moves were statistically effective or ineffective against your favorite WWE pro wrestlers.

1. Move the `full.tsv` created by RASSLN to the `data` directory.
2. Run `compile.data.R`.
3. [Deploy](http://shiny.rstudio.com/deploy/) Shiny app.