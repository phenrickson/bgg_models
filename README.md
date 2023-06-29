# about

*bgg_models* is a project for training predictive models using data from boardgamegeek (bgg). 

I train predictive models to estimate individual games at two levels: bgg community and bgg user.

Community models predict how the *bgg community as a whole* will evaluate games (average rating, number of user ratings, complexity) while user models estimate how *individual* users will evaluate games (likely to own).

My models are trained on game and collection data pulled via [BoardGameGeek's API](https://boardgamegeek.com/wiki/page/BGG_XML_API2) and stored via BigQuery.

## pre-requisities

This project requires:

1) Authentication to GCP and BigQuery for data loaded from bgg.
2) R (developed on version 4.3.0)

This project uses [*renv*](https://rstudio.github.io/renv/articles/renv.html) for package management and environment setup, and *renv.lock* can be used to restore the project state for running code.

## using the code

*_make_data* creates a local copy of the data used in training the model

### jobs

scripts that generate output (data and reports)

### src

contains the source code used in jobs

### notebooks

Rmarkdown notebooks used in generating reports
