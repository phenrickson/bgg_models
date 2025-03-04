# about

*bgg_models* is a project for training predictive models using data from boardgamegeek (bgg). 

these models predict how the *bgg community as a whole* will evaluate games (average rating, number of user ratings, complexity). models are trained on game and collection data pulled via [BoardGameGeek's API](https://boardgamegeek.com/wiki/page/BGG_XML_API2) and stored via GoogleCloudStorage

## reproducibillity

- set token for authentication to Google Cloud Storage
- install R and restore project dependencies with `renv::restore()`
- run pipeline with `targets::tar_make()`