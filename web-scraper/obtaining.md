# Obtaining Data

We obtained the raw data using `Beautiful Soup` in `Python`.

> We extended the code written by [vnherdeiro](https://github.com/vnherdeiro/transfermarkt-scraping/commits?author=vnherdeiro "View all commits by vnherdeiro") and this is the [repository](https://github.com/vnherdeiro/transfermarkt-scraping).

Our contribution was to extract more features and collect every type of player.

## Results

`../data/performance.csv`

> Feature list
>
> name | age | height | position | sub position | foot | current club | contract expires | market value
>
> 17/18 games | 17/18 goals | 17/18 assists | 17/18 yellows | 17/18 second yellows | 17/18 reds | 17/18 minutes
>
> 18/19 games | 18/19 goals | 18/19 assists | 18/19 yellows | 18/19 second yellows | 18/19 reds | 18/19 minutes
>
> 19/20 games | 19/20 goals | 19/20 assists | 19/20 yellows | 19/20 second yellows | 19/20 reds | 19/20 minutes
>
> 20/21 games | 20/21 goals | 20/21 assists | 20/21 yellows | 20/21 second yellows | 20/21 reds | 20/21 minutes
>
> current league

## script

- `main.py` access the website and extracts a list of the top N (=10) leagues and at the end, flattens the data and save everything into a `csv` file
- `league.py` for every league extracts the list of teams
- `team.py` for every team extracts the list of players filtering out the goalkeepers
- `player.py` downloads the information of a single player: his personal attributes and for every season the relevant statistics
