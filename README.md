​
## Dependencies
​
```
$ brew install pkg-config (so that the package data.table does not fail installation)
```
​
Let's assume we have R installed and working. In the R console:
```
install.packages("DT")
install.packages("data.table")
install.packages("plotly")
install.packages("shiny")
```

## Run

Run the following command in the terminal:

```
$ R -e "shiny::runApp('csv_plot')"
```

## Screenshots

![Alt text](./screenshots/import_tab.png?raw=true "import_tab")

![Alt text](./screenshots/timeseries_tab.png?raw=true "timeseries_tab")

![Alt text](./screenshots/graph_tab.png?raw=true "graph_tab")
