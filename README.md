Building
--------

### Backend

Build and run with `stack`:

```bash
cd backend
stack build
stack exec backend ../config.example.yaml
```

If no argument is given, a file `./config.yaml` is tried.

### Frontend

Fetch dependencies with `yarn`:

```back
cd frontend
yarn install
```

And copy everything to the directory your `config.yaml` says to serve
static files from:

```bash
mkdir -p ../web/vendor
cp node_modules/highcharts/highcharts.js    ../web/vendor/highcharts.min.js
cp node_modules/highcharts/modules/stock.js ../web/vendor/highstock.min.js
cp node_modules/mustache/mustache.min.js    ../web/vendor/mustache.min.js
cp src/* ../web/
```


Usage
-----

### Keybindings

- `left`:  go to prior month (if not January)
- `right`: go to next month (if not December)
- `r`:     re-draw charts
- `h`:     toggle history chart

### Clicky Things

- Account names in the assets legend
