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

Also fetch the webfonts:

- The Calluna Sans regular font face (the free one) from https://www.fontspring.com/fonts/exljbris/calluna-sans,
  saved as `CallunaSansRegular.woff2`
- The Noto Serif latin font face from https://fonts.google.com/?selection.family=Noto+Serif,
  saved as `NotoSerifRegular.woff2`

And copy everything to the directory your `config.yaml` says to serve
static files from:

```bash
mkdir -p ../web/vendor
cp node_modules/highcharts/highcharts.js    ../web/vendor/highcharts.min.js
cp node_modules/highcharts/modules/stock.js ../web/vendor/highstock.min.js
cp node_modules/mustache/mustache.min.js    ../web/vendor/mustache.min.js
cp CallunaSansRegular.woff2 NotoSerifRegular.woff2 ../web/fonts/
cp src/* ../web/
```


Usage
-----

### Keybindings

- `left`:  go to prior month (if not January)
- `right`: go to next month (if not December)
- `r`: re-draw charts
- `s`: show asset summary
- `h`: show historical balances
- `c`: show monthly cashflow
- `b`: show a balance sheet

### Clicky Things

- Account names in the assets legend
