Building
--------

### Backend

Build and run with `stack`:

```bash
cd backend
stack build
stack exec backend
```

You need a `config.yaml` in the same directory.  See
`config.example.yaml` for inspiration.

### Frontend

Fetch dependencies with `yarn`:

```back
cd frontend
yarn install
```

And copy everything to the directory your `config.yaml` says to serve
static files from:

```bash
mkdir -p ../backend/static/vendor
cp node_modules/highcharts/highcharts.js    ../backend/static/vendor/highcharts.min.js
cp node_modules/highcharts/modules/stock.js ../backend/static/vendor/highstock.min.js
cp node_modules/mustache/mustache.min.js    ../backend/static/vendor/mustache.min.js
cp src/* ../backend/static/
```
