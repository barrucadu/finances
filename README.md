Building
--------

### Backend

Build and run with `stack`:

```bash
stack build
stack exec webui
```

### Frontend

Fetch dependencies with `yarn`:

```back
yarn install
cp node_modules/chart.js/dist/Chart.min.js static/vendor/
cp node_modules/mustache/mustache.min.js static/vendor/
```
