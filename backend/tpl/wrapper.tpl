{# if you want to use a CDN, set this to "true" #}
{% set use_cdn = false %}

{% set navlinks =
    [ {"title": "Summary",             "page": "index",        "icon": "home"}
    , {"title": "Balance Sheet",       "page": "balancesheet", "icon": "balance-scale"}
    , {"title": "Cashflow",            "page": "cashflow",     "icon": "bank"}
    , {"title": "Transaction History", "page": "history",      "icon": "list"}
    ]
%}
<!DOCTYPE html>
<html>
  <head>
    <title>{% block title %}{% endblock %}</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    {% if use_cdn %}
      <link rel="stylesheet" crossorigin="anonymous" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css">
      <link rel="stylesheet" crossorigin="anonymous" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
    {% else %}
      <link rel="stylesheet" href="vendor/bootstrap/css/bootstrap.min.css">
      <link rel="stylesheet" href="vendor/font-awesome/css/font-awesome.min.css">
    {% endif %}
    <link rel="stylesheet" href="style.css" type="text/css">
  </head>
  <body>
    <nav id="navbar">
      <ul class="nav nav-pills flex-column">
        {% for entry in navlinks %}
          <li class="nav-item"><a href="/{{entry.page}}.html"
                                  class="nav-link {% if (page == entry.page) %}active{% endif %}"
                                  data-toggle="tooltip"
                                  data-placement="right"
                                  title="{{ entry.title }}">
              <span><i class="fa fa-lg fa-{{ entry.icon }}" aria-hidden="true" aria-label="{{ entry.title }}"></i></span>
          </a></li>
        {% endfor %}
      </ul>
    </nav>

    <div class="container">
      <header class="container">
        <div class="row">
          <div class="col align-self-start">
            <small class="text-muted text-uppercase">Finances</small>
            <h1>{% block title %}{% endblock %}</h1>
          </div>

          <div class="col col-auto align-self-end">
            {% block header %}{% endblock %}
          </div>
        </div>
      </header>

      {% block content %}{% endblock %}
    </div>

    {% if use_cdn %}
      <script crossorigin="anonymous" src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
      <script crossorigin="anonymous" src="https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js"></script>
      <script crossorigin="anonymous" src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js"></script>
      <script crossorigin="anonymous" src="https://cdnjs.cloudflare.com/ajax/libs/highcharts/5.0.14/highstock.js"></script>
      <script crossorigin="anonymous" src="https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.min.js"></script>
    {% else %}
      <script src="vendor/jquery.min.js"></script>
      <script src="vendor/tether.min.js"></script>
      <script src="vendor/bootstrap/js/bootstrap.min.js"></script>
      <script src="vendor/highstock.js"></script>
      <script src="vendor/mustache.min.js"></script>
    {% endif %}
    <script src="lib.js"></script>
    {% block scripts %}{% endblock %}
    <script>
      setChartDefaults();
      $('[data-toggle="tooltip"]').tooltip();
    </script>
  </body>
</html>
