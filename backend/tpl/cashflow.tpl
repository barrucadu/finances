{% extends "wrapper" %}

{%- block title %}Cashflow{% endblock %}

{%- block scripts %}
  <script src="cashflow.js" type="text/javascript"></script>
{% endblock %}

{%- block content %}
  <div id="cashflow_chart_container"></div>

  <div class="divider">
    <h2>Income Breakdown</h2>
  </div>

  <div id="income_breakdown_chart_container"></div>

  <div class="divider">
    <h2>Expenditure Breakdown</h2>
  </div>

  <div id="expenses_breakdown_chart_container"></div>
{% endblock %}
