{% extends "wrapper" %}

{%- block title %}Summary{% endblock %}

{%- block scripts %}
  <script src="history.js" type="text/javascript"></script>
  <script src="finance.js" type="text/javascript"></script>
{% endblock %}

{%- block header %}
  <div class="input-group">
    <div class="btn-group monthpicker" data-month-picker="true">
      <div class="input-group-addon"><i class="fa fa-calendar" aria-hidden="true"></i></div>
      <button type="button" class="btn btn-secondary dropdown-caption">
        <span id="month-name">Month</span>
      </button>
      <button type="button" class="btn btn-secondary dropdown-toggle dropdown-toggle-split" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
        <span class="sr-only">Toggle Dropdown</span>
      </button>
    </div>
  </div>
{% endblock %}

{%- block content %}
  <div class="row">
    <div class="col"><div id="allocation_chart_container"></div></div>
    <div class="col"><div id="balances_chart_container"></div></div>
  </div>

  <div class="divider">
    <h2>Cashflow</h2>
  </div>

  <div class="row">
    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="3">Envelope Budget</th></tr>
        </thead>
        <tbody id="cur_budget_tbody"></tbody>
        <tfoot id="cur_budget_tfoot"></tfoot>
      </table>
    </div>

    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="3">Income</th></tr>
        </thead>
        <tbody id="cur_income_tbody"></tbody>
        <tfoot id="cur_income_tfoot"></tfoot>
      </table>
      <p class="small">Deltas are compared to <span id="cur_income_prior_date"></span>.</p>
    </div>

    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="3">Expenses</th></tr>
        </thead>
        <tbody id="cur_expenses_tbody"></tbody>
        <tfoot id="cur_expenses_tfoot"></tfoot>
      </table>
      <p class="small">Deltas are compared to <span id="cur_expenses_prior_date"></span>.</p>
    </div>
  </div>

  <div class="divider">
    <h2>Transactions</h2>
  </div>

  <div class="row">
    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="3">Recent Transactions</th></tr>
        </thead>
        <tbody id="history_recent_tbody"></tbody>
        <tfoot id="history_recent_tfoot"></tfoot>
      </table>
    </div>
  </div>
{% endblock %}
