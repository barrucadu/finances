{% extends "wrapper" %}

{%- block title %}Balance Sheet{% endblock %}

{%- block scripts %}
  <script src="balancesheet.js" type="text/javascript"></script>
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
    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="2">Assets</th></tr>
        </thead>
        <tbody id="bs_assets_tbody"></tbody>
        <tfoot id="bs_assets_tfoot"></tfoot>
      </table>
    </div>

    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="2">Equity</th></tr>
        </thead>
        <tbody id="bs_equity_tbody"></tbody>
        <tfoot id="bs_equity_tfoot"></tfoot>
      </table>
    </div>

    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="2">Liabilities</th></tr>
        </thead>
        <tbody id="bs_liabilities_tbody"></tbody>
        <tfoot id="bs_liabilities_tfoot"></tfoot>
      </table>
    </div>
  </div>

  <div class="row">
    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="2">Income</th></tr>
        </thead>
        <tbody id="bs_income_tbody"></tbody>
        <tfoot id="bs_income_tfoot"></tfoot>
      </table>
    </div>

    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="2">Expenses</th></tr>
        </thead>
        <tbody id="bs_expenses_tbody"></tbody>
        <tfoot id="bs_expenses_tfoot"></tfoot>
      </table>
    </div>

    <div class="col">
      <table class="table table-sm">
        <thead>
          <tr><th colspan="2">Total</th></tr>
        </thead>
        <tbody id="bs_total_tbody"></tbody>
        <tfoot id="bs_total_tfoot"></tfoot>
      </table>
      <p class="small">Expanded accounting equation for double-entry accounting with signed amounts:<br/>
        <strong>assets + equity + expenses + income + liabilities = 0</strong></p>
    </div>
  </div>
{% endblock %}
