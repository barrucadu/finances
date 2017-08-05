{% extends "wrapper" %}

{%- block title %}Transaction History{% endblock %}

{%- block scripts %}
  <script src="history.js" type="text/javascript"></script>
{% endblock %}

{%- block header %}
  <form class="form-inline">
    <label class="sr-only" for="search">Search Transactions</label>
    <div class="input-group">
      <div class="input-group-addon"><i class="fa fa-search"></i></div>
      <input type="text" class="form-control" id="search" onkeyup="renderHistory()" placeholder="Search...">
    </div>
  </form>
{% endblock %}

{%- block content %}
  {% set months = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] %}
  {% for i in [12,11,10,9,8,7,6,5,4,3,2,1] %}
    <table id="history_{{ i }}_table" class="table table-sm">
      <thead>
        <tr><th colspan="3">{{ months[i - 1] }}</th></tr>
      </thead>
      <tbody id="history_{{ i }}_tbody"></tbody>
      <tfoot id="history_{{ i }}_tfoot"></tfoot>
    </table>
  {% endfor %}
{% endblock %}
