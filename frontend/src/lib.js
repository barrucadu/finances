// Month names
const MONTH_NAMES = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];

// The visible month, if the current page allows time scrolling.
var visible_month = 0;

// The latest retrieved data.
var cached_data = undefined;


/*****************************************************************************
 * functions
 *****************************************************************************/

// Make an ajax request and do something with the result
function ajax(url, cb) {
    let httpRequest = new XMLHttpRequest();
    httpRequest.onreadystatechange = function() {
        if(httpRequest.readyState === XMLHttpRequest.DONE && httpRequest.status === 200) {
            let data = JSON.parse(httpRequest.responseText);
            cb(data);
        }
    };
    httpRequest.open('GET', url);
    httpRequest.send();
}

// Render finances for the end of last month, if the current page
// allows time scrolling.
function renderFinancesForLastMonth(renderFinances) {
    if (visible_month > 1) {
        renderFinancesFor(renderFinances, visible_month - 1);
    }
}

// Render finances for the end of next month, if the current page
// allows time scrolling.
function renderFinancesForNextMonth(renderFinances) {
    if (visible_month < 12) {
        renderFinancesFor(renderFinances, visible_month + 1);
    }
}

// Render finances for a given month, if the current page allows time
// scrolling.
function renderFinancesFor(renderFinances, month=-1) {
    if(month == -1) {
        month = new Date().getMonth()+1
    }

    ajax(`/data?month=${month}`, data => {
        visible_month = month;
        cached_data   = data;
        renderFinances(month, data);
    });
}


// Pretty-print an amount or delta.
function strAmount(amount, showPlus=false, showSymbol=true) {
    let sign = (amount > -0.01) ? (showPlus ? '+' : '') : '-';
    let amt = Math.abs(amount).toFixed(2);
    let sym = showSymbol ? '£' : '';
    return `${sign}${sym}${amt}`;
}

// Check if an amount if roughly equal to zero.
function zeroish(val) {
    return val < 0.01 && val > -0.01;
}

// Generate a colour value by hashing a string.
function colour(str) {
    let hash = 0;
    for (let i = 0; i < str.length; i ++) {
        hash = ((hash << 5) - hash) + str.charCodeAt(i);
        hash |= 0;
    }
    hash = Math.abs(hash);

    return `rgb(${(hash * 37) % 255}, ${(hash * 131) % 255}, ${(hash * 239) % 255})`
}

// Default values for charts
function setChartDefaults() {
    Highcharts.setOptions({
        chart:    { backgroundColor: null },
        title:    { text: '' },
        subTitle: { text: '' },
        credits:  { enabled: false },
        plotOptions: {
            pie: {
                borderWidth: 2,
                shadow: false,
                center: ['50%', '50%'],
                tooltip: {
                    headerFormat: '<span style="color:{point.color}; font-weight: bold">{point.key}</span><br/>'
                }
            },
            series: {
                dataLabels: { enabled: false }
            }
        }
    });
}

// Gather balance data from an account report.
function gatherFromAccountReport(raw_data) {
    let out = {};
    for (let key in raw_data) {
        let datum = raw_data[key];
        for (let i = 0; i < datum.breakdown.length; i ++) {
            let account = datum.breakdown[i];
            if (account.amount == 0) continue;
            if (!(account.category in out)) {
                out[account.category] = [];
            }
            out[account.category].push({ name: account.name, amount: account.amount });
        }
    }
    return out;
}


/*****************************************************************************
 * templates
 *****************************************************************************/

// Partial for displaying a single tag in the tags legend.
const TPL_PART_SHOW_TAG = `
<tr>
  <td class="left">
    <span title="{{percentage}}% of overall allocation">{{amount}}</span>
  </td>
  <td class="right">{{tag}}</td>
  <td class="colour" style="background-color: {{colour}}">
    <span></span>
  </td>
</tr>
`;

// Partial for displaying a single account in the balances legend.
const TPL_PART_SHOW_ACCOUNT = `
<tr class="{{#name}}sub{{/name}} {{#hidden}}hidden{{/hidden}}">
  <td class="colour" style="background-color: {{colour}}">
    <span></span>
  </td>
  {{#subcolour}}
    <td class="colour" style="background-color: {{subcolour}}">
      <span></span>
    </td>
  {{/subcolour}}
  <td {{^subcolour}}colspan="2"{{/subcolour}}>
    {{^name}}
      <span onclick="{{onclick}}">{{asset}}</span>
    {{/name}}{{#name}}
      <span onclick="{{onclick}}">{{name}}</span>
    {{/name}}
    {{#url}}
      <a class="note" href="{{url}}" title="More information...">(?)</a>
    {{/url}}
  </td>
  <td class="right">
    {{^name}}
      <span title="{{percentage}}% of overall portfolio">{{amount}}</span>
    {{/name}}{{#name}}
      <span title="{{percentage}}% of {{asset}}">{{amount}}</span>
    {{/name}}
  </td>
</tr>
`;

// The "assets", "equity", and "liabilities" balance sheet tables.
const TPL_BALANCE_TABLE = `
{{#category}}
  <tr class="category">
    <th colspan="2">{{title}}</th>
  </tr>
  {{#entry}}
    <tr>
      <td>{{name}}</td>
      <td class="right">{{amount}}</td>
    </tr>
  {{/entry}}
{{/category}}
<tfoot>
  <tr>
    <th class="left">{{foot.caption}}</th>
    <td class="right">{{foot.value}}</td>
  </tr>
</tfoot>
`;

// The "income", "budget", and "expenses" summary tables.
const TPL_SUMMARY_TABLE = `
<tbody>
  {{#entry}}
    <tr>
      <td>{{source}}</td>
      <td class="{{#good}}good{{/good}}{{^good}}bad{{/good}} right">{{delta}}</td>
      <td class="right">{{amount}}</td>
    </tr>
  {{/entry}}
</tbody>
<tfoot>
  <tr>
    <th class="left">Total</th>
    <td class="{{#foot.good}}good{{/foot.good}}{{^foot.good}}bad{{/foot.good}} right">{{foot.delta}}</td>
    <td class="right">{{foot.amount}}</td>
  </tr>
</tfoot>
`;

// The "history" table.
const TPL_HISTORY_TABLE = `
<tbody>
  {{#entry}}
    <tr>
      <th>{{day}}</th>
      <td{{#first.virtual}} class="virtual"{{/first.virtual}}>{{first.title}}</td>
      <td class="{{#first.good}}good{{/first.good}}{{^first.good}}bad{{/first.good}}">{{first.delta}}</td>
    </tr>
    {{#rest}}
      <tr class="sub">
        <th></th>
        <td{{#virtual}} class="virtual"{{/virtual}}>{{title}}</td>
        <td class="{{#good}}good{{/good}}{{^good}}bad{{/good}}">{{delta}}</td>
      </tr>
    {{/rest}}
  {{/entry}}
</tbody>
<tfoot>
  <tr>
    <th class="left" colspan="2">Total</th>
    <td class="{{#foot.good}}good{{/foot.good}}{{^foot.good}}bad{{/foot.good}} right">{{foot.delta}}</td>
  </tr>
</tfoot>
`;
