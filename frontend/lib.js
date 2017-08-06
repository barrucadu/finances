// Month names
const MONTH_NAMES = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];


/*****************************************************************************
 * functions
 *****************************************************************************/

// Get the length of a month (0-based)
function daysInMonth(year, month) {
    let d = new Date(year, month + 1, 0);
    return d.getDate();
}

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

// Render finances for a given month, if the current page allows time
// scrolling.
function renderFinances(cb) {
    if (cached_data == undefined) {
        ajax('/data', data => {
            cached_data = data;
            cb(data);
        });
    } else {
        cb(cached_data);
    }
}


// Pretty-print an amount or delta.
function strAmount(amount, showPlus=false, showSymbol=true) {
    let sign = (amount > -0.01) ? ((amount > 0 && showPlus) ? '+' : '') : '-';
    let amt = Math.abs(amount).toFixed(2);
    let sym = showSymbol ? '£' : '';
    return `${sign}${sym}${amt}`;
}

// Pretty-print a date.
function strDate(date) {
    return `${date.getDate()} ${MONTH_NAMES[date.getMonth()]} ${date.getFullYear()}`;
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

    return `rgb(${(hash * 37 * str.length) % 255}, ${(hash * 131 * str.length) % 255}, ${(hash * 239 * str.length) % 255})`
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

// Get the most recent balance from a history report as of the given
// date.
function summariseHistory(history, date) {
    let amount = 0;
    for (let entry of history) {
        if (Date.parse(entry.date) <= date.getTime()) {
            amount = entry.amount;
        } else {
            break;
        }
    }
    return amount;
}

// Manage the month picker
function monthpicker(cb) {
    let month = new Date().getMonth();
    let matches = document.querySelectorAll('[data-month-picker]');

    for (let ele of matches) {
        let monthlist = document.createElement('div');
        monthlist.className = 'dropdown-menu';

        for (let i = 0; i < MONTH_NAMES.length; i ++) {
            let link = document.createElement('a');
            link.className = 'dropdown-item';
            link.href = '#';
            link.onclick = () => cb(i, ele);
            link.innerText = MONTH_NAMES[i];
            monthlist.appendChild(link);
        }

        ele.appendChild(monthlist);

        cb(month, ele);
    }
}


/*****************************************************************************
 * templates
 *****************************************************************************/

// The "assets", "equity", and "liabilities" balance sheet tables.
const TPL_BALANCE_TABLE_BODY = `
{{#category}}
  <tr class="category">
    <th colspan="2">{{title}}</th>
  </tr>
  {{#entry}}
    <tr>
      <td>{{name}}</td>
      <td class="text-right">{{amount}}</td>
    </tr>
  {{/entry}}
{{/category}}
`;
const TPL_BALANCE_TABLE_FOOT = `
<tr>
  <th>{{caption}}</th>
  <td class="text-right">{{value}}</td>
</tr>
`;

// The "income", "budget", and "expenses" summary tables.
const TPL_SUMMARY_TABLE_BODY = `
{{#entry}}
  <tr>
    <td>{{source}}</td>
    <td class="{{#good}}text-success{{/good}}{{#bad}}text-danger{{/bad}} text-right">{{delta}}</td>
    <td class="{{#abad}}text-danger{{/abad}} text-right">{{amount}}</td>
  </tr>
{{/entry}}
`;
const TPL_SUMMARY_TABLE_FOOT = `
<tr>
  <th>Total</th>
  <td class="{{#good}}text-success{{/good}}{{#bad}}text-danger{{/bad}} text-right">{{delta}}</td>
  <td class="{{#abad}}text-danger{{/abad}} text-right">{{amount}}</td>
</tr>
`;

// The "history" table.
const TPL_HISTORY_TABLE_BODY = `
{{#entry}}
  <tr>
    <th>{{day}}</th>
    <td{{#first.virtual}} class="virtual"{{/first.virtual}}>{{first.title}}</td>
    <td class="{{#first.good}}text-success{{/first.good}}{{#first.bad}}text-danger{{/first.bad}} text-right">{{first.delta}}</td>
  </tr>
  {{#rest}}
    <tr class="sub">
      <th></th>
      <td{{#virtual}} class="virtual"{{/virtual}}>{{title}}</td>
      <td class="{{#good}}text-success{{/good}}{{#bad}}text-danger{{/bad}} text-right">{{delta}}</td>
    </tr>
  {{/rest}}
{{/entry}}
`;
const TPL_HISTORY_TABLE_FOOT = `
<tr>
  <th colspan="2">Total</th>
  <td class="{{#good}}text-success{{/good}}{{#bad}}text-danger{{/bad}} text-right">{{delta}}</td>
</tr>
`;
