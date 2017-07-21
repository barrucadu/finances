const THIS_MONTH = new Date().getMonth() + 1;

var visible_month = 0;
var cached_assets_data = undefined;
var show_history = false;
var hidden_accounts = {};

function hoverCallback(f) {
    return function (tooltipItem, data) {
        let general_label  = data.labels[tooltipItem.index];
        let specific_label = data.datasets[tooltipItem.datasetIndex].label;
        let label = (specific_label == undefined) ? general_label : specific_label;
        let value = data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
        return `${label}: ${f(value)}`;
    }
}

function isHidden(asset, account=null) {
    let k = (account === null) ? asset.name : `${asset.name}${account.name}`;
    return asset.name in hidden_accounts || k in hidden_accounts;
}

function toggleHide(asset, account=null) {
    function toggle(k) {
        if (k in hidden_accounts) {
            delete hidden_accounts[k];
        } else {
            hidden_accounts[k] = true;
        }
    }

    if (account == null) {
        // unhide all the breakdowns
        for (let i = 0; i < asset.breakdown.length; i ++) {
            let k = `${asset.name}${asset.breakdown[i].name}`;
            if (k in hidden_accounts) {
                delete hidden_accounts[k];
            }
        }

        // then toggle the state of the asset
        toggle(asset.name)
    } else {
        // if the asset is hidden, unhide it, then hide all the
        // breakdowns except this one; otherwise just toggle the state
        // of this one.
        if (isHidden(asset)) {
            toggle(asset.name);

            for (let i = 0; i < asset.breakdown.length; i ++) {
                if (asset.breakdown[i].name == account.name) continue;
                toggle(`${asset.name}${asset.breakdown[i].name}`);
            }
        } else {
            toggle(`${asset.name}${account.name}`);
        }
    }

    renderAssets(cached_assets_data);
}

function colour(str) {
    let total = 0;
    for (let i = 0; i < str.length; i ++) {
        total += str.charCodeAt(i);
    }

    return `rgb(${total % 255}, ${(total * 7) % 255}, ${(total * 13) % 255})`
}

function zeroish(val) {
    return val < 0.01 && val > -0.01;
}

function zeroise(val) {
    return (val > -0.01) ? val : 0
}

function range (start, end) {
    return [...Array(1 + end - start).keys()].map(v => start + v)
}

function randRange(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

function strAmount(amount, showPlus=false) {
    let sign = (amount > -0.01) ? (showPlus ? '+' : '') : '-';
    let amt = Math.abs(amount).toFixed(2);
    return `${sign}£${amt}`;
}

function renderAssetsTagsChartAndLegend(raw_assets_data) {
    let tagsAmounts  = {};
    let overallTotal = 0;

    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        let assetAmount = asset.breakdown.reduce((acc, d) => isHidden(asset, d) ? acc : acc + d.amount, 0);
        overallTotal += assetAmount;

        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];
            let accountAmount = isHidden(asset, account) ? 0 : account.amount;
            let totalShare = account.tags.reduce((acc, d) => acc + d.share, 0);

            for (let j = 0; j < account.tags.length; j ++) {
                let tag = account.tags[j];
                let tagPortion = tag.share / totalShare;

                if (!(tag.tag in tagsAmounts)) {
                    tagsAmounts[tag.tag] = 0;
                }

                tagsAmounts[tag.tag] += accountAmount * tagPortion;
            }
        }
    }

    let tagsData      = [];
    let legendEntries = [];

    for (let tag in tagsAmounts) {
        let amount = tagsAmounts[tag];

        if (zeroish(amount)) continue;

        tagsData.push({
            name: tag,
            y: amount,
            color: colour(tag),
        });

        legendEntries.push({
            tag: tag,
            amount: strAmount(amount),
            percentage: (100 * amount / overallTotal).toFixed(2),
            colour: colour(tag)
        });
    }

    Highcharts.chart('assets_tags_chart_container', {
        chart: { type: 'pie' },
        series: [{
            name: 'Allocation',
            data: tagsData,
            size: '80%',
            tooltip: {
                pointFormatter: function() {
                    return `${strAmount(this.y)} (${(100*this.y/overallTotal).toFixed(2)}% of overall allocation)<br/>`;
                }
            }
        }]
    });

    document.getElementById('assets_tags_legend_container').removeChild(document.getElementById('assets_tags_legend'));
    let legend = document.createElement('table');
    legend.id = 'assets_tags_legend';
    legend.innerHTML = Mustache.render(
        TPL_LEGEND_TABLE,
        { 'entry': legendEntries },
        { 'show_entry': TPL_PART_SHOW_TAG }
    );
    document.getElementById('assets_tags_legend_container').appendChild(legend);
}

function renderAssetsHistoryChart(raw_assets_data) {
    let series  = {};
    let enabled = {};

    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];

        if(isHidden(asset)) continue;

        if (!(asset.name in series)) {
            series[asset.name]  = {};
            enabled[asset.name] = 0;
            for (let i = 0; i < asset.breakdown.length; i ++) {
                let account = asset.breakdown[i];
                if (isHidden(asset, account)) continue;
                series[`${asset.name} (${account.name})`] = {};
                enabled[asset.name] += 1;
            }
        }

        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];
            if (isHidden(asset, account)) continue;

            for (let j = 0; j < account.history.length; j ++) {
                let entry  = account.history[j];
                let date   = new Date(entry.date).getTime();
                let amount = entry.amount;

                if (enabled[asset.name] > 1) {
                    series[`${asset.name} (${account.name})`][date] = zeroish(amount) ? undefined : amount;
                }

                if(!(date in series[asset.name]) || series[asset.name][date] == undefined) {
                    series[asset.name][date] = zeroish(amount) ? undefined : amount;
                } else {
                    series[asset.name][date] += zeroish(amount) ? 0 : amount;
                }
            };
        }
    }

    Highcharts.stockChart('assets_history_chart_container', {
        chart: { zoomType: 'x' },
        yAxis: {
            title: { text: 'Balance (£)' }
        },
        series: Object.keys(series).map(key => {
            return {
                name: key,
                data: Object.keys(series[key]).map(date => [parseInt(date), zeroish(series[key][date]) ? undefined : series[key][date]]),
                color: colour(key),
                step: true,
                tooltip: {
                    pointFormatter: function() {
                        return zeroish(this.y) ? '' : `<span style="color:${this.series.color}; font-weight: bold">${this.series.name}</span> ${strAmount(this.y)}<br/>`;
                    }
                }
            }
        })
    });
}

function renderAssetsSnapshotChart(raw_assets_data) {
    let assetData    = [];
    let accountData  = [];
    let assetTotals  = {};
    let overallTotal = 0;

    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        let assetAmount = asset.breakdown.reduce((acc, d) => isHidden(asset, d) ? acc : acc + d.amount, 0);
        overallTotal += assetAmount;

        if(zeroish(assetAmount)) continue;

        assetData.push({
            name: asset.name,
            y: assetAmount,
            color: colour(asset.name)
        });

        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];
            let amount = isHidden(asset, account) ? 0 : account.amount;

            if (zeroish(amount)) continue;

            accountData.push({
                name: account.name,
                y: amount,
                color: colour((account.name == asset.name) ? asset.name : `${asset.name} (${account.name})`),
                asset: {name: asset.name, amount: assetAmount}
            });
        }
    }

    Highcharts.chart('assets_balances_chart_container', {
        chart: { type: 'pie' },
        series: [{
            name: 'Accounts',
            data: assetData,
            size: '60%',
            tooltip: {
                pointFormatter: function() {
                    return `${strAmount(this.y)} (${(100*this.y/overallTotal).toFixed(2)}% of overall portfolio)<br/>`;
                }
            }
        }, {
            name: 'Breakdown',
            data: accountData,
            size: '80%',
            innerSize: '60%',
            id: 'versions',
            tooltip: {
                pointFormatter: function() {
                    return `${strAmount(this.y)} (${(100*this.y/this.asset.amount).toFixed(2)}% of ${this.asset.name})<br/>`;
                }
            }
        }]
    });
}

function renderAssetsBalancesLegend(raw_assets_data) {
    let overalltotal = raw_assets_data.reduce((acc, ass) => acc + ass.breakdown.reduce((acc2, d) => acc2 + d.amount, 0), 0);

    let entries = [];
    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        let total = asset.breakdown.reduce((acc, d) => acc + d.amount, 0)

        let subentries = [];
        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];

            if (zeroish(account.amount)) continue;

            subentries.push({
                'name': account.name,
                'url': account.url,
                'colour': colour(asset.name),
                'subcolour': colour(`${asset.name} (${account.name})`),
                'amount': strAmount(account.amount),
                'percentage': Math.abs(100 * account.amount / total).toFixed(0),
                'hidden': isHidden(asset, account),
                'onclick': `toggleHide(${JSON.stringify(asset)},${JSON.stringify(account)})`
            });
        }

        if (!zeroish(total)) {
            entries.push({
                'asset': asset.name,
                'url': asset.url,
                'colour': colour(asset.name),
                'amount': strAmount(total),
                'percentage': Math.abs(100 * total / overalltotal).toFixed(0),
                'subentry': (subentries.length == 1 && subentries[0].name == asset.name) ? [] : subentries,
                'hidden': isHidden(asset),
                'onclick': `toggleHide(${JSON.stringify(asset)})`
            });
        }
    }

    let legend = document.createElement('table');
    legend.id = 'assets_balances_legend';
    legend.innerHTML = Mustache.render(
        TPL_LEGEND_TABLE,
        { 'entry': entries },
        { 'show_entry': TPL_PART_SHOW_ACCOUNT }
    );
    return legend;
}

function renderAssets(raw_assets_data) {
    cached_assets_data = raw_assets_data;

    // tags
    renderAssetsTagsChartAndLegend(raw_assets_data);

    // balances or history chart
    document.getElementById('assets_tags_legend_container').style.display    = show_history ? 'none'  : 'flex';
    document.getElementById('assets_tags_chart_container').style.display     = show_history ? 'none'  : 'block';
    document.getElementById('assets_balances_chart_container').style.display = show_history ? 'none'  : 'block';
    document.getElementById('assets_history_chart_container').style.display  = show_history ? 'block' : 'none';

    if (show_history) {
        renderAssetsHistoryChart(raw_assets_data);
    } else {
        renderAssetsSnapshotChart(raw_assets_data);
    }

    document.getElementById('assets_balances_legend_container').removeChild(document.getElementById('assets_balances_legend'));
    document.getElementById('assets_balances_legend_container').appendChild(renderAssetsBalancesLegend(raw_assets_data));
}

function renderTable(raw_data, ele, flipGoodBad=false) {
    let entries = [];
    let sources = Object.keys(raw_data).sort();
    for (let i = 0; i < sources.length; i ++) {
        let source = sources[i];
        let data   = raw_data[source];

        if (data.amount < 0.01) continue;

        entries.push({
            'source': source,
            'good':   (data.delta > 0) ? !flipGoodBad : flipGoodBad,
            'delta':  (data.delta >= 0.01) ? strAmount(data.delta, true) : '',
            'amount': strAmount(data.amount)
        });
    }

    ele.innerHTML = Mustache.render(TPL_SUMMARY_TABLE, { 'entry': entries });
}

function renderIncome(raw_income_data) {
    renderTable(raw_income_data, document.getElementById('income_table'));
}

function renderBudget(raw_budget_data) {
    renderTable(raw_budget_data, document.getElementById('budget_table'));
}

function renderExpenses(raw_expenses_data) {
    renderTable(raw_expenses_data, document.getElementById('expenses_table'), true);
}

function renderHistory(raw_history_data) {
    let entries = [];
    let totalDelta = 0;
    let days = Object.keys(raw_history_data).sort().reverse();
    for (let i = 0; i < days.length; i ++) {
        let day  = days[i];
        let data = raw_history_data[day];

        let transactions = [];
        for (let j = 0; j < data.length; j ++) {
            let transaction = data[j];

            if (zeroish(transaction.delta)) continue;

            transactions.push({
                'title': transaction.title,
                'good':  transaction.delta > 0,
                'delta': strAmount(transaction.delta, true),
            });

            totalDelta += transaction.delta;
        }

        if (transactions.length > 0) {
            entries.push({
                'day': day,
                'first': transactions[0],
                'rest': transactions.slice(1)
            });
        }
    }

    document.getElementById('history_table').innerHTML = Mustache.render(TPL_HISTORY_TABLE, { 'entry': entries });

    let total = document.getElementById('history_total');
    total.className = (totalDelta > 0) ? 'good' : 'bad';
    total.innerText = `${strAmount(totalDelta, true)}`;
}

function renderFinancesForLastMonth() {
    if (visible_month > 1) {
        renderFinancesFor(visible_month - 1);
    }
}

function renderFinancesForNextMonth() {
    if (visible_month < 12) {
        renderFinancesFor(visible_month + 1);
    }
}

function renderFinancesFor(month) {
    let httpRequest = new XMLHttpRequest();
    httpRequest.onreadystatechange = function() {
        if(httpRequest.readyState === XMLHttpRequest.DONE && httpRequest.status === 200) {
            let response = JSON.parse(httpRequest.responseText);
            renderFinances(month, response);
        }
    };
    httpRequest.open('GET', (month == THIS_MONTH) ? '/data' : `/data?month=${month}`);
    httpRequest.send();
}

function renderFinances(month, data) {
    visible_month = month;

    document.title = data.when;
    document.getElementById('when').innerText = data.when;

    document.getElementById('back').style.visibility = (month == 1)  ? 'hidden' : 'visible';
    document.getElementById('next').style.visibility = (month == 12) ? 'hidden' : 'visible';

    renderAssets(data.assets);
    renderIncome(data.income);
    renderBudget(data.budget);
    renderExpenses(data.expenses);
    renderHistory(data.history);
}

window.onload = () => {
    // Default options for charts
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

    // Fetch the data
    renderFinancesFor(THIS_MONTH);

    // Set up keybindings
    document.onkeyup = function(e) {
        if (e.key == 'ArrowLeft') {
            renderFinancesForLastMonth();
        } else if (e.key == 'ArrowRight') {
            renderFinancesForNextMonth();
        } else if (e.key == 'r') {
            renderFinancesFor(visible_month);
        } else if (e.key == 'h') {
            show_history = !show_history;
            renderAssets(cached_assets_data);
        }
    }
};


/*****************************************************************************
 * TEMPLATES
 *****************************************************************************/

// The legend tables.
const TPL_LEGEND_TABLE = `
{{#entry}}
  {{>show_entry}}
  {{#subentry}}
    {{>show_entry}}
  {{/subentry}}
{{/entry}}
`;

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

// The "income", "budget", and "expenses" summary tables.
const TPL_SUMMARY_TABLE = `
{{#entry}}
  <tr>
    <th>{{source}}</th>
    <td class="{{#good}}good{{/good}}{{^good}}bad{{/good}}">{{delta}}</td>
    <td class="right">{{amount}}</td>
  </tr>
{{/entry}}
`;

// The "history" table.
const TPL_HISTORY_TABLE = `
{{#entry}}
  <tr>
    <th>{{day}}</th>
    <td>{{first.title}}</td>
    <td class="{{#first.good}}good{{/first.good}}{{^first.good}}bad{{/first.good}}">{{first.delta}}</td>
  </tr>
  {{#rest}}
    <tr class="sub">
      <th></th>
      <td>{{title}}</td>
      <td class="{{#good}}good{{/good}}{{^good}}bad{{/good}}">{{delta}}</td>
    </tr>
  {{/rest}}
{{/entry}}
`;
