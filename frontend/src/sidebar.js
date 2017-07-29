// A cut-down version of the pie chart on the summary page.  It doesn't have the breakdown accounts.
function renderSidebarChart(data) {
    let assetData    = [];
    let assetTotals  = {};
    let overallTotal = 0;

    for (let key in data) {
        let asset = data[key];
        let assetAmount = asset.breakdown.reduce((acc, d) => acc + d.amount, 0);
        overallTotal += assetAmount;

        if(zeroish(assetAmount)) continue;

        assetData.push({
            name: asset.name,
            y: assetAmount,
            color: colour(asset.name)
        });
    }

    Highcharts.chart('sidebar_chart_container', {
        chart: { type: 'pie' },
        series: [{
            name: 'Accounts',
            data: assetData,
            size: '100%',
            tooltip: {
                pointFormatter: function() {
                    return `${strAmount(this.y)} (${(100*this.y/overallTotal).toFixed(2)}% of overall portfolio)<br/>`;
                }
            }
        }]
    });
}

// Sum an account report.
function sumAccountReport(raw_data, category) {
    let filtered_data = raw_data;
    if (category !== undefined) {
        filtered_data = [];
        for (let datum of raw_data) {
            filtered_data.push({
                name: datum.name,
                breakdown: datum.breakdown.filter(b => b.category == category)
            });
        }
    }

    let data = gatherFromAccountReport(filtered_data);
    return Object.values(data).reduce((acc, d) => acc + d.reduce((acc, a) => acc + a.amount, 0), 0);
}

// Sum a delta report.
function sumDeltaReport(data) {
    return Object.values(data.accounts).reduce((acc, d) => acc + d.amount, 0);
}

// Actually render the sidebar
function sidebarCallback(month, data) {
    let assets_current      = sumAccountReport(data.assets, "Current");
    let liabilities_current = Math.abs(sumAccountReport(data.liabilities, "Current"));
    let assets_total        = sumAccountReport(data.assets);
    let liabilities_total   = Math.abs(sumAccountReport(data.liabilities));
    let income_month        = Math.abs(sumDeltaReport(data.income));
    let expenditures_month  = sumDeltaReport(data.expenses);

    document.getElementById('sidebar').innerHTML = Mustache.render(TPL_SIDEBAR, {
        month:             MONTH_NAMES[month-1],
        assets:            strAmount(assets_current),
        liabilities:       strAmount(liabilities_current),
        net_worth:         strAmount(assets_current - liabilities_current),
        net_worth_good:    assets_current > liabilities_current,
        assets_total:      strAmount(assets_total),
        liabilities_total: strAmount(liabilities_total),
        net_worth_total:   strAmount(assets_total - liabilities_total),
        income:            strAmount(income_month),
        expenditures:      strAmount(expenditures_month),
        cashflow:          strAmount(income_month - expenditures_month),
        cashflow_good:     income_month > expenditures_month
    });

    renderSidebarChart(data.assets);
}

// Called in the window.onload of the appropriate page
function renderSidebar() {
    setChartDefaults();

    renderFinancesFor(sidebarCallback);
}

const TPL_SIDEBAR = `
<div id="sidebar_chart_container" style="height: 150px;"></div>
<nav>
  <ul>
    <li><a href="/">Summary</a></li>
    <li><a href="/balancesheet.html">Balance Sheet</a></li>
    <li><a href="/cashflow.html">Cashflow</a></li>
    <li><a href="/history.html">History</a></li>
  </ul>
</nav>
<hr>
<ul class="kv">
  <li><span class="key">{{month}} Income</span>       <span>{{income}}</span></li>
  <li><span class="key">{{month}} Expenditures</span> <span>{{expenditures}}</span></li>
  <li><span class="key">Cashflow</span>               <span class="{{#cashflow_good}}good{{/cashflow_good}}{{^cashflow_good}}bad{{/cashflow_good}}">{{cashflow}}</span></li>
</ul>
<hr>
<ul class="kv">
  <li><span class="key">Current Assets</span>      <span title="Total: {{assets_total}}">{{assets}}</span></li>
  <li><span class="key">Current Liabilities</span> <span title="Total: {{liabilities_total}}">{{liabilities}}</span></li>
  <li><span class="key">Net Worth</span>           <span title="Total: {{net_worth_total}}" class="{{#net_worth_good}}good{{/net_worth_good}}{{^net_worth_good}}bad{{/net_worth_good}}">{{net_worth}}</span></li>
</ul>
`;
