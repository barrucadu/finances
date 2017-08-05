const THIS_YEAR  = new Date().getFullYear();
const THIS_MONTH = new Date().getMonth() + 1;
const A_MONTH_AND_A_HALF_FROM_NOW =
      (THIS_MONTH < 12) ? new Date(THIS_YEAR, THIS_MONTH, 15) : new Date(THIS_YEAR, 12, 31);

var historical_chart_axes = undefined;

var hidden_assets = {};
var hidden_allocations = {};

// Toggle points in a pie chart
function legendItemClick(hider) {
    return function() {
        if (this.name in hider) {
            delete hider[this.name];
        } else {
            hider[this.name] = true;
        }

        for (let point of this.series.chart.series[1].points) {
            if (point.owner == this.name) {
                if (this.name in hider) {
                    point.update({visible: false, name: '', originalName: point.name });
                } else {
                    point.update({visible: true, name: point.originalName });
                }
            }
        }
    }
}

// Render a two-ring pie chart where the outer doughnut is a breakdown
// of the inner.
function renderBreakdownPie(ele_id, hider, raw_data, category_series_name, category_tooltip, breakdown_series_name, breakdown_tooltip) {
    let categoryData   = [];
    let breakdownData  = [];
    let categoryTotals = {};
    let overallTotal   = 0;

    for (let ckey in raw_data) {
        let category      = raw_data[ckey];
        let categoryTotal = Object.values(category).reduce((acc, b) => acc + b.worth, 0);
        overallTotal += categoryTotal;

        if(zeroish(categoryTotal)) continue;

        categoryData.push({
            name: ckey,
            y: categoryTotal,
            color: colour(ckey),
            visible: !(ckey in hider)
        });

        for (let bkey in category) {
            let breakdown = category[bkey]

            if (zeroish(breakdown.worth)) continue;

            breakdown.name       = bkey;
            breakdown.y          = breakdown.worth;
            breakdown.color      = colour(bkey);
            breakdown.owner      = ckey;
            breakdown.ownerTotal = categoryTotal;
            breakdown.visible    = !(ckey in hider);

            breakdownData.push(breakdown);
        }
    }

    Highcharts.chart(ele_id, {
        chart: { type: 'pie' },
        series: [{
            name: category_series_name,
            data: categoryData,
            size: '60%',
            dataLabels: { enabled: false },
            showInLegend: true,
            tooltip: { pointFormatter: category_tooltip(overallTotal) },
            point: { events: { legendItemClick: legendItemClick(hider) } }
        },{
            name: breakdown_series_name,
            data: breakdownData,
            size: '80%',
            innerSize: '60%',
            dataLabels: {
                enabled: true,
                formatter: function() { return (this.percentage >= 0.5) ? this.point.name : null; }
            },
            tooltip: { pointFormatter: breakdown_tooltip }
        }]
    });
}

function renderAllocationChart(assets_data, commodities_data) {
    let commodityWorthTotals  = {};
    let commodityAmountTotals = {};
    for (let asset of Object.values(assets_data)) {
        for (let breakdown of asset.breakdown) {
            for (let ckey in breakdown.commodities) {
                let commodity = breakdown.commodities[ckey];

                if (!(ckey in commodityWorthTotals)) {
                    commodityWorthTotals[ckey]  = 0;
                    commodityAmountTotals[ckey] = 0;
                }

                commodityWorthTotals[ckey]  += commodity.worth;
                commodityAmountTotals[ckey] += commodity.amount;
            }
        }
    }

    let allocations = { 'Cash': true }
    for (let commodity of Object.values(commodities_data)) {
        for (let akey in commodity.allocation) {
            allocations[akey] = true;
        }
    }

    let data = {};
    for (let akey in allocations) {
        data[akey] = {};
        for (let ckey in commodityWorthTotals) {
            function push (portion) {
                let name = (ckey in commodities_data && 'name' in commodities_data[ckey]) ? commodities_data[ckey].name : ckey;
                data[akey][name] = {
                    worth: commodityWorthTotals[ckey] * portion,
                    amount: commodityAmountTotals[ckey] * portion,
                    ckey: ckey
                };
            }

            if (ckey in commodities_data) {
                let totalShare = Object.values(commodities_data[ckey].allocation).reduce((acc, s) => acc + s, 0);
                if (akey in commodities_data[ckey].allocation) {
                    let share = commodities_data[ckey].allocation[akey];
                    push(share / totalShare);
                }
            } else if (akey == 'Cash') {
                push(1);
            }
        }
    }

    renderBreakdownPie(
        'allocation_chart_container', hidden_allocations, data,
        'Allocation', overallTotal => function () {
            return `${strAmount(this.y)} (${(100*this.y/overallTotal).toFixed(2)}% of overall allocation)<br/>`;
        },
        'Commodities', function () {
            let naked = strAmount(this.amount, false, false);
            let worth = (this.ckey == '£') ? '' : `(worth ${strAmount(this.y)})`;
            let fraction = (100 * this.worth / this.ownerTotal).toFixed(2);
            if (this.ckey.match(/^[0-9a-zA-Z]+$/)) {
                return `${naked} ${this.ckey} ${worth} (${fraction}% of ${this.owner})<br/>`;
            } else {
                return `${this.ckey}${naked} ${worth} (${fraction}% of ${this.owner})<br/>`;
            }
        });
}

function renderAssetsSnapshotChart(raw_data) {
    let data = {};

    for (let akey in raw_data) {
        let asset = raw_data[akey];
        data[asset.name] = {};
        for (let breakdown of asset.breakdown) {
            data[asset.name][breakdown.name] = { worth: breakdown.amount };
        }
    }

    renderBreakdownPie(
        'balances_chart_container', hidden_assets, data,
        'Accounts', overallTotal => function () {
            return `${strAmount(this.y)} (${(100*this.y/overallTotal).toFixed(2)}% of overall portfolio)<br/>`;
        },
        'Breakdown', function () {
            return `${strAmount(this.y)} (${(100*this.y/this.ownerTotal).toFixed(2)}% of ${this.owner})<br/>`;
        });
}

function renderCharts(data) {
    if (data == undefined) {
        data = cached_data;
    }

    renderAllocationChart(data.assets, data.commodities);
    renderAssetsSnapshotChart(data.assets);
}

function renderTable(raw_data, name, opts={}) {
    let flipGoodBad = (opts.flipGoodBad == undefined) ? true  : opts.flipGoodBad;
    let showDelta   = (opts.showDelta   == undefined) ? true  : opts.showDelta;
    let showABad    = (opts.showABad    == undefined) ? false : opts.showABad;

    let entries = [];
    let sources = Object.keys(raw_data.accounts).sort();
    let totalAmount = 0;
    let totalDelta  = 0;
    for (let i = 0; i < sources.length; i ++) {
        let source = sources[i];
        let data   = raw_data.accounts[source];

        if (zeroish(data.amount)) continue;

        entries.push({
            'source': source,
            'good': (data.delta == 0) ? false : ((data.delta > 0) ? !flipGoodBad : flipGoodBad),
            'bad':  (data.delta == 0) ? false : ((data.delta < 0) ? !flipGoodBad : flipGoodBad),
            'abad':  (data.amount == 0    || !showABad)  ? false : ((data.amount < 0) ? !flipGoodBad : flipGoodBad),
            'delta': (zeroish(data.delta) || !showDelta) ? ''    : strAmount(data.delta, true),
            'amount': strAmount(data.amount)
        });
        totalAmount += data.amount;
        totalDelta += data.delta;
    }

    document.getElementById(`cur_${name}_tbody`).innerHTML = Mustache.render(TPL_SUMMARY_TABLE_BODY, {
        entry: entries
    });
    document.getElementById(`cur_${name}_tfoot`).innerHTML = Mustache.render(TPL_SUMMARY_TABLE_FOOT, {
        good: (totalDelta == 0) ? false : ((totalDelta > 0) ? !flipGoodBad : flipGoodBad),
        bad:  (totalDelta == 0) ? false : ((totalDelta < 0) ? !flipGoodBad : flipGoodBad),
        abad:  (totalAmount == 0    || !showABad)  ? false : ((totalAmount < 0) ? !flipGoodBad : flipGoodBad),
        delta: (zeroish(totalDelta) || !showDelta) ? ''    : strAmount(totalDelta, true),
        amount: strAmount(totalAmount)
    });
    if (showDelta) {
        document.getElementById(`cur_${name}_prior_date`).innerText = raw_data.prior_date;
    }
}

function renderIncome(raw_income_data) {
    renderTable(raw_income_data, 'income');
}

function renderBudget(raw_budget_data) {
    renderTable(raw_budget_data, 'budget', { flipGoodBad: false, showDelta: false, showABad: true });
}

function renderExpenses(raw_expenses_data) {
    renderTable(raw_expenses_data, 'expenses');
}

function renderFinances(month, data) {
    renderCharts(data);
    renderIncome(data.income);
    renderBudget(data.budget);
    renderExpenses(data.expenses);

    // from history.js
    renderHistoryFor(data.history, 'recent');
}

window.onload = () => {
    // Default options for charts
    setChartDefaults();

    // Render the navbar
    navbar('summary');

    // Set up the month picker
    let visible_month = -1;
    monthpicker(i => {
        if (i == visible_month) return;
        visible_month = i;
        document.getElementById('month-name').innerText = MONTH_NAMES[i];
        renderFinancesFor(renderFinances, i);
    });
};
