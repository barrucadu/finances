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
                point.update({ visible: !(this.name in hider) });
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

function renderAllocationChart(assets_data, commodities_data, date) {
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

                commodityWorthTotals[ckey]  += summariseHistory(commodity.worth, date);
                commodityAmountTotals[ckey] += summariseHistory(commodity.amount, date);
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

function renderAssetsSnapshotChart(raw_data, date) {
    let data = {};

    for (let akey in raw_data) {
        let asset = raw_data[akey];
        data[asset.name] = {};
        for (let breakdown of asset.breakdown) {
            data[asset.name][breakdown.name] = { worth: summariseHistory(breakdown.history, date) };
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

function renderIncomeExpensesTable(raw_data, name, date) {
    let startDate = new Date(date.getFullYear(), date.getMonth(), 1);
    let priorDate = new Date(date.getFullYear(), date.getMonth(), 0);
    let priorStartDate = new Date(priorDate.getFullYear(), priorDate.getMonth(), 1);

    let entries = [];
    let sources = Object.keys(raw_data).sort();
    let totalAmount = 0;
    let totalDelta  = 0;
    for (let i = 0; i < sources.length; i ++) {
        let source  = sources[i];
        let history = raw_data[source];
        let amount  = summariseHistory(history, date)      - summariseHistory(history, startDate);
        let prior   = summariseHistory(history, priorDate) - summariseHistory(history, priorStartDate);
        let delta   = amount - prior;

        if (zeroish(amount)) continue;

        entries.push({
            'source': source,
            'good': (delta > -0.01) ? false : true,
            'bad':  (delta <  0.01) ? false : true,
            'delta': zeroish(delta) ? '' : strAmount(delta, true),
            'amount': strAmount(amount)
        });
        totalAmount += amount;
        totalDelta += delta;
    }

    document.getElementById(`cur_${name}_tbody`).innerHTML = Mustache.render(TPL_SUMMARY_TABLE_BODY, {
        entry: entries
    });
    document.getElementById(`cur_${name}_tfoot`).innerHTML = Mustache.render(TPL_SUMMARY_TABLE_FOOT, {
        good: (totalDelta > -0.01) ? false : true,
        bad:  (totalDelta <  0.01) ? false : true,
        delta: (zeroish(totalDelta)) ? '' : strAmount(totalDelta, true),
        amount: strAmount(totalAmount)
    });
    if (true) {
        document.getElementById(`cur_${name}_prior_date`).innerText = strDate(priorDate);
    }
}

function renderIncome(raw_income_data, date) {
    renderIncomeExpensesTable(raw_income_data, 'income', date);
}

function renderExpenses(raw_expenses_data, date) {
    renderIncomeExpensesTable(raw_expenses_data, 'expenses', date);
}

function renderBudget(raw_data, date) {
    let entries = [];
    let sources = Object.keys(raw_data).sort();
    let totalAmount = 0;
    for (let i = 0; i < sources.length; i ++) {
        let source  = sources[i];
        let history = raw_data[source];
        let amount  = summariseHistory(history, date);

        if (zeroish(amount)) continue;

        entries.push({
            'source': source,
            'abad':   amount < 0,
            'amount': strAmount(amount)
        });
        totalAmount += amount;
    }

    document.getElementById(`cur_budget_tbody`).innerHTML = Mustache.render(TPL_SUMMARY_TABLE_BODY, {
        entry: entries
    });
    document.getElementById(`cur_budget_tfoot`).innerHTML = Mustache.render(TPL_SUMMARY_TABLE_FOOT, {
        abad:   totalAmount < 0,
        amount: strAmount(totalAmount)
    });
}

function renderFinances(data, month) {
    let this_year = new Date().getFullYear();
    let date = new Date(this_year, month, daysInMonth(this_year, month));

    renderAllocationChart(data.assets, data.commodities, date);
    renderAssetsSnapshotChart(data.assets, date);
    renderIncome(data.income, date);
    renderBudget(data.budget, date);
    renderExpenses(data.expenses, date);
}

function renderHistory(data, month) {
    let this_year = new Date().getFullYear();

    renderHistoryFor(getMonthlyTxns(data, this_year, month+1), 'recent');
}

window.onload = () => {
    // Set up the month picker
    let visible_month = undefined;
    let cached_account_data = undefined;
    let cached_history_data = undefined;
    monthpicker(i => {
        if (i == visible_month) return;
        visible_month = i;
        document.getElementById('month-name').innerText = MONTH_NAMES[i];

        if (cached_account_data == undefined) {
            ajax('/data', account_data => {
                cached_account_data = account_data;
                renderFinances(account_data, i);
            });
        } else {
            renderFinances(cached_account_data, visible_month);
        }

        if (cached_history_data == undefined) {
            ajax('/history', history_data => {
                cached_history_data = history_data;
                renderHistory(history_data, i);
            });
        } else {
            renderHistory(cached_history_data, visible_month);
        }
    });
};
