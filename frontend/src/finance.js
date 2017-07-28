const THIS_YEAR  = new Date().getFullYear();
const THIS_MONTH = new Date().getMonth() + 1;
const A_MONTH_AND_A_HALF_FROM_NOW =
      (THIS_MONTH < 12) ? new Date(THIS_YEAR, THIS_MONTH, 15) : new Date(THIS_YEAR, 12, 31);

var show = 'summary';
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
            dataLabels: { enabled: true },
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

function renderAssetsHistoricalChart(raw_assets_data) {
    let keys   = Object.keys(raw_assets_data);
    let axes   = [];
    let gap    = 5;
    let height = 100 * 1 / keys.length - gap;
    let series = {};

    for (let i = 0; i < keys.length; i ++) {
        let asset = raw_assets_data[keys[i]];

        axes.push({
            height: `${height}%`,
            top: `${(height + gap) * i}%`,
            offset: 0,
            min: 0
        });

        if (!(asset.name in series)) {
            for (let j = 0; j < asset.breakdown.length; j ++) {
                let account = asset.breakdown[j];
                series[`${asset.name} (${account.name})`] = {
                    asset: asset,
                    account: account,
                    colour: colour(`${asset.name} (${account.name})`),
                    data: [],
                    yAxis: axes.length-1
                };
            }
            if (asset.breakdown.length == 1 && asset.name == asset.breakdown[0].name) {
                series[asset.name] = {
                    asset: asset,
                    account: undefined,
                    colour: colour(asset.name),
                    data: {},
                    yAxis: axes.length-1
                };
            }
        }

        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];

            for (let j = 0; j < account.history.length; j ++) {
                let entry  = account.history[j];
                let date   = new Date(entry.date).getTime();
                let amount = entry.amount;

                if (asset.breakdown.length == 1 && asset.name == asset.breakdown[0].name) {
                    series[asset.name].data[date] = zeroish(amount) ? undefined : amount;
                } else {
                    series[`${asset.name} (${account.name})`].data[date] = zeroish(amount) ? undefined : amount;
                }
            }
        }
    }

    let chart = Highcharts.stockChart('historical_chart_container', {
        chart: { zoomType: 'x' },
        xAxis: { max: A_MONTH_AND_A_HALF_FROM_NOW.getTime() },
        yAxis: axes,
        rangeSelector: { selected: 1 },
        series: Object.keys(series).map(function(key) {
            // convert a datum to a point
            function toPoint(date) {
                return {
                    x: parseInt(date),
                    y: zeroish(series[key].data[date]) ? undefined : series[key].data[date],
                    asset: series[key].asset,
                    account: series[key].account
                };
            }

            // format a point for a tooltip
            function pointFormatter() {
                if (zeroish(this.y)) return '';

                let key = `<span style="color:${colour(this.asset.name)};font-weight:bold">${this.asset.name}</span>`;
                if (this.account != undefined) {
                    let c = colour(`${this.asset.name} (${this.account.name})`);
                    key = `${key} <span style="color:${c};font-weight:bold"> (${this.account.name})</span>`;
                }

                return `${key} ${strAmount(this.y)}<br/>`;
            }

            return {
                type: 'spline',
                name: key,
                data: Object.keys(series[key].data).map(toPoint),
                color: series[key].colour,
                tooltip: { pointFormatter: pointFormatter },
                yAxis: series[key].yAxis
            }
        })
    });

    // restore the old X positioning
    if (historical_chart_axes != undefined) {
        chart.xAxis[0].setExtremes(historical_chart_axes.userMin, historical_chart_axes.userMax);
        chart.showResetZoom();
    }
    historical_chart_axes = chart.xAxis[0];
}

function renderCashflowChart(income_data, expense_data) {
    function gather(raw_data) {
        let out     = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
        let sources = Object.keys(raw_data.accounts);
        for (let i = 0; i < sources.length; i ++) {
            let source = sources[i];
            let data   = raw_data.accounts[source];
            let last   = 0;
            for (let j = 0; j < data.history.length; j ++) {
                let m = new Date(Date.parse(data.history[j].date)).getMonth();
                let amount = Math.abs(data.history[j].amount);
                out[m] += amount - last;
                last = amount;
            }
        }
        return out;
    }

    let incomes      = gather(income_data);
    let expenditures = gather(expense_data);

    let balances = [];
    let cur      = 0;
    for (let i = 0; i < incomes.length; i ++) {
        cur = cur + incomes[i] - expenditures[i];
        balances.push(cur);
    }

    function pointFormatter() { return `<span style="color:${this.series.color}; font-weight:bold">${this.series.name}</span> ${strAmount(this.y)}<br/>`; }
    let greenColour = 'rgb(100,200,100)';
    let redColour   = 'rgb(250,100,100)';
    Highcharts.chart('cashflow_chart_container', {
        yAxis: { title: { text: '' } },
        xAxis: { categories: ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'] },
        tooltip: { shared: true },
        series: [{
            type: 'column',
            name: 'Income',
            color: greenColour,
            data: incomes,
            tooltip: { pointFormatter: pointFormatter }
        }, {
            type: 'column',
            name: 'Expenditure',
            color: redColour,
            data: expenditures,
            tooltip: { pointFormatter: pointFormatter }
        }, {
            type: 'spline',
            name: 'Total Change',
            data: balances,
            showPlus: true,
            color: 'rgb(100,100,250)',
            tooltip: {
                pointFormatter: function () {
                    let tag = `<span style="color:${this.series.color}; font-weight:bold">${this.series.name}</span>`;
                    let amount = strAmount(this.y, true);
                    let col = zeroish(this.y) ? 'black' : ((this.y < 0) ? redColour : greenColour);
                    return `${tag}  <span style="color:${col}">${amount}</span><br/>`;
                }
            }
        }]
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

    // allocation
    renderAllocationChart(data.assets, data.commodities);

    // balances or history chart
    document.getElementById('general_chart_container').style.display    = (show == 'summary' || show == 'historical') ? 'flex' : 'none';
    document.getElementById('cashflow_chart_container').style.display   = (show == 'cashflow')   ? 'block' : 'none';
    document.getElementById('allocation_chart_container').style.display       = (show == 'summary')    ? 'block' : 'none';
    document.getElementById('balances_chart_container').style.display   = (show == 'summary')    ? 'block' : 'none';
    document.getElementById('historical_chart_container').style.display = (show == 'historical') ? 'block' : 'none';

    if (show == 'historical') {
        renderAssetsHistoricalChart(data.assets);
    } else if (show == 'cashflow') {
        renderCashflowChart(data.income, data.expenses);
    } else {
        renderAssetsSnapshotChart(data.assets);
    }
}

function renderTable(raw_data, name, flipGoodBad=false) {
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
            'good':   (data.delta > 0) ? !flipGoodBad : flipGoodBad,
            'delta':  zeroish(data.delta) ? '' : strAmount(data.delta, true),
            'amount': strAmount(data.amount)
        });
        totalAmount += data.amount;
        totalDelta += data.delta;
    }

    document.getElementById(`cur_${name}_table`).innerHTML = Mustache.render(TPL_SUMMARY_TABLE, {
        entry: entries,
        foot: {
            good:   (totalDelta > 0) ? !flipGoodBad : flipGoodBad,
            delta:  zeroish(totalDelta) ? '' : strAmount(totalDelta, true),
            amount: strAmount(totalAmount)
        }
    });
    document.getElementById(`cur_${name}_prior_date`).innerText = raw_data.prior_date;
}

function renderIncome(raw_income_data) {
    renderTable(raw_income_data, 'income', true);
}

function renderBudget(raw_budget_data) {
    renderTable(raw_budget_data, 'budget');
}

function renderExpenses(raw_expenses_data) {
    renderTable(raw_expenses_data, 'expenses', true);
}

function renderFinances(month, data) {
    document.title = data.when;
    document.getElementById('when').innerText = data.when;

    document.getElementById('back').style.visibility = (month == 1)  ? 'hidden' : 'visible';
    document.getElementById('next').style.visibility = (month == 12) ? 'hidden' : 'visible';

    renderCharts(data);
    renderIncome(data.income);
    renderBudget(data.budget);
    renderExpenses(data.expenses);

    // this function comes from history.js
    renderHistory(data.history, 'history_table');
}

window.onload = () => {
    // Default options for charts
    setChartDefaults();

    // From sidebar.js
    renderSidebar();

    // Fetch the data
    renderFinancesFor(renderFinances);

    // Set up keybindings
    document.onkeyup = function(e) {
        if (e.key == 'ArrowLeft') {
            renderFinancesForLastMonth(renderFinances);
        } else if (e.key == 'ArrowRight') {
            renderFinancesForNextMonth(renderFinances);
        } else if (e.key == 'r') {
            renderCharts();
        } else if (e.key == 's' && show != 'summary') {
            show = 'summary';
            renderCharts();
        } else if (e.key == 'h' && show != 'historical') {
            show = 'historical';
            renderCharts();
        } else if (e.key == 'c' && show != 'cashflow') {
            show = 'cashflow';
            renderCharts();
        }
    }
};
