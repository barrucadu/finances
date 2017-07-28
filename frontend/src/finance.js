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

function renderAllocationChart(assets_data, commodities_data) {
    let overallTotal = 0;

    let commodityWorthTotals  = {};
    let commodityAmountTotals = {};
    for (let akey in assets_data) {
        let asset = assets_data[akey];
        let assetAmount = asset.breakdown.reduce((acc, d) => acc + d.amount, 0);
        overallTotal += assetAmount;

        for (let account of asset.breakdown) {
            for (let ckey in account.commodities) {
                let commodity = account.commodities[ckey];

                if (!(ckey in commodityWorthTotals)) {
                    commodityWorthTotals[ckey]  = 0;
                    commodityAmountTotals[ckey] = 0;
                }

                commodityWorthTotals[ckey]  += commodity.worth;
                commodityAmountTotals[ckey] += commodity.amount;
            }
        }
    }

    let allocationTotals = { 'Cash': 0 };
    let allocationData   = [];
    let commodityData    = [];
    for (let ckey in commodityWorthTotals) {
        let commodityWorth = commodityWorthTotals[ckey];
        if (ckey in commodities_data) {
            let totalShare = Object.values(commodities_data[ckey].allocation).reduce((acc, s) => acc + s, 0);
            for (let akey in commodities_data[ckey].allocation) {
                let share = commodities_data[ckey].allocation[akey];
                if (!(akey in allocationTotals)) {
                    allocationTotals[akey] = 0;
                }
                allocationTotals[akey] += commodityWorth * share / totalShare;
            }
        } else {
            allocationTotals['Cash'] += commodityWorth;
        }
    }
    for (let akey in allocationTotals) {
        let amount = allocationTotals[akey];

        if (zeroish(amount)) continue;

        allocationData.push({
            name: akey,
            y: amount,
            color: colour(akey),
            visible: !(akey in hidden_allocations)
        });

        for (let ckey in commodityWorthTotals) {
            let commodityWorth  = commodityWorthTotals[ckey];
            let commodityAmount = commodityAmountTotals[ckey];

            if (ckey in commodities_data) {
                let totalShare = Object.values(commodities_data[ckey].allocation).reduce((acc, s) => acc + s, 0);
                if (akey in commodities_data[ckey].allocation) {
                    let share = commodities_data[ckey].allocation[akey];
                    commodityData.push({
                        name: (ckey in commodities_data && 'name' in commodities_data[ckey]) ? commodities_data[ckey].name : ckey,
                        y: commodityWorth * share / totalShare,
                        color: colour(ckey),
                        ckey: ckey,
                        amount: commodityAmount * share / totalShare,
                        owner: akey,
                        visible: !(akey in hidden_allocations)
                    });
                }
            } else if (akey == 'Cash') {
                commodityData.push({
                    name: (ckey in commodities_data && 'name' in commodities_data[ckey]) ? commodities_data[ckey].name : ckey,
                    y: commodityWorth,
                    color: colour(ckey),
                    ckey: ckey,
                    amount: commodityAmount,
                    owner: 'Cash',
                    visible: !('Cash' in hidden_allocations)
                });
            }
        }
    }

    Highcharts.chart('allocation_chart_container', {
        chart: { type: 'pie' },
        series: [{
            name: 'Allocation',
            data: allocationData,
            size: '60%',
            dataLabels: { enabled: false },
            showInLegend: true,
            tooltip: {
                pointFormatter: function() {
                    return `${strAmount(this.y)} (${(100*this.y/overallTotal).toFixed(2)}% of overall allocation)<br/>`;
                }
            },
            point: {
                events: { legendItemClick: legendItemClick(hidden_allocations) }
            }
        },{
            name: 'Commodities',
            data: commodityData,
            size: '80%',
            innerSize: '60%',
            dataLabels: { enabled: true },
            tooltip: {
                pointFormatter: function() {
                    let naked = strAmount(this.amount, false, false);
                    let worth = (this.ckey == '£') ? '' : `(worth ${strAmount(this.y)})`;
                    let fraction = (100 * this.y / allocationTotals[this.owner]).toFixed(2);
                    if (this.ckey.match(/^[0-9a-zA-Z]+$/)) {
                        return `${naked} ${this.ckey} ${worth} (${fraction}% of ${this.owner})<br/>`;
                    } else {
                        return `${this.ckey}${naked} ${worth} (${fraction}% of ${this.owner})<br/>`;
                    }
                }
            }
        }]
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

function renderAssetsSnapshotChart(raw_assets_data) {
    let assetData    = [];
    let accountData  = [];
    let assetTotals  = {};
    let overallTotal = 0;

    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        let assetAmount = asset.breakdown.reduce((acc, d) => acc + d.amount, 0);
        overallTotal += assetAmount;

        if(zeroish(assetAmount)) continue;

        assetData.push({
            name: asset.name,
            y: assetAmount,
            color: colour(asset.name),
            visible: !(asset.name in hidden_assets)
        });

        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];
            let amount = account.amount;

            if (zeroish(amount)) continue;

            accountData.push({
                name: account.name,
                y: amount,
                color: colour((account.name == asset.name) ? asset.name : `${asset.name} (${account.name})`),
                asset: {name: asset.name, amount: assetAmount},
                owner: asset.name,
                visible: !(asset.name in hidden_assets)
            });
        }
    }

    Highcharts.chart('balances_chart_container', {
        chart: { type: 'pie' },
        series: [{
            name: 'Accounts',
            data: assetData,
            size: '60%',
            dataLabels: { enabled: false },
            showInLegend: true,
            tooltip: {
                pointFormatter: function() {
                    return `${strAmount(this.y)} (${(100*this.y/overallTotal).toFixed(2)}% of overall portfolio)<br/>`;
                }
            },
            point: {
                events: { legendItemClick: legendItemClick(hidden_assets) }
            }
        }, {
            name: 'Breakdown',
            data: accountData,
            size: '80%',
            innerSize: '60%',
            id: 'versions',
            dataLabels: { enabled: true },
            tooltip: {
                pointFormatter: function() {
                    return `${strAmount(this.y)} (${(100*this.y/this.asset.amount).toFixed(2)}% of ${this.asset.name})<br/>`;
                }
            }
        }]
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
