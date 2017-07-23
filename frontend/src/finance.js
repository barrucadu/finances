const THIS_YEAR  = new Date().getFullYear();
const THIS_MONTH = new Date().getMonth() + 1;
const A_MONTH_AND_A_HALF_FROM_NOW =
      (THIS_MONTH < 12) ? new Date(THIS_YEAR, THIS_MONTH, 15) : new Date(THIS_YEAR, 12, 31);

var show = 'summary';
var hidden_accounts = {};
var historical_chart_axes = undefined;

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

    renderCharts();
}

function colour(str) {
    let hash = 0;
    for (let i = 0; i < str.length; i ++) {
        hash = ((hash << 5) - hash) + str.charCodeAt(i);
        hash |= 0;
    }
    hash = Math.abs(hash);

    return `rgb(${(hash * 37) % 255}, ${(hash * 131) % 255}, ${(hash * 239) % 255})`
}

function zeroish(val) {
    return val < 0.01 && val > -0.01;
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

    Highcharts.chart('tags_chart_container', {
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

    let legend = document.getElementById('tags_legend');
    legend.innerHTML = Mustache.render(
        TPL_LEGEND_TABLE,
        { 'entry': legendEntries },
        { 'show_entry': TPL_PART_SHOW_TAG }
    );
}

function renderAssetsHistoricalChart(raw_assets_data) {
    let keys   = Object.keys(raw_assets_data).filter(k => !isHidden(raw_assets_data[k]));
    let axes   = [];
    let gap    = 5;
    let height = 100 * 1 / keys.length - gap;
    let series = {};

    for (let i = 0; i < keys.length; i ++) {
        let asset = raw_assets_data[keys[i]];

        if(isHidden(asset)) continue;

        axes.push({
            height: `${height}%`,
            top: `${(height + gap) * i}%`,
            offset: 0,
            min: 0
        });

        if (!(asset.name in series)) {
            for (let j = 0; j < asset.breakdown.length; j ++) {
                let account = asset.breakdown[j];
                if (isHidden(asset, account)) continue;
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
            if (isHidden(asset, account)) continue;

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

    Highcharts.chart('balances_chart_container', {
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

function renderAssetsBalancesLegend(raw_assets_data, show_all=false) {
    let overalltotal = raw_assets_data.reduce((acc, ass) => acc + ass.breakdown.reduce((acc2, d) => acc2 + d.amount, 0), 0);

    let entries = [];
    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        let total = asset.breakdown.reduce((acc, d) => acc + d.amount, 0)

        let subentries = [];
        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];

            if (zeroish(account.amount) && !show_all) continue;

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

        if (!zeroish(total) || show_all) {
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

    let legend = document.getElementById('balances_legend');
    legend.innerHTML = Mustache.render(
        TPL_LEGEND_TABLE,
        { 'entry': entries },
        { 'show_entry': TPL_PART_SHOW_ACCOUNT }
    );
}

function renderCharts(data) {
    if (data == undefined) {
        data = cached_data;
    }

    // tags
    renderAssetsTagsChartAndLegend(data.assets);

    // balances or history chart
    document.getElementById('general_chart_container').style.display    = (show == 'summary' || show == 'historical') ? 'flex' : 'none';
    document.getElementById('cashflow_chart_container').style.display   = (show == 'cashflow')   ? 'block' : 'none';
    document.getElementById('tags_legend_container').style.display      = (show == 'summary')    ? 'flex'  : 'none';
    document.getElementById('tags_chart_container').style.display       = (show == 'summary')    ? 'block' : 'none';
    document.getElementById('balances_chart_container').style.display   = (show == 'summary')    ? 'block' : 'none';
    document.getElementById('historical_chart_container').style.display = (show == 'historical') ? 'block' : 'none';

    if (show == 'historical') {
        renderAssetsHistoricalChart(data.assets);
        renderAssetsBalancesLegend(data.assets, true);
    } else if (show == 'cashflow') {
        renderCashflowChart(data.income, data.expenses);
    } else {
        renderAssetsSnapshotChart(data.assets);
        renderAssetsBalancesLegend(data.assets, false);
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

    document.getElementById('history_table').innerHTML = Mustache.render(TPL_HISTORY_TABLE, {
        entry: entries,
        foot: {
            delta: strAmount(totalDelta, true),
            good: totalDelta > 0
        }
    });
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
