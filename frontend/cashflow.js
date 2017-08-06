// Common configuration for column tooltips.  For some reason this doesn't work when set as a global default.
const columnTooltip = {
    shared: true,
    useHTML: true,
    headerFormat: '<span style="font-size:10px">{point.key}</span><table class="tooltip">',
    footerFormat: '</table>',
    pointFormatter: function ()
    {
        return `<tr><td style="color:${this.series.color}; font-weight:bold">${this.series.name}</td><td class="right">${strAmount(this.y)}</td></tr>`;
    }
};

// Condense an account history into 12 monthly totals
function monthise(history_data) {
    let out = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    let last = 0;
    for (let entry of history_data) {
        let m = new Date(Date.parse(entry.date)).getMonth();
        let amount = Math.abs(entry.amount);
        out[m] += amount - last;
        last = amount;
    }

    return out;
}

function renderCashflowChart(income_data, expense_data) {
    function gather(raw_data) {
        let out = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
        for (let history of Object.values(raw_data)) {
            let summary = monthise(history);
            for (let j = 0; j < 12; j ++) {
                out[j] += summary[j];
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

    let greenColour = 'rgb(100,200,100)';
    let redColour   = 'rgb(250,100,100)';
    Highcharts.chart('cashflow_chart_container', {
        chart: { type: 'column' },
        xAxis: { categories: MONTH_NAMES, crosshair: true },
        yAxis: { title: { text: '' }, labels: { formatter: function () { return strAmount(this.value); } } },
        tooltip: columnTooltip,
        series: [{
            name: 'Income',
            color: greenColour,
            data: incomes
        }, {
            name: 'Expenditure',
            color: redColour,
            data: expenditures
        }, {
            type: 'spline',
            name: 'Cumulative Change',
            data: balances,
            showPlus: true,
            color: 'rgb(100,100,250)',
            tooltip: {
                pointFormatter: function () {
                    let tag = `<td style="color:${this.series.color}; font-weight:bold">${this.series.name}</td>`;
                    let amount = strAmount(this.y, true);
                    let col = zeroish(this.y) ? 'black' : ((this.y < 0) ? redColour : greenColour);
                    return `<tr>${tag}<td style="color:${col}" class="right">${amount}</td></tr>`;
                }
            }
        }]
    });
}

function renderBreakdownChart(ele_id, raw_data) {
    Highcharts.chart(ele_id, {
        chart: { type: 'column' },
        xAxis: { categories: MONTH_NAMES, crosshair: true },
        yAxis: { title: { text: '' }, min: 0, labels: { formatter: function () { return strAmount(this.value); } } },
        tooltip: columnTooltip,
        series: Object.keys(raw_data).sort().map(k => {
            return {
                name: k,
                color: colour(k),
                data: monthise(raw_data[k]).map(x => zeroish(x) ? NaN : x)
            };
        })
    });
}

function renderCharts(income_data, expense_data) {
    if (income_data  == undefined) { income_data  = cached_data.income; }
    if (expense_data == undefined) { expense_data = cached_data.expenses; }

    renderCashflowChart(income_data, expense_data);
    renderBreakdownChart('expenses_breakdown_chart_container', expense_data);
    renderBreakdownChart('income_breakdown_chart_container',   income_data);
}

window.onload = () => ajax('/data', account_data => renderCharts(account_data.income, account_data.expenses));
