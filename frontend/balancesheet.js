// Render one component of the balance sheet.
function renderComponent(name, data) {
    let total = Object.values(data).reduce((acc, d) => acc + d.reduce((acc, a) => acc + a.amount, 0), 0);

    document.getElementById(`bs_${name}_tbody`).innerHTML = Mustache.render(TPL_BALANCE_TABLE_BODY, {
        'category': Object.keys(data).sort().map(k => {
            let entries = [];
            for (let i = 0; i < data[k].length; i ++) {
                entries.push({ name: data[k][i].name, amount: strAmount(data[k][i].amount) });
            }
            return { title: k, entry: entries };
        })
    });
    document.getElementById(`bs_${name}_tfoot`).innerHTML = Mustache.render(TPL_BALANCE_TABLE_FOOT, {
        caption: 'Total',
        value: strAmount(total)
    });

    return total;
}

// Gather balance data from an account report.
function gatherFromAccountReport(raw_data, date) {
    let out = {};
    for (let key in raw_data) {
        let datum = raw_data[key];
        for (let i = 0; i < datum.breakdown.length; i ++) {
            let account = datum.breakdown[i];
            let amount = summariseHistory(account.history, date);
            if (amount == 0) continue;
            if (!(account.category in out)) {
                out[account.category] = [];
            }
            out[account.category].push({ name: account.name, amount: amount });
        }
    }
    return out;
}

// Gather balance data from a basic report.
function gatherFromBasicReport(raw_data, date, k='End of Period') {
    let out = {};
    out[k] = [];
    let keys = Object.keys(raw_data).sort();
    for (let i = 0; i < keys.length; i ++) {
        let key = keys[i];
        let history = raw_data[key];
        out[k].push({ name: key, amount: summariseHistory(history, date) });
    }
    return out;
}

// Render the balance sheet
function renderBalanceSheet(raw_data, date) {
    let assets_data      = gatherFromAccountReport(raw_data.assets, date);
    let liabilities_data = gatherFromAccountReport(raw_data.liabilities, date);

    let expenses_data = gatherFromBasicReport(raw_data.expenses, date);
    let income_data   = gatherFromBasicReport(raw_data.income, date);
    let equity_data   = gatherFromBasicReport(raw_data.equity, date, 'Start of Period');

    let assets_total      = renderComponent('assets',      assets_data);
    let equity_total      = renderComponent('equity',      equity_data,   'Start of Period');
    let expenses_total    = renderComponent('expenses',    expenses_data, 'End of Period');
    let income_total      = renderComponent('income',      income_data,   'End of Period');
    let liabilities_total = renderComponent('liabilities', liabilities_data);

    document.getElementById(`bs_total_tbody`).innerHTML = Mustache.render(TPL_BALANCE_TABLE_BODY, {
        category: [{
            title: 'Balance',
            entry: [ { name: 'Assets',      amount: strAmount(assets_total) },
                     { name: 'Equity',      amount: strAmount(equity_total) },
                     { name: 'Expenses',    amount: strAmount(expenses_total) },
                     { name: 'Income',      amount: strAmount(income_total) },
                     { name: 'Liabilities', amount: strAmount(liabilities_total) }]
        }],
    });
    document.getElementById(`bs_total_tfoot`).innerHTML = Mustache.render(TPL_BALANCE_TABLE_FOOT, {
        caption: 'Overall Total',
        value: strAmount(assets_total + equity_total + expenses_total + income_total + liabilities_total)
    });
}

function renderFinances(data, month) {
    let this_year = new Date().getFullYear();
    let date = new Date(this_year, month, daysInMonth(this_year, month));

    renderBalanceSheet(data, date);
}

window.onload = () => {
    // Set up the month picker
    let visible_month = undefined;
    let cached_account_data = undefined;
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
    });
};
