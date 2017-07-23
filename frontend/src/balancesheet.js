// Render one component of the balance sheet.
function renderComponent(name, data) {
    let table_ele = document.getElementById(`bs_${name}_table`);
    let total = Object.values(data).reduce((acc, d) => acc + d.reduce((acc, a) => acc + a.amount, 0), 0);

    table_ele.innerHTML = Mustache.render(TPL_BALANCE_TABLE, {
        'category': Object.keys(data).sort().map(k => {
            let entries = [];
            for (let i = 0; i < data[k].length; i ++) {
                entries.push({ name: data[k][i].name, amount: strAmount(data[k][i].amount) });
            }
            return { title: k, entry: entries };
        }),
        foot: {
            caption: 'Total',
            value: strAmount(total)
        }
    });

    return total;
}

// Gather balance data from an account report.
function gatherFromAccountReport(raw_data) {
    let out = {};
    for (let key in raw_data) {
        let datum = raw_data[key];
        for (let i = 0; i < datum.breakdown.length; i ++) {
            let account = datum.breakdown[i];
            if (account.amount == 0) continue;
            if (!(account.balance_tag in out)) {
                out[account.balance_tag] = [];
            }
            out[account.balance_tag].push({ name: account.name, amount: account.amount });
        }
    }
    return out;
}

// Gather balance data from a delta report.
function gatherFromDeltaReport(date, raw_data) {
    let out = [];
    let keys = Object.keys(raw_data).sort();
    for (let i = 0; i < keys.length; i ++) {
        let key = keys[i];
        let datum = raw_data[key];
        let amount = 0;
        for (let j = 0; j < datum.history.length; j ++) {
            if (Date.parse(datum.history[j].date) <= date.getTime()) {
                amount = datum.history[j].amount;
            } else {
                break;
            }
        }
        out.push({ name: key, amount: amount });
    }
    return { 'End of Period': out };
}

// Render the balance sheet
function renderBalanceSheet(raw_data) {
    let assets_data      = gatherFromAccountReport(raw_data.assets);
    let liabilities_data = gatherFromAccountReport(raw_data.liabilities);

    let date = new Date(Date.parse(raw_data.date));
    let expenses_data = gatherFromDeltaReport(date, raw_data.expenses.accounts);
    let income_data   = gatherFromDeltaReport(date, raw_data.income.accounts);

    let equity_data = [];
    let keys = Object.keys(raw_data.equity).sort();
    for (let i = 0; i < keys.length; i ++) {
        let key = keys[i];
        equity_data.push({ name: key, amount: raw_data.equity[key] });
    }
    equity_data = { 'Start of Period': equity_data };

    let assets_total      = renderComponent('assets',      assets_data);
    let equity_total      = renderComponent('equity',      equity_data,   'Start of Period');
    let expenses_total    = renderComponent('expenses',    expenses_data, 'End of Period');
    let income_total      = renderComponent('income',      income_data,   'End of Period');
    let liabilities_total = renderComponent('liabilities', liabilities_data);

    document.getElementById(`bs_total_table`).innerHTML = Mustache.render(TPL_BALANCE_TABLE, {
        category: [{
            title: 'Balance',
            entry: [ { name: 'Assets',      amount: strAmount(assets_total) },
                     { name: 'Equity',      amount: strAmount(equity_total) },
                     { name: 'Expenses',    amount: strAmount(expenses_total) },
                     { name: 'Income',      amount: strAmount(income_total) },
                     { name: 'Liabilities', amount: strAmount(liabilities_total) }]
        }],
        foot: {
            caption: 'Overall Total',
            value: strAmount(assets_total + equity_total + expenses_total + income_total + liabilities_total)
        }
    });
}

function renderFinances(month, data) {
    document.title = data.when;
    document.getElementById('when').innerText = data.when;

    document.getElementById('back').style.visibility = (month == 1)  ? 'hidden' : 'visible';
    document.getElementById('next').style.visibility = (month == 12) ? 'hidden' : 'visible';

    renderBalanceSheet(data);
}

window.onload = () => {
    // Fetch the data
    renderFinancesFor(renderFinances);

    // Set up keybindings
    document.onkeyup = function(e) {
        if (e.key == 'ArrowLeft') {
            renderFinancesForLastMonth(renderFinances);
        } else if (e.key == 'ArrowRight') {
            renderFinancesForNextMonth(renderFinances);
        } else if (e.key == 'r') {
            renderFinances();
        }
    }
};
