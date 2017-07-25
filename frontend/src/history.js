// Render a transaction history
function renderHistory(raw_data, eleid) {
    let entries = [];
    let totalDelta = 0;
    let days = Object.keys(raw_data).sort().reverse();
    for (let i = 0; i < days.length; i ++) {
        let day  = days[i];
        let data = raw_data[day];

        let transactions = [];
        for (let j = 0; j < data.length; j ++) {
            let transaction = data[j];
            let virtual = zeroish(transaction.delta);

            transactions.push({
                'title': transaction.title,
                'good':  transaction.delta > 0,
                'delta':   virtual ? '' : strAmount(transaction.delta, true),
                'virtual': virtual
            });

            totalDelta += transaction.delta;
        }

        if (transactions.length > 0) {
            entries.push({
                'day': day.substr(5),
                'first': transactions[0],
                'rest': transactions.slice(1)
            });
        }
    }

    document.getElementById(eleid).innerHTML = Mustache.render(TPL_HISTORY_TABLE, {
        entry: entries,
        foot: {
            delta: strAmount(totalDelta, true),
            good: totalDelta > 0
        }
    });
}

// Extract one month of data from the conglomerate.
function getMonthlyTxns(raw_data, year, month) {
    let out = {};
    for (let i = 1; i < 32; i ++) {
        let key = `${year}-${month.toString().padStart(2, '0')}-${i.toString().padStart(2, '0')}`;
        if (key in raw_data) {
            out[key] = raw_data[key];
        }
    }
    return out;
}

window.onload = () => ajax(`/history`, all_data => {
    // From sidebar.js
    renderSidebar();

    // render one month at a time, all into separate tables.
    let year = new Date().getFullYear();
    for (let i = 12; i > 0; i --) {
        let data = getMonthlyTxns(all_data, year, i);
        renderHistory(data, `history_for_${i}`);
    }
});
