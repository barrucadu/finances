// Render a transaction history
function renderHistoryFor(raw_data, eleid, filter='', hide_empty=false) {
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

            if(transaction.title.toLowerCase().indexOf(filter) == -1) {
                continue;
            }

            transactions.push({
                'title': transaction.title,
                'good': transaction.delta > 0,
                'bad': transaction.delta < 0,
                'delta': virtual ? '' : strAmount(transaction.delta, true),
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

    if (hide_empty) {
        document.getElementById(`history_${eleid}_table`).style.display = (entries.length == 0) ? 'none' : 'table';
    }

    document.getElementById(`history_${eleid}_tbody`).innerHTML = Mustache.render(TPL_HISTORY_TABLE_BODY, {
        entry: entries
    });
    document.getElementById(`history_${eleid}_tfoot`).innerHTML = Mustache.render(TPL_HISTORY_TABLE_FOOT, {
        delta: strAmount(totalDelta, true),
        good: totalDelta > 0,
        bad: totalDelta < 0
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

// Render the history tables.
function renderHistory() {
    let filter = document.getElementById('search').value.toLowerCase();
    for (let i = 1; i < 13; i ++) {
        renderHistoryFor(cached_data[i-1], i, filter, true);
    }
}

window.onload = () => ajax(`/history`, all_data => {
    let year = new Date().getFullYear();

    cached_data = [];
    for (let i = 1; i < 13; i ++) {
        cached_data.push(getMonthlyTxns(all_data, year, i));
    }

    renderHistory();
});
