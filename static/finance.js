const THIS_MONTH = new Date().getMonth() + 1;

var cached_assets_data = undefined;
var show_breakdown = false;
var hidden_accounts = {};

function hoverCallback(f) {
    return function (tooltipItem, data) {
        var label = data.labels[tooltipItem.index];
        let value = data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
        return `${label}: ${f(value)}`;
    }
}

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
                if (asset.breakdown[i] == account) continue;
                toggle(`${asset.name}${asset.breakdown[i].name}`);
            }
        } else {
            toggle(`${asset.name}${account.name}`);
        }
    }

    renderAssets(cached_assets_data);
}

function colour(str, alpha=1) {
    let total = 0;
    for (let i = 0; i < str.length; i ++) {
        total += str.charCodeAt(i);
    }

    return `rgba(${total % 255}, ${(total * 7) % 255}, ${(total * 13) % 255}, ${alpha})`
}

function zeroish(val) {
    return val < 0.01 && val > -0.01;
}

function zeroise(val) {
    return (val > -0.01) ? val : 0
}

function randRange(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function strAmount(amount, showPlus=false) {
    let sign = (amount > -0.01) ? (showPlus ? '+' : '') : '-';
    let amt = Math.abs(amount).toFixed(2);
    return `${sign}£${amt}`;
}

function showAmount(ele, amount, brackets=false, flipGoodBad=false, noEmphasisGood=false, noEmphasisBad=false) {
    ele.classList.remove('good');
    ele.classList.remove('bad');
    ele.innerText = '';

    if (amount != 0) {
        let isGood = flipGoodBad ? amount < 0 : amount > 0;

        if (isGood && !noEmphasisGood) {
            ele.classList.add('good');
        } else if (!isGood && !noEmphasisBad) {
            ele.classList.add('bad');
        }

        ele.innerText = `${strAmount(amount, !noEmphasisGood)}`;
        if (brackets) {
            ele.innerText = `(${ele.innerText})`;
        }
    }
}

function renderAssetsTags(raw_assets_data) {
    let canvas = document.createElement('canvas')
    canvas.id = 'assets_tags';
    canvas.width = 300;
    canvas.height = 300;

    let data = {};
    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];
            let total = account.tags.reduce((acc, t) => acc + t.share, 0);
            for (let j = 0; j < account.tags.length; j ++) {
                let tag = account.tags[j];
                let amount = isHidden(asset, account) ? 0 : account.amount;
                data[tag.tag] = ((tag.tag in data) ? data[tag.tag] : 0) + amount * (tag.share / total);
            }
        }
    }

    let keys = Object.keys(data).sort();
    let total = Object.values(data).reduce((acc, d) => acc + d, 0);

    new Chart(canvas.getContext('2d'), {
        type: 'bar',
        data: {
            datasets: [{
                data: keys.map(k => 100 * zeroise(data[k]) / total),
                backgroundColor: keys.map(k => colour(k, 0.2)),
                borderColor: keys.map(k => colour(k)),
                borderWidth: 1
            }],
            labels: keys
        },
        options: {
            tooltips: { callbacks: { label: hoverCallback(x => `${Math.abs(x).toFixed(0)}%`) } },
            scales: { yAxes: [{ ticks: { min: 0, max: 100 } }] }
        }
    });

    return canvas;
}

function renderAssetsChart(raw_assets_data) {
    let canvas = document.createElement('canvas');
    canvas.id = 'assets_chart';
    canvas.width = 300;
    canvas.height = 300;

    let data = {};
    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        if (show_breakdown && asset.breakdown.length > 1) {
            for (let i = 0; i < asset.breakdown.length; i ++) {
                let account = asset.breakdown[i];
                let amount = isHidden(asset, account) ? 0 : account.amount;
                data[account.name] = {
                    bgcolour: colour(`${asset.name}${account.name}`, 0.2),
                    bordercolour: colour(`${asset.name}${account.name}`),
                    amount: amount
                };
            }
        } else {
            data[asset.name] = {
                bgcolour: colour(asset.name, 0.2),
                bordercolour: colour(asset.name),
                amount: asset.breakdown.reduce((acc, d) => isHidden(asset, d) ? acc : acc + d.amount, 0)
            };
        }
    }

    let keys = Object.keys(data);
    let total = Object.values(data).reduce((acc, d) => acc + d.amount, 0);

    new Chart(canvas.getContext('2d'), {
        type: 'doughnut',
        data: {
            datasets: [{
                data: keys.map(k => zeroise(data[k].amount)),
                backgroundColor: keys.map(k => data[k].bgcolour),
                borderColor: keys.map(k => data[k].bordercolour),
                borderWidth: 1
            }],
            labels: keys
        },
        options: {
            tooltips: { callbacks: { label: hoverCallback(strAmount) } },
            elements: {
                center: {
                    text: `${strAmount(total)}`,
                    color: '#36A2EB',
                    fontStyle: 'CallunaSansRegular',
                }
            }
        }
    });

    return canvas;
}

function renderAssetsLegend(raw_assets_data) {
    let legend = document.createElement('table');
    legend.id = 'assets_legend';

    function add_row_cells(row, colour, name, url, strvalue, onclick, subcolour=null) {
        let acc_colour_cell = row.insertCell();
        acc_colour_cell.style.backgroundColor = colour;
        acc_colour_cell.className = 'colour';
        acc_colour_cell.appendChild(document.createElement('span'));

        if (subcolour !== null) {
            let subacc_colour_cell = row.insertCell();
            subacc_colour_cell.style.backgroundColor = subcolour;
            subacc_colour_cell.className = 'colour';
            subacc_colour_cell.appendChild(document.createElement('span'));
        }

        let acc_name_cell = row.insertCell();
        let acc_name = document.createElement('span');
        acc_name.innerText = name;
        acc_name.onclick = onclick;
        acc_name_cell.appendChild(acc_name);

        if (subcolour == null) {
            acc_name_cell.colSpan = '2';
        }

        if (url !== undefined) {
            let link = document.createElement('a');
            link.href = url;
            link.innerText = '(?)';
            link.className = 'note';
            link.title = 'More information...';
            acc_name_cell.appendChild(link);
        }

        let value_cell = row.insertCell();
        value_cell.innerText = strvalue;
        value_cell.className = 'num';
    }

    let row;
    for (let key in raw_assets_data) {
        let asset = raw_assets_data[key];
        let total = asset.breakdown.reduce((acc, d) => acc + d.amount, 0)

        if(zeroish(total)) continue;

        row = legend.insertRow();
        add_row_cells(row,
                      colour(asset.name),
                      asset.name,
                      asset.url,
                      strAmount(total),
                      () => toggleHide(asset));
        if (isHidden(asset)) {
            row.classList.add('hidden');
        }

        if (asset.breakdown.length == 1) {
            continue;
        }
        row.classList.add('lightbottom');

        for (let i = 0; i < asset.breakdown.length; i ++) {
            let account = asset.breakdown[i];

            row = legend.insertRow();
            row.classList.add('subaccount');
            row.classList.add('lightbottom');
            if (isHidden(asset, account)) {
                row.classList.add('hidden');
            }
            add_row_cells(row,
                          colour(asset.name),
                          account.name,
                          account.url,
                          `${Math.abs(100 * account.amount / total).toFixed(0)}%`,
                          () => toggleHide(asset, account),
                          colour(`${asset.name}${account.name}`));
        }
        row.classList.remove('lightbottom');
    }

    row.classList.add('nobottom');

    return legend;
}

function renderAssets(raw_assets_data) {
    cached_assets_data = raw_assets_data;

    function piechart() {
        document.getElementById('assets_chart_container').removeChild(document.getElementById('assets_chart'));
        document.getElementById('assets_chart_container').appendChild(renderAssetsChart(raw_assets_data));

        document.getElementById('assets_chart').onclick = function() {
            show_breakdown = !show_breakdown;
            piechart();
        };
    }

    document.getElementById('assets_tags_container').removeChild(document.getElementById('assets_tags'));
    document.getElementById('assets_tags_container').appendChild(renderAssetsTags(raw_assets_data));

    piechart();

    document.getElementById('assets_legend_container').removeChild(document.getElementById('assets_legend'));
    document.getElementById('assets_legend_container').appendChild(renderAssetsLegend(raw_assets_data));
}

function renderTable(raw_data, ele, flipGoodBad) {
    ele.innerHTML = '';

    let lastRow;
    let sources = Object.keys(raw_data).sort();
    for (let i = 0; i < sources.length; i ++) {
        let source = sources[i];
        if (zeroish(raw_data[source].amount)) continue;

        let title = document.createElement('th');
        title.innerText = source;

        let delta = document.createElement('td');
        showAmount(delta, raw_data[source].delta, true, flipGoodBad)

        let balance = document.createElement('td');
        showAmount(balance, raw_data[source].amount, false, false, true)
        balance.classList.add('num');

        lastRow = ele.insertRow();
        lastRow.appendChild(title);
        lastRow.appendChild(delta);
        lastRow.appendChild(balance);
    }

    if(lastRow !== undefined) {
        lastRow.className = 'nobottom';
    }
}

function renderIncome(raw_income_data) {
    renderTable(raw_income_data, document.getElementById('income_table'), false);
}

function renderBudget(raw_budget_data) {
    renderTable(raw_budget_data, document.getElementById('budget_table'), false);
}

function renderExpenses(raw_expenses_data) {
    renderTable(raw_expenses_data, document.getElementById('expenses_table'), true);
}

function renderHistory(raw_history_data) {
    let table = document.getElementById('history_table');
    let total = document.getElementById('history_total');
    table.innerHTML = '';

    let totalDelta = 0;
    let lastRow;
    let days = Object.keys(raw_history_data).sort().reverse();
    for (let i = 0; i < days.length; i ++) {
        let day = days[i];

        let title = document.createElement('th');
        title.innerText = day;

        for (let j = 0; j < raw_history_data[day].length; j ++) {
            let entry = raw_history_data[day][j];
            if (zeroish(entry.delta)) continue;

            let description = document.createElement('td');
            description.innerText = entry.title;
            let delta = document.createElement('td');
            showAmount(delta, entry.delta)
            delta.classList.add('num');

            lastRow = table.insertRow();
            lastRow.className = 'lightbottom';
            lastRow.appendChild(title);
            lastRow.appendChild(description);
            lastRow.appendChild(delta);

            title = document.createElement('th');
            totalDelta += entry.delta;
        }

        lastRow.className = '';
    }

    lastRow.className = 'nobottom';

    showAmount(total, totalDelta);
}

function renderFinancesFor(month) {
    let httpRequest = new XMLHttpRequest();
    httpRequest.onreadystatechange = function() {
        if(httpRequest.readyState === XMLHttpRequest.DONE && httpRequest.status === 200) {
            let response = JSON.parse(httpRequest.responseText);
            renderFinances(month, response);
        }
    };
    httpRequest.open('GET', (month == THIS_MONTH) ? '/data' : `/data?month=${month}`);
    httpRequest.send();
}

function renderFinances(month, data) {
    document.title = data.when;
    document.getElementById('when').innerText = data.when;

    document.getElementById('back').onclick = () => renderFinancesFor(month - 1);
    document.getElementById('next').onclick = () => renderFinancesFor(month + 1);
    document.getElementById('back').style.visibility = (month == 1)  ? 'hidden' : 'visible';
    document.getElementById('next').style.visibility = (month == 12) ? 'hidden' : 'visible';

    document.onkeyup = function(e) {
        if (e.key == 'ArrowLeft' && month > 1) {
            renderFinancesFor(month - 1);
        } else if (e.key == 'ArrowRight' && month < 12) {
            renderFinancesFor(month + 1);
        }
    }

    renderAssets(data.assets);
    renderIncome(data.income);
    renderBudget(data.budget);
    renderExpenses(data.expenses);
    renderHistory(data.history);
}

window.onload = () => {
    Chart.defaults.global.responsive = false;
    Chart.defaults.global.maintainAspectRatio = true;
    Chart.defaults.global.legend.display = false;

    Chart.pluginService.register({
        beforeDraw: function (chart) {
            if (chart.config.options.elements.center) {
                //Get ctx from string
                var ctx = chart.chart.ctx;

                //Get options from the center object in options
                var centerConfig = chart.config.options.elements.center;
                var fontStyle = centerConfig.fontStyle || 'Arial';
                var txt = centerConfig.text;
                var color = centerConfig.color || '#000';
                var sidePadding = centerConfig.sidePadding || 20;
                var sidePaddingCalculated = (sidePadding/100) * (chart.innerRadius * 2)
                //Start with a base font of 30px
                ctx.font = "30px " + fontStyle;

                //Get the width of the string and also the width of the element minus 10 to give it 5px side padding
                var stringWidth = ctx.measureText(txt).width;
                var elementWidth = (chart.innerRadius * 2) - sidePaddingCalculated;

                // Find out how much the font can grow in width.
                var widthRatio = elementWidth / stringWidth;
                var newFontSize = Math.floor(30 * widthRatio);
                var elementHeight = (chart.innerRadius * 2);

                // Pick a new font size so it will not be larger than the height of label.
                var fontSizeToUse = Math.min(newFontSize, elementHeight);

                //Set font settings to draw it correctly.
                ctx.textAlign = 'center';
                ctx.textBaseline = 'middle';
                var centerX = ((chart.chartArea.left + chart.chartArea.right) / 2);
                var centerY = ((chart.chartArea.top + chart.chartArea.bottom) / 2);
                ctx.font = fontSizeToUse+"px " + fontStyle;
                ctx.fillStyle = color;

                //Draw text in center
                ctx.fillText(txt, centerX, centerY);
            }
        }
    });

    // Fetch the data
    renderFinancesFor(THIS_MONTH);
};
