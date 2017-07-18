const THIS_MONTH = new Date().getMonth() + 1;

var show_breakdown = false;

function zeroish(val) {
    return val < 0.01 && val > -0.01;
}

function randRange(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function strAmount(amount, showPlus=false) {
    let sign = (amount > 0) ? (showPlus ? '+' : '') : '-';
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

function renderAssetsBreakdown(breakdown_data) {
    let breakdown = document.createElement('canvas')
    breakdown.id = 'assets_breakdown';
    breakdown.width = 300;
    breakdown.height = 300;

    let data = {};
    for (let key in breakdown_data) {
        let asset = breakdown_data[key];
        data[asset.tag] = ((asset.tag in data) ? data[asset.tag] : 0) + asset.amount;
    }

    let keys = Object.keys(data).sort();
    let total = Object.values(data).reduce((acc, d) => acc + d, 0);
    let colours = keys.map(() => [randRange(0,255), randRange(0,255), randRange(0,255)]);

    new Chart(breakdown.getContext('2d'), {
        type: 'bar',
        data: {
            datasets: [{
                data: keys.map(k => (data[k] > -0.01) ? 100 * data[k] / total : 0),
                backgroundColor: colours.map(c => `rgba(${c[0]}, ${c[1]}, ${c[2]}, 0.2)`),
                borderColor: colours.map(c => `rgb(${Math.max(0, c[0]-20)}, ${Math.max(0, c[1]-20)}, ${Math.max(0, c[2]-20)})`),
                borderWidth: 1
            }],
            labels: keys
        },
        options: {
            tooltips: {
                callbacks: {
                    label: function(tooltipItem, data) {
                        var label = data.labels[tooltipItem.index];
                        let percent = data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
                        return `${label}: ${Math.abs(percent).toFixed(0)}%`;
                    }
                }
            },
            scales: {
                yAxes: [{ ticks: { min: 0, max: 100 } }]
            }
        }
    });

    return breakdown;
}

function renderAssetsChart(raw_assets_data, colours) {
    let keys = Object.keys(raw_assets_data).sort();
    let assets_data = {
        datasets: [{
            data: keys.map(k => (raw_assets_data[k].amount > -0.01) ? raw_assets_data[k].amount : 0),
            backgroundColor: keys.map(k => colours[k]),
        }],
        labels: keys
    };

    let canvas = document.createElement('canvas');
    canvas.id = 'assets_chart';
    canvas.width = 300;
    canvas.height = 300;

    new Chart(canvas.getContext('2d'), {
        type: 'doughnut',
        data: assets_data,
        options: {
            tooltips: {
                callbacks: {
                    label: function(tooltipItem, data) {
                        var label = data.labels[tooltipItem.index];
                        let amount = data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
                        return `${label}: ${strAmount(amount)}`;
                    }
                }
            },
            elements: {
                center: {
                    text: `${strAmount(Object.values(raw_assets_data).reduce((acc, d) => acc + d.amount, 0))}`,
                    color: '#36A2EB',
                    fontStyle: 'CallunaSansRegular',
                }
            }
        }
    });

    return canvas;
}

function renderAssetsLegend(raw_assets_data, colours) {
    let legend = document.createElement('table');
    legend.id = 'assets_legend';

    let keys = Object.keys(raw_assets_data).sort();
    for (let i = 0; i < keys.length; i ++) {
        let key = keys[i];
        let asset = raw_assets_data[key];

        if(zeroish(asset.amount)) continue;

        let row = legend.insertRow();
        row.className = 'nobottom';

        let colour_cell = row.insertCell();
        colour_cell.style.backgroundColor = colours[key];
        colour_cell.className = 'colour';

        let name_cell = row.insertCell();
        name_cell.innerText = key;

        if (asset.url !== undefined) {
            let link = document.createElement('a');
            link.href = asset.url;
            link.innerText = '(?)';
            link.className = 'note';
            link.title = 'More information...';
            name_cell.appendChild(link);
        }

        row.insertCell().innerText = strAmount(asset.amount);
    }

    return legend;
}

function renderAssets(raw_data, refresh=false) {
    let raw_assets_data = show_breakdown ? raw_data.breakdown : raw_data.assets;

    if (!refresh) {
        document.getElementById('assets_breakdown_container').removeChild(document.getElementById('assets_breakdown'));
        document.getElementById('assets_breakdown_container').appendChild(renderAssetsBreakdown(raw_data.breakdown));
    }

    let keys = Object.keys(raw_assets_data);
    let colours = {};
    for (let i = 0; i < keys.length; i ++) {
        colours[keys[i]] = `rgb(${randRange(0,255)}, ${randRange(0,255)}, ${randRange(0,255)})`;
    }

    document.getElementById('assets_chart_container').removeChild(document.getElementById('assets_chart'));
    document.getElementById('assets_chart_container').appendChild(renderAssetsChart(raw_assets_data, colours));

    document.getElementById('assets_legend_container').removeChild(document.getElementById('assets_legend'));
    document.getElementById('assets_legend_container').appendChild(renderAssetsLegend(raw_assets_data, colours));

    document.getElementById('assets_chart').onclick = function() {
        show_breakdown = !show_breakdown;
        renderAssets(raw_data, true);
    };
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

    renderAssets(data);
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
