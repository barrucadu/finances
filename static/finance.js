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

function renderAssets(raw_assets_data) {
    let assets_data = {
        datasets: [{
            data: Object.values(raw_assets_data),
            backgroundColor: Object.values(raw_assets_data).map(() => `rgb(${randRange(0,255)}, ${randRange(0,255)}, ${randRange(0,255)})`)
        }],
        labels: Object.keys(raw_assets_data)
    }

    let assets_ctx = document.getElementById('assets_chart').getContext('2d');
    new Chart(assets_ctx, {
        type: 'doughnut',
        data: assets_data,
        options: {
            responsive: false,
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
                    text: `${strAmount(assets_data.datasets[0].data.reduce((a,b) => a+b))}`,
                    color: '#36A2EB',
                    fontStyle: 'CallunaSansRegular',
                }
            }
        }
    });
}

function renderTable(raw_data, ele, flipGoodBad) {
    ele.innerHTML = '';

    let lastRow;
    for (let source in raw_data) {
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

    lastRow.className = 'nobottom';
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

    let totalBalance = 0;
    let lastRow;
    for (let day in raw_history_data) {
        let title = document.createElement('th');
        title.innerText = day;

        for (let i = 0; i < raw_history_data[day].length; i ++) {
            let entry = raw_history_data[day][i];

            let description = document.createElement('td');
            description.innerText = entry.title;
            let amount = document.createElement('td');
            showAmount(amount, entry.amount)
            amount.classList.add('num');

            lastRow = table.insertRow();
            lastRow.className = 'lightbottom';
            lastRow.appendChild(title);
            lastRow.appendChild(description);
            lastRow.appendChild(amount);

            title = document.createElement('th');
            totalBalance += entry.amount;
        }

        lastRow.className = '';
    }

    lastRow.className = 'nobottom';

    showAmount(total, totalBalance);
}

function renderFinances(data) {
    document.getElementById('when').innerText = data.when;

    renderAssets(data.assets);
    renderIncome(data.income);
    renderBudget(data.budget);
    renderExpenses(data.expenses);
    renderHistory(data.history);
}

window.onload = () => {
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
};
