guys = [0 1 2]
names = <[ le ra lu ]>

months = [
*   name: "janeiro",
    data: [
    *   name: "aluguel"
        price: 900.00
        who: 1
        owers: [0, 1, 2]
        payed: [1]
    ]

*   name: "dezembro"
    data: [
    *   name: "aluguel"
        price: 900.00
        who: 1
        owers: [0, 1, 2]
        payed: [1]

    *   name: "condominio"
        price: 300.00
        who: 1
        owers: [0, 1, 2]
        payed: [1]

    *   name: "internet"
        price: 150.00
        who: 0
        owers: [0, 1, 2]
        payed: [0]

    *   name: "telefone"
        price: 40.00
        who: 0
        owers: [1, 2]
        payed: [0]

    *   name: "food"
        price: 400.00
        who: 0
        owers: [0, 1]
        payed: [0]

    *   name: "luz"
        price: 100.00
        who: 1
        owers: [0, 1, 2]
        payed: [1]
    ]
]

# ---------

new-text-cell = (text, placeholder="") ->
    cell = $ \<td></td>
    input = $ '<input type="text" class="textcell">'
    input.attr 'placeholder', placeholder
    input.prop 'value', text
    input.change update
    cell.append input

new-radio-cell = (name, selected) ->
    cell = $ \<td></td>

    for i in guys
        input = $ '<input type=\"radio\">'
        input.attr 'name', name
        input.val i
        input.change update
        if i == selected
            input.prop 'checked', true

        cell.append input

    cell

new-check-cell = (checked) ->
    cell = $ \<td></td>

    for i in guys
        input = $ '<input type=\"checkbox\">'
        input.val i
        input.change update
        if i in checked
            input.prop 'checked', true

        cell.append input

    cell

new-row = (it, tag) ->
    row = $ \<tr></tr>

    if it.name
        row.append new-text-cell it.name, "<delete>"
        row.append new-text-cell it.price
        row.append new-radio-cell \radio- + tag, it.who
        row.append new-check-cell it.owers
        row.append new-check-cell it.payed
    else
        row.append new-text-cell "", "<new>"
        row.append "<td></td>"
        [row.append "<td class='names'>#{names * ','}</td>" for i in [3 4 5]]

    row

create-table = (month, tbody) ->
    for item, i in month.data
        tbody.append new-row item, month.name + i

    tbody.append new-row {}

create-month = (month) ->
    html = """
        <div>
            <h1></h1>
            <div class="row">
                <div class="span6 tablebox">
                    <table width="100%">
                        <thead class="tablehead">
                            <tr>
                                <th>coisa</th>
                                <th>$</th>
                                <th>pagou</th>
                                <th>deve pagar</th>
                                <th>pagou</th>
                            </tr>
                        </thead>
                        <tbody id="table">
                        </tbody>
                    </table>
                </div>

                <div class="span5" id="result">
                </div>
            </div>
        </div>
    """

    div = $ html
    title = $ 'h1', div
    tbody = $ 'tbody', div
    
    title.text month.name
    create-table month, tbody
    div

create-page = !->
    container = $ \#content
    container.empty!

    for month in months
        container.append create-month month

# ---------

read-text = (cell) ->
    input = $ 'input', cell
    input.val!

read-float = (cell) ->
    num = parse-float read-text cell
    if isNaN num then 0 else num

read-inputs = (cell) ->
    inputs = $ 'input', cell
    checked = []
    inputs.each ->
        if $ @ .prop 'checked'
            checked.push($ @ .val!)
    map parse-int, checked

read-table = (element) ->
    tbl = $ 'tbody', element

    for row, i in tbl.children!
        cells = $ row .children!
        item = {}
        item.name = read-text cells[0]
        if item.name.length == 0
            continue

        item.price = read-float cells[1]
        item.who = head read-inputs cells[2]
        item.owers = read-inputs cells[3]
        item.payed = read-inputs cells[4]
        item

update = !->
    container = $ \#content
    months := []

    for month in container.children!
        name = ($ 'h1', month).text!
        data = read-table month
        months.push { name, data }

    create-page!
    calculate!

# ---------

currency = (amount) ->
    if amount >= 0
        '$' + amount.toFixed 0
    else
        '-$' + (-amount).toFixed 0

neg-amount = (item) ->
    { item.name, amount: -item.amount }

print-item = (item) ->
    "<nobr>#{item.name} #{currency item.amount}</nobr>"

print-list = (items) ->
    (map print-item, items) * ", "

print-span-list = (items, css) ->
    "<span class=\"#{css}\">#{print-list items}</span>"

calculate-month = (month) ->
    # owes[i][j] = i owes j
    owes = [[[] for i in guys] for j in guys]

    for item in month.data
        num = item.owers.length
        amount = item.price * 1.0 / num
        for i in item.owers
            if i not in item.payed
                owes[i][item.who].push { item.name, amount }

    summaries = []

    for i in guys
        for j in guys
            if i == j
                continue
            ij = sum map (.amount), owes[i][j]
            ji = sum map (.amount), owes[j][i]
            if ij <= ji
                continue

            summary = "<h4>#{names[i]} → #{names[j]} #{currency ij - ji}</h4>"
            summary += "<h6>"
            summary += print-span-list owes[i][j], \owed

            if ji > 0
                negged = map neg-amount, owes[j][i]
                summary += ", " + print-span-list negged, \deducted
            summary += "</h6>"

            summaries.push summary
    
    summaries * "<hr>"

calculate = !->
    container = $ \#content

    for month, i in container.children!
        result = $ '#result', month
        result.append calculate-month months[i]

# ------------

run = !->
    create-page!
    calculate!

$ run
