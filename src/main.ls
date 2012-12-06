guys = [0 1 2]
names = <[ le ra lu ]>

data = [
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

# ---------

new-text-cell = (text) ->
    cell = $ \<td></td>
    input = $ '<input type="text" class="textcell" placeholder="novo">'
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

new-row = (it, i) ->
    row = $ \<tr></tr>

    row.append new-text-cell it.name
    if it.name
        row.append new-text-cell it.price
        row.append new-radio-cell \radio + i, it.who
        row.append new-check-cell it.owers
        row.append new-check-cell it.payed
    else
        [row.append "<td></td>" for i in [1 to 5]]

    row

create-table = !->
    tbl = $ \#table
    tbl.empty!

    for item, i in data
        tbl.append new-row item, i

    tbl.append new-row {}, data.length

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

update = !->
    tbl = $ \#table
    data := []

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
        data.push item

    create-table!
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

calculate = !->
    # owes[i][j] = i owes j
    owes = [[[] for i in guys] for j in guys]

    for item in data
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
            summary += "<span class=\"owed\">#{print-list owes[i][j]}</span>"

            if ji > 0
                summary += "<span class=\"deducted\">, #{print-list map neg-amount, owes[j][i]}</span>"
            summary += "</h6>"

            summaries.push summary
    
    result = $ \#result
    result.empty!
    result.append summaries * "<hr>"

# ------------

run = !->
    create-table!
    calculate!

$ run
