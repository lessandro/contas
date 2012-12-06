guys = [0 1 2]
names = <[ le ra lu ]>

data = [
    *   name: "Aluguel"
        price: 900.00
        who: 1
        owers: [0, 1, 2]
        payed: [1]
        id: 0

    *   name: "Condominio"
        price: 300.00
        who: 1
        owers: [0, 1, 2]
        payed: [1]
        id: 1

    *   name: "Internet"
        price: 150.00
        who: 0
        owers: [0, 1, 2]
        payed: [0]
        id: 2

    *   name: "Telefone"
        price: 40.00
        who: 0
        owers: [1, 2]
        payed: [0]
        id: 3

    *   name: "Food"
        price: 400.00
        who: 0
        owers: [0, 1]
        payed: [0]
        id: 4
]

new-text-cell = (text) ->
    cell = $ \<td></td>
    input = $ '<input type="text" class="textcell">'
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

new-button-cell = (text) ->
    cell = $ \<td></td>

    input = $ '<input type=\"button\">'
    input.val text

    cell.append input

new-row = ->
    row = $ \<tr></tr>

    row.append new-text-cell it.name
    row.append new-text-cell it.price
    row.append new-radio-cell \radio + it.id, it.who
    row.append new-check-cell it.owers
    row.append new-check-cell it.payed
    row.append new-button-cell \del

    row

read-text = (cell) ->
    input = $ 'input', cell
    input.val!

read-inputs = (cell) ->
    inputs = $ 'input', cell
    checked = []
    inputs.each ->
        if $ @ .prop 'checked'
            checked.push($ @ .val!)
    map parse-int, checked

update = !->
    tbl = $ \#table
    for row, i in tbl.children!
        cells = $ row .children!
        data[i].name = read-text cells[0]
        data[i].price = parse-float read-text cells[1]
        data[i].who = head read-inputs cells[2]
        data[i].owers = read-inputs cells[3]
        data[i].payed = read-inputs cells[4]

    calculate!

create-table = !->
    tbl = $ \#table
    for item in data
        row = new-row item
        tbl.append row

calculate = !->
    # owes[i][j] = i owes j
    owes = [[0 for i in guys] for j in guys]

    for item in data
        value = item.price
        num = item.owers.length
        divided = value * 1.0 / num
        for i in item.owers
            if i not in item.payed
                owes[i][item.who] += divided

    result = $ \#result
    result.empty!

    for i in guys
        for j in guys
            if i == j
                continue
            owed = owes[i][j] - owes[j][i]
            if owed <= 0
                continue
            result.append "#{names[i]} -> #{names[j]} R$ #{owed}<br>"

run = !->
    create-table!
    calculate!

$ run
