data = [
    *   name: "Aluguel"
        price: 123.00
        owner: 0
        owers: [0, 1, 2]
        payed: [0, 1]
        id: 1

    *   name: "Internet"
        price: 123.00
        owner: 1
        owers: [0, 1]
        payed: [0]
        id: 2

    *   name: "Telefone"
        price: 123.00
        owner: 2
        owers: [1, 2]
        payed: [2]
        id: 3
]

new-text-cell = (text) ->
    cell = $ \<td></td>
    input = $ '<input type="text" class="textcell">'
    input.prop 'value', text
    cell.append input

new-radio-cell = (name, selected) ->
    cell = $ \<td></td>

    for i in [1 to 3]
        input = $ '<input type=\"radio\">'
        input.attr 'name', name
        if i == selected
            input.prop 'checked', true

        cell.append input

    cell

new-check-cell = (checked) ->
    cell = $ \<td></td>

    for i in [1 to 3]
        input = $ '<input type=\"checkbox\">'
        if i in checked
            input.prop 'checked', true

        cell.append input

    cell

new-row = ->
    row = $ \<tr></tr>

    row.append new-text-cell it.name
    row.append new-text-cell it.price
    row.append new-radio-cell \radio + it.id, it.owner
    row.append new-check-cell it.owers
    row.append new-check-cell it.payed

    row

run = !->
    tbl = $ \#table
    for item in data
        row = new-row item
        tbl.append row

run!