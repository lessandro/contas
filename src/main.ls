{Str, filter, map, any, head, sum, lines, unlines} = require 'prelude-ls'

backend-url = 'http://lessandro.com/contas/ws'
names = <[ le ra lu ]>
month-names = <[ void janeiro fevereiro março abril maio junho julho agosto setembro outubro novembro dezembro ]>

prev = ""
updated = ""
months = []
guys = [0 to names.length - 1]

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
        input.prop 'checked', i == selected

        cell.append input

    cell

new-check-cell = (checked, enabled) ->
    cell = $ \<td></td>

    for i in guys
        input = $ '<input type=\"checkbox\">'
        input.val i
        input.change update
        input.prop 'checked', (i in checked)
        input.prop 'disabled', (i not in enabled)

        cell.append input

    cell

new-row = (item, tag) ->
    row = $ \<tr></tr>

    if item.name
        row.append new-text-cell item.name, "<delete>"
        row.append new-text-cell item.price
        row.append new-radio-cell \radio- + tag, item.who
        row.append new-check-cell item.owers, guys
        checked = item.payed ++ [item.who]
        enabled = filter (!= item.who), item.owers
        row.append new-check-cell checked, enabled
    else
        row.append new-text-cell "", "<new>"
        row.append "<td></td>"
        [row.append "<td class='names'>#{names * ','}</td>" for i in [3 4 5]]

    row

create-table = (month, tbody) ->
    for item, i in month.data
        tbody.append new-row item, "#{month.year}-#{month.month}-#{i}"

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
            <hr>
        </div>
    """

    div = $ html
    title = $ 'h1', div
    tbody = $ 'tbody', div
    
    div.data 'month', month.month
    div.data 'year', month.year
    title.text month-names[month.month]
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
    if isNaN num
        $('input', cell).val "0"
        0
    else
        num

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
        item.payed = filter (in item.owers), read-inputs cells[4]
        item

update = !->
    container = $ \#content
    months := []

    for element in container.children!
        div = $ element
        months.push {
            month: parse-int div.data \month
            year: parse-int div.data \year
            data: read-table element
        }

    save-data!

    refresh!

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

    # lu -> le becomes lu -> ra, ra -> le
    for item in owes[2][0]
        item.name += ' (lu → le)'
    owes[2][1] = owes[2][1] ++ owes[2][0]
    owes[1][0] = owes[1][0] ++ owes[2][0]
    owes[2][0] = []

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
    
    summaries * "<hr class='smallhr'>"

calculate = !->
    container = $ \#content

    for month, i in container.children!
        result = $ '#result', month
        result.empty!
        result.append calculate-month months[i]

# ------------

next-month = (data=[]) ->
    if months.length > 0
        last-month = months[0]
    else
        today = new Date!
        last-month =
            year: today.getFullYear!
            month: today.getMonth!
            data: []

    data = JSON.parse JSON.stringify data
    year = last-month.year
    month = last-month.month + 1

    if month == 13
        month = 1
        year++

    for entry in data
        entry.payed = []

    { month, year, data }

refresh = !->
    create-page!
    calculate!
    $ \#updated .text updated

set-status = !(text) ->
    status = $ \#status
    if text == ''
        status.fadeOut 'fast', !->
            status.text ''
    else
        status .text text
        status.fadeIn 'fast'

load-ref = !(ref) ->
    set-status 'loading...'

    $.ajax {
        type: 'GET'
        url: backend-url + ref
        success: (data) ->
            return unless data
            [prev_, json] = Str.break-str (is ' '), data
            prev := prev_
            blob = JSON.parse json
            return unless blob
            months := blob.months
            updated := blob.updated
            refresh!
        error: !->
            window.alert 'load error :('
        complete: !->
            set-status ''
    }

save-data = !->
    set-status 'saving...'

    updated := moment! .format 'YYYY-MM-DD HH:mm:ss'

    $.ajax {
        type: \POST
        url: backend-url
        data: JSON.stringify { months, updated }
        dataType: \json
        contentType: \application/json
        error: !->
            window.alert 'save error :('
        complete: !->
            set-status ''
    }

has = (str, substr) --> (str.indexOf substr) != -1

run = !->
    $ \#copy .click !->
        if months.length == 0
            months.unshift next-month!
        else
            months.unshift next-month months[0].data
        save-data!
        refresh!

    $ \#del .click !->
        if months.length == 0
            return

        if window.confirm 'sure?'
            months.shift!
            save-data!
            refresh!

    $ \#back .click !->
        if prev != ''
            load-ref prev
    
    $ \#food-calc .click !->
        text = $ \#food .val!
        is-ok = -> !(any has(it), ["JOSE", "Dispo"]) && has(it, "R$")
        valid = filter is-ok, lines text
        $ \#food .val unlines valid
        get-price = -> (last it.split("$")).replace(",", ".") * 1.0
        total = sum map get-price, valid
        $ \#food-total .val Math.round total
        $ \#food-total .focus! .select!

    load-ref ""

$ run
