{spawn, exec} = require \child_process
watchr = require \watchr

run = (cmd, silent = true) ->
    exec cmd, [], (error, stdout, stderr) ->
        return if silent
        console.error stderr if stderr
        console.log stdout if stdout

build = ->
    run 'cat src/*.ls | lsc -sdbc > www/js/main.js', false

task \build \build build

task \watch \watch ->
    build!
    console.log 'watching'
    watchr.watch {
        path: \src
        listener: (e, file) ->
            if /\.ls$/.test file
                console.log file
                build!
    }
