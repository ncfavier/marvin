SECONDS = 1024
MINUTES = 1025
HOURS   = 1026

loop:
    load SECONDS
    add 1
    store SECONDS
    sub 60
    jl loop

    store SECONDS
    load MINUTES
    add 1
    store MINUTES
    sub 60
    jl loop

    store MINUTES
    load HOURS
    add 1
    store HOURS
    sub 24
    jl loop

    store HOURS
    jump loop
