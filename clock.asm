SECOND = 1024
MINUTE = 1025
HOUR   = 1026
DAY    = 1027
MONTH  = 1028
YEAR   = 1029
PULSE  = 1030

loop:
    load PULSE
    sub 1
    jl loop
    store PULSE

    load SECOND
    add 1
    store SECOND
    sub 60
    jl loop

    store SECOND
    load MINUTE
    add 1
    store MINUTE
    sub 60
    jl loop

    store MINUTE
    load HOUR
    add 1
    store HOUR
    sub 24
    jl loop

    store HOUR
    load DAY
    add 1
    store DAY
    sub 31
    jl loop

    store DAY
    load MONTH
    add 1
    store MONTH
    sub 12
    jl loop

    store MONTH
    load YEAR
    add 1
    store YEAR
    jump loop
