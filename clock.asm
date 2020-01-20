SECOND = 1024
MINUTE = 1025
HOUR   = 1026
DAY    = 1027
MONTH  = 1028
YEAR   = 1029
SYNC   = 1030

loop:
    load SYNC
    jz loop
    sub 1
    store SYNC

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
    load MONTH
    sub 2
    jz february
    load DAY
    sub 31 ; if (MONTH & 1) ^ (MONTH & 8), sub 32
    jump end_february
february:
    load DAY
    sub 29 ; if leap year, sub 30
end_february:
    jl loop

    add 1
    store DAY
    load MONTH
    add 1
    store MONTH
    sub 13
    jl loop

    add 1
    store MONTH
    load YEAR
    add 1
    store YEAR
    jump loop
