second = 1024
minute = 1025
hour   = 1026
day    = 1027
month  = 1028
year   = 1029
sync   = 1030

loop:
    lda sync
    jz loop
    sub 1
    sta sync

    lda second
    add 1
    sta second
    sub 60
    jl loop

    sta second
    lda minute
    add 1
    sta minute
    sub 60
    jl loop

    sta minute
    lda hour
    add 1
    sta hour
    sub 24
    jl loop

    sta hour
    lda day
    add 1
    sta day
    lda month
    sub 2
    jz february
    lda day
    sub 31 ; if (month & 1) ^ (month & 8), sub 32
    jmp end_february
february:
    lda day
    sub 29 ; if leap year, sub 30
end_february:
    jl loop

    add 1
    sta day
    lda month
    add 1
    sta month
    sub 13
    jl loop

    add 1
    sta month
    lda year
    add 1
    sta year
    jmp loop
