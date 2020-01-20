second = 1024
minute = 1025
hour   = 1026
day    = 1027
month  = 1028
year   = 1029
sync   = 1030

loop:
    mov *sync %a
    jz loop
    dec %a
    mov %a *sync

    mov *second %a
    inc %a
    mov %a *second
    sub 60 %a
    jl loop
    mov %a *second

    mov *minute %a
    inc %a
    mov %a *minute
    sub 60 %a
    jl loop
    mov %a *minute

    mov *hour %a
    inc %a
    mov %a *hour
    sub 24 %a
    jl loop
    mov %a *hour

    mov *month %a
    sub 2 %a
    jz february
    add 2 %a
    mov %a %c
    and 8 %a
    test %a %a
    and 1 %c
    test %c %c
    xor %a %c
    and 31 %c
    jmp end_february
february:
    mov 29 %c
    mov *year %b
    mov %b %a
    mod 4 %a
    jnz end_february
    mov 30 %c
    mov %b %a
    mod 100 %a
    jnz end_february
    mov 29 %c
    mov %b %a
    mod 400 %a
    jnz end_february
    mov 30 %c
end_february:
    mov *day %a
    inc %a
    mov %a *day
    sub %c %a
    jl loop
    inc %a
    mov %a *day

    mov *month %a
    inc %a
    mov %a *month
    sub 13 %a
    jl loop
    inc %a
    mov %a *month

    mov *year %a
    inc %a
    mov %a *year

    jmp loop
