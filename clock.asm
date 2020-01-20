second  = 1024
minute  = 1025
hour    = 1026
day     = 1027
month   = 1028
year    = 1029
sync    = 1030
elapsed = 1031

loop:
    ; Synchronisation
    mov *sync %a
    jz end_sync
    mov *elapsed %a
    jz loop
    dec %a
    mov %a *elapsed
end_sync:

    ; Seconds
    mov *second %a
    inc %a
    mov %a *second
    sub 60 %a
    jl loop
    mov %a *second

    ; Minutes
    mov *minute %a
    inc %a
    mov %a *minute
    sub 60 %a
    jl loop
    mov %a *minute

    ; Hours
    mov *hour %a
    inc %a
    mov %a *hour
    sub 24 %a
    jl loop
    mov %a *hour

    ; Store number of days in the month into %c
    mov *month %a
    sub 2 %a
    je february
    ; Not February: the number of days is 30 + ((month & 8 >> 3) ^ (month & 1))
    add 2 %a
    mov %a %c
    and 8 %a
    test %a %a
    and 1 %c
    xor %a %c
    add 30 %c
    jmp end_february
february:
    ; February: 29 days if leap year, 28 otherwise
    mov 28 %c
    mov *year %b
    mov %b %a
    mod 4 %a
    jnz end_february
    mov 29 %c
    mov %b %a
    mod 100 %a
    jnz end_february
    mov 28 %c
    mov %b %a
    mod 400 %a
    jnz end_february
    mov 29 %c
end_february:

    ; Days
    mov *day %a
    inc %a
    mov %a *day
    sub %c %a
    jle loop
    mov %a *day

    ; Months
    mov *month %a
    inc %a
    mov %a *month
    sub 12 %a
    jle loop
    mov %a *month

    ; Years
    mov *year %a
    inc %a
    mov %a *year

    jmp loop
