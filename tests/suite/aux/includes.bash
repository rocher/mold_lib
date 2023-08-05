#!/bin/bash

function loop_includes {
    # $1 -> First
    # $2 -> Last
    First=$(printf %02d $1)
    Last=$(printf %02d $2)
    Out_File="suite/molt/lorem-ipsum-from-${First}-to-${Last}.molt"

    for i in $(seq $1 $2); do
        N=$(printf %02d $i)
        Inc_File="suite/molt/lorem-ipsum-${N}.molt"
        if [[ "$i" = "$2" ]]; then
            echo "{{ include:${Inc_File} }}" >> ${Out_File}
        else
            echo -e "{{ include:${Inc_File} }}\n" >> ${Out_File}
        fi
    done
}

loop_includes  0 99

loop_includes  0  9
loop_includes 10 19
loop_includes 20 29
loop_includes 30 39
loop_includes 40 49
loop_includes 50 59
loop_includes 60 69
loop_includes 70 79
loop_includes 80 89
loop_includes 90 99

loop_includes  0 49
loop_includes 50 99
