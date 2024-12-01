cmake_minimum_required(VERSION 3.20)

file(STRINGS input.txt lines)

foreach(line IN LISTS lines)
    string(REGEX REPLACE " +" ";" pair "${line}")
    if(NOT pair STREQUAL "")
        list(GET pair 0 first)
        list(GET pair 1 second)
        list(APPEND left ${first})
        list(APPEND right ${second})
    endif()
endforeach()

function(part1 output left right)
    set(sum 0)
    list(SORT left)
    list(SORT right)
    foreach(a b IN ZIP_LISTS left right)
        math(EXPR diff "${a}-${b}")
        if(${diff} LESS 0)
            math(EXPR diff "${diff}*-1")
        endif()
        math(EXPR sum "${sum}+${diff}")
    endforeach()
    set(${output} ${sum} PARENT_SCOPE)
endfunction()

function(part2 output left right)
    set(sum 0)
    foreach(elem IN LISTS left)
        string(REGEX MATCHALL "${elem}" matches "${right}")
        list(LENGTH matches freq)
        math(EXPR sum "${sum}+(${elem}*${freq})")
    endforeach()
    set(${output} ${sum} PARENT_SCOPE)
endfunction()

part1(result1 "${left}" "${right}")
part2(result2 "${left}" "${right}")
message("part 1: ${result1}\npart 2: ${result2}")

