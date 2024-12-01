cmake_minimum_required(VERSION 3.29)

file(STRINGS input.txt lines)

foreach(line IN LISTS lines)
    string(REGEX MATCH "([0-9]+) +([0-9]+)" _ "${line}")
    list(APPEND left "${CMAKE_MATCH_1}")
    list(APPEND right "${CMAKE_MATCH_2}")
endforeach()

function(part1 output left right)
    list(SORT left)
    list(SORT right)
    foreach(a b IN ZIP_LISTS left right)
        math(EXPR diff "${a}-${b}")
        string(REGEX REPLACE "-" "" diff ${diff}) # absolute value
        math(EXPR sum "${sum}+${diff}")
    endforeach()
    set(${output} ${sum} PARENT_SCOPE)
endfunction()

function(part2 output left right)
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

