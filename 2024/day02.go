package main

import (
	"bufio"
	"fmt"
	"io"
	"iter"
	"os"
	"strconv"
	"strings"
)

func SkipOne[T any](s []T) iter.Seq[[]T] {
    return func(yield func([]T) bool) {
        for i := range s {
            var buffer []T
            for j, v := range s {
                if i != j {
                    buffer = append(buffer, v)
                }
            }
            if !yield(buffer) {
                return
            }
        }
    }
}

func Adjacents[T any](s []T) iter.Seq2[T, T] {
    return func(yield func(T, T) bool) {
        for i := 1; i < len(s); i++ {
            if !yield(s[i - 1], s[i]) {
                return
            }
        }
    }
}

func Sign(n int) int {
    if n < 0 {
        return -1
    } else if n > 0 {
        return 1
    }
    return 0
}

func IsSuperSafe(report []int) bool {
    var gaps []int
    for a, b := range Adjacents(report) {
        gaps = append(gaps, a - b)
    }

    for i := 1; i < len(gaps); i++ {
        if (Sign(gaps[i - 1]) != Sign(gaps[i])) {
            return false
        }
    }

    for _, v := range gaps {
        if v < 0 {
            v = -v
        }
        if v < 1 || v > 3 {
            return false
        }
    }
    return true
}

func IsSafe(report []int) bool {
    for r := range SkipOne(report) {
        if IsSuperSafe(r) {
            return true
        }
    }
    return false
}

func Part1(input [][]int) int {
    var count int
    for _, report := range input {
        if IsSuperSafe(report) {
            count++
        }
    }
    return count
}

func Part2(input [][]int) int {
    var count int
    for _, report := range input {
        if IsSafe(report) {
            count++
        }
    }
    return count
}

func Parse(r io.Reader) (reports [][]int) {
    s := bufio.NewScanner(r)

    for s.Scan() && s.Err() == nil {
        reports = append(reports, []int{})
        last := len(reports) - 1
        for _, digits := range strings.Split(s.Text(), " ") {
            num, _ := strconv.Atoi(digits)
            reports[last] = append(reports[last], num)
        }
    }

    return
}

func main() {
    file, err := os.Open("./input.txt")
    if err != nil {
        fmt.Println("error: failed to open input file")
        os.Exit(1)
    }
    defer file.Close()

    input := Parse(file)
    fmt.Printf("part 1: %d\n", Part1(input))
    fmt.Printf("part 2: %d\n", Part2(input))
}
