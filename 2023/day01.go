package main

import (
    "fmt"
    "os"
    "bufio"
    "unicode"
    "strconv"
    "strings"
    "regexp"
)

func firstIndexWhere[T any](slice []T , predicate func(T) bool) int {
    for i, v := range slice {
        if predicate(v) {
            return i
        }
    }
    return -1
}

func lastIndexWhere[T any](slice []T, predicate func(T) bool) int {
    for i := len(slice) - 1; i >= 0; i-- {
        if predicate(slice[i]) {
            return i
        }
    }
    return -1
}

func part01(lines []string) int {
    var sum int

    for _, line := range lines {
        runes := []rune(line)

        first := '0'
        if idx := firstIndexWhere(runes, unicode.IsDigit); idx != -1 {
           first = runes[idx]
        }

        last := '0'
        if idx := lastIndexWhere(runes, unicode.IsDigit); idx != -1 {
           last = runes[idx]
        }

        if num, err := strconv.Atoi(string(first) + string(last)); err != nil {
            fmt.Println("Error:", err)
            os.Exit(1)
        } else {
            sum += num
        }
    }

    return sum
}

func part02(lines []string) int {
    digits := map[string]byte{
        "one":   '1',
        "two":   '2',
        "three": '3',
        "four":  '4',
        "five":  '5',
        "six":   '6',
        "seven": '7',
        "eight": '8',
        "nine":  '9',
    }

    var keys []string
    for key := range digits {
        keys = append(keys, key)
    }
    
    pattern := strings.Join(keys, "|")
    regex := regexp.MustCompile(pattern)

    for i, line := range lines {
        bytes := []byte(line)
        
        for {
            if match := regex.Find(bytes); match != nil {
                match[0] = digits[string(match)]
                continue
            } 
            break
        }

        lines[i] = string(bytes)
    }

    return part01(lines)
}

func main() {
    file, err := os.Open("./input.txt")

    if err != nil {
        fmt.Println("Error:", err)
        os.Exit(1)
    }

    defer file.Close()

    var lines []string
    for scanner := bufio.NewScanner(file); scanner.Scan(); {
        lines = append(lines, scanner.Text())
    }

    fmt.Println("Part01:", part01(lines))
    fmt.Println("Part02:", part02(lines))
}

