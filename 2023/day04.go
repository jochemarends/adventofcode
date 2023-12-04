package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
    "slices"
)

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

func main() {
    file, err := os.Open("./input.txt")
    
    if err != nil {
        fmt.Println("Error:", err)
        os.Exit(1)
    }

    defer file.Close()


    scanner := bufio.NewScanner(file)
    var matches, pile []int
    var part1 int
    for scanner.Scan() {
        line := scanner.Text()
        line = line[strings.IndexRune(line, ':') + 1:]
        
        numbers := strings.Split(line, "|")
        winning := strings.Fields(numbers[0])
        playing := strings.Fields(numbers[1])
        matches = append(matches, 0)
        pile = append(pile, 1)

        var points int
        for _, number := range playing {
            if slices.Contains(winning, number) {
                points = max(1, points * 2)
                matches[len(matches) - 1]++
            }
        }
        part1 += points
    }

    part2 := 0
    for top := 0; top < len(pile); top++ {
        for offset := 1; offset <= matches[top]; offset++ {
            pile[top + offset] += pile[top]
        }
        part2 += pile[top]
    }

    fmt.Println("Part 1:", part1)
    fmt.Println("Part 2:", part2)
}
