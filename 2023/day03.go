package main

import (
    "fmt"
    "os"
    "bufio"
    "regexp"
    "unicode"
    "strconv"
)

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func partOne(schematic []string) int {
    pattern := "[0-9]+"
    regex := regexp.MustCompile(pattern)
    sum := 0

    for y, line := range schematic {
        getAdjacentBytes := func(x, y, count int) (bytes []byte) {
            for i := max(x - 1, 0); i < min(x + count + 1, len(line)); i++ {
                for j := max(y - 1, 0); j < min(y + 2, len(schematic)); j++ {
                    if j != y || i < x || i >= x + count {
                        bytes = append(bytes, schematic[j][i])
                    }
                }
            }
            return
        }

        for _, indices := range regex.FindAllStringIndex(line, -1) {
            x, count := indices[0], indices[1] - indices[0]
            for _, b := range getAdjacentBytes(x, y, count) {
                if !unicode.IsDigit(rune(b)) && b != '.' {
                    str := line[x: x + count]
                    num, _ := strconv.Atoi(str)
                    sum += num
                    break
                }
            }
        }
    }

    return sum
}

func main() {
    file, err := os.Open("./input.txt")

    if err != nil {
        fmt.Println("Error:", err)
    }

    defer file.Close()

    var schematic []string
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        schematic = append(schematic, scanner.Text())
    }

    fmt.Println("Part01:", partOne(schematic))
}

