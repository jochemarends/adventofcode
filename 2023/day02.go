package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

func main() {
    file, err := os.Open("./input.txt")

    if err != nil {
        fmt.Println("Error:", err)
        os.Exit(1)
    }

    defer file.Close()

    cubes := map[string]int{
        "red":   12,
        "green": 13,
        "blue":  14,
    }

    isPossible := func(game map[string]int) bool {
        for color, count := range cubes {
            if game[color] > count {
                return false
            }
        }
        return true
    }

    part01, part02 := 0, 0
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        var id int
        fmt.Sscanf(line, "Game %d", &id)
        line = line[strings.IndexByte(line, ':')+2:]

        maxCubes := make(map[string]int)
        for _, set := range strings.Split(line, "; ") {
            for _, str := range strings.Split(set, ", ") {
                var count int
                var color string
                fmt.Sscanf(str, "%d%s", &count, &color)

                if count > maxCubes[color] {
                    maxCubes[color] = count
                }
            }
        }

        if isPossible(maxCubes) {
            part01 += id
        }

        power := 1
        for _, count := range maxCubes {
            power *= count
        }
        part02 += power
    }

    fmt.Println("Part01:", part01)
    fmt.Println("Part02:", part02)

}

