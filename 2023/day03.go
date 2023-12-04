package main

import (
    "fmt"
    "os"
    "bufio"
    "regexp"
    "unicode"
    "strconv"
)

func min(a, b int) int {
    if a < b {
        return a;
    }
    return b;
}

func max(a, b int) int {
    if a > b {
        return a;
    }
    return b;
}

type Point struct {
    X int
    Y int
}

type Region struct {
    Point
    Count int
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

    text := func(r Region) string {
        return schematic[r.Y][r.X:r.X + r.Count]
    }

    number := func(r Region) int {
        num, _ := strconv.Atoi(text(r))
        return num
    }

    pattern := `\d+`
    regex := regexp.MustCompile(pattern)

    var part01 int
    var partNumbers []Region
    asterisks := make(map[Point][]Region)
    for y, line := range schematic {
        adjacentPoints := func(r Region) (points []Point) {
            for x := max(r.X - 1, 0); x < min(r.X + r.Count + 1, len(line)); x++ {
                for y := max(r.Y - 1, 0); y < min(r.Y + 2, len(schematic)); y++ {
                    points = append(points, Point{x, y})
                }
            }
            return
        }

        for _, indices := range regex.FindAllStringIndex(line, -1) {
            region := Region{Point{indices[0], y}, indices[1] - indices[0]}
            isPartNumber := false
            for _, p := range adjacentPoints(region) {
                r := rune(schematic[p.Y][p.X])
                if r == '*' {
                    asterisks[p] = append(asterisks[p], region)
                }
                if !isPartNumber && !unicode.IsDigit(r) && r != '.' {
                    partNumbers = append(partNumbers, region)
                    part01 += number(region)
                    isPartNumber = true
                }
            }
        }
    }

    var part02 int
    for _, regions := range asterisks {
        if len(regions) == 2 {
            part02 += number(regions[0]) * number(regions[1])
        }
    }

    fmt.Println("Part01:", part01)
    fmt.Println("Part02:", part02)
}

