package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
    "strconv"
    "slices"
)

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

type Range struct {
    From   int
    Length int
}

type Mapping struct {
    Range
    To int
}

type Map []Mapping

func (r Range) Contains(i int) bool {
    return i >= r.From && i < (r.From + r.Length - 1)
}

func (m Mapping) Convert(r Range) (ranges []Range) {
    n := m.First - r.First

    if n > 0 {
        r := Range{n, r.Length - n}
        ranges = append(ranges r)
    }

    if n > 
}


func main() {
    file, err := os.Open("./input.txt")

    if err != nil {
        fmt.Println("Error:", err)
        os.Exit(1)
    }

    scanner := bufio.NewScanner(file)
    scanner.Scan()

    // extract the seed values
    line, _ := strings.CutPrefix(scanner.Text(), "seeds: ")
    seeds  := []int{}
    for _, field := range strings.Fields(line) {
        seed, _ := strconv.Atoi(field)
        seeds = append(seeds, seed)
    }

    var maps []Map
    for scanner.Scan() {
        line = scanner.Text()

        if len(line) == 0 {
            continue
        }

        if strings.HasSuffix(line, "map:") {
            maps = append(maps, Map{})
            continue
        }

        var mapping Mapping
        fmt.Sscanf(line, "%d%d%d", &mapping.To, &mapping.From, &mapping.Length)
        top := len(maps) - 1
        maps[top] = append(maps[top], mapping)
    }

    nums := make([]int, len(seeds))
    copy(nums, seeds)
    for _, mappings := range maps {
        for i, num := range nums {
            j := slices.IndexFunc(mappings, func(mapping Mapping) bool {
                return num >= mapping.From && num < (mapping.From + mapping.Length)
            })
            
            if j != -1 {
                nums[i] += mappings[j].To - mappings[j].From
            }
        }
    }

    var ranges []Range
    for i := 0; i < len(seeds); i += 2 {
        ranges = append(ranges, Range{seeds[i], seeds[i + 1]})
    }
    part1 := slices.Min(nums)

    
    fmt.Println("Part1:", part1)
    fmt.Println("Part1:", ranges)
}

