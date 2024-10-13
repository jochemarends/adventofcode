package main

import (
	"bufio"
	"cmp"
	"fmt"
	"iter"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

type Range struct {
    Start  int
    Length int
}

func (r Range) End() int {
    return r.Start + r.Length - 1
}

func (r Range) Values() iter.Seq[int] {
    return func(yield func(int) bool) {
        for i := 0; i < r.Length; i++ {
            if !yield(r.Start + i) {
                return
            }
        }
    }
}

func (r Range) Contains(num int) bool {
    return num >= r.Start && num < (r.Start + r.Length)
}

func (r Range) ContainsRange(other Range) bool {
    return r.Contains(other.Start) && r.Contains(other.End())
}

func (r Range) Intersects(other Range) bool {
    return r.Contains(other.Start) || r.Contains(other.End())
}

func (r Range) Intersection(other Range) Range {
    var rng Range

    if r.Intersects(other) {
        rng.Start = max(r.Start, other.Start)
        end := min(r.End(), other.End())
        rng.Length = end - rng.Start
    }
     
    return rng
}

func (r Range) Exclude(other Range) (result []Range) {
    if !r.Intersects(other) {
        result = append(result, r)
        return
    }

    if other.ContainsRange(r) {
       return
    }

    if other.Start < r.Start {
        result = append(result, Range{Start: r.Start, Length: other.Start - r.Start})
    }

    if other.End() < r.End() {
        result = append(result, Range{Start: other.End(), Length: r.End() - other.End()})
    }

    return
}

type Mapping struct {
    Range
    To int
}

type Map struct {
    mappings []Mapping
}

func (m *Map) Convert (num int) int {
    index := slices.IndexFunc(m.mappings, func(mapping Mapping) bool {
        return mapping.Contains(num)
    })

    if index != -1 {
        mapping := m.mappings[index]
        num += (mapping.To - mapping.Start)
    }

    return num
}

func (m *Map) ConvertRange(r Range) (res []Range) {
    index := slices.IndexFunc(m.mappings, func(mapping Mapping) bool {
        return r.Contains(mapping.Range.Start) || r.Contains(mapping.Range.End())
    })

    // if one intersection was found
    if index != -1 {
        mapping := m.mappings[index]
        in := mapping.Intersection(r)

        for _, ex := range r.Exclude(in) {
            for _, r := range m.ConvertRange(ex){
                res = append(res, r)
            }
        }
    }

    return
}

type Almanac struct {
    seeds []int
    maps  []Map
}

func (a *Almanac) Solve(num int) int {
    for _, mapper := range a.maps {
        num = mapper.Convert(num)
    }
    return num
}

func (a *Almanac) SolveRange(r Range) (solved []Range) {
    solved = append(solved, r)

    for _, mapper := range a.maps {
        var mapped []Range

        for _, r := range solved {
            for _, converted := range mapper.ConvertRange(r) {
                mapped = append(mapped, converted)
            }
        }

        solved = mapped
    }

    return
}

func parseAlmanac(scanner *bufio.Scanner) (almanac *Almanac, err error) {
    scanner.Scan()
    almanac = &Almanac{}

    // extract the seed values
    line, found := strings.CutPrefix(scanner.Text(), "seeds: ")
    if !found {
        err = fmt.Errorf("the seeds are missing from the almanac!")
        return
    }
    
    for _, field := range strings.Fields(line) {
        var seed int
        seed, err = strconv.Atoi(field)

        if err != nil {
            return
        }

        almanac.seeds = append(almanac.seeds, seed)
    }

    for scanner.Scan() {
        line = scanner.Text()

        if len(line) == 0 {
            continue
        }

        if strings.HasSuffix(line, "map:") {
            almanac.maps = append(almanac.maps, Map{})
            continue
        }

        var mapping Mapping
        _, err = fmt.Sscanf(line, "%d%d%d", &mapping.To, &mapping.Start, &mapping.Length)
        
        if err != nil {
            return
        }

        if len(almanac.maps) == 0 {
            err = fmt.Errorf("expected a map header before an entry")
            return
        } else {
            top := len(almanac.maps) - 1
            almanac.maps[top].mappings = append(almanac.maps[top].mappings, mapping)
        }
    }

    return
}

func part1(almanac *Almanac) int {
    nums := make([]int, len(almanac.seeds))
    copy(nums, almanac.seeds)

    for i, num := range nums {
        nums[i] = almanac.Solve(num)
    }
    
    return slices.Min(nums)
}

func part2(almanac *Almanac) int {
    var seeds []Range
    var solutions []Range

    for i := 0; i < len(almanac.seeds); i += 2 {
        r := Range{almanac.seeds[i], almanac.seeds[i + 1]}
        seeds = append(seeds, r)
    }

    for _, r := range seeds {
        for _, solved := range almanac.SolveRange(r) {
            solutions = append(solutions, solved)
        }
    }
    
    solution := slices.MinFunc(solutions, func(a, b Range) int {
        return cmp.Compare(a.Start, b.Start)
    })

    return solution.Start
}

func main() {
    file, err := os.Open("./input.txt")

    if err != nil {
        log.Fatalln(err)
    }

    scanner := bufio.NewScanner(file)
    almanac, err := parseAlmanac(scanner)

    if err != nil {
        log.Fatalln(err)
    }
    
    fmt.Println("Part1:", part1(almanac))
    fmt.Println("Part2:", part2(almanac))
}

