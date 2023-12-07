package main

import (
    "fmt"
    "os"
    "bufio"
)

func main() {
    file, err := os.Open("./input.txt")

    if err != nil {
        fmt.Println("Error:", err)
        os.Exit(1)
    }

    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        fmt.Println(line)
    }
}

