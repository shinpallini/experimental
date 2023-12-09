package main

import (
	"fmt"

	"golang.org/x/exp/maps"
	"golang.org/x/exp/slices"
)

func main() {
	l := []int{1, 2, 3, 4, 5}
	b := slices.Contains(l, 2)
	fmt.Println(b)
	m := map[string]int{
		"one":   1,
		"two":   2,
		"three": 3,
	}
	maps.DeleteFunc(m, func(k string, v int) bool {
		return v == 2
	})
	fmt.Println(m)
}
