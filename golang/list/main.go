package main

import (
	"fmt"
	"strconv"
)

func Map[T any, U any](f func(T) U, l []T) []U {
	newList := make([]U, len(l))
	for i, v := range l {
		newList[i] = f(v)
	}
	return newList
}

func main() {
	l := []int{1, 2, 3, 4, 5}
	newList := Map(func(x int) string { return "value:" + strconv.Itoa(x+1) }, l)
	fmt.Println(newList)
}
