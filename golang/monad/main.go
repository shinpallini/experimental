package main

import (
	"bufio"
	"fmt"
	"os"
)

// IOMonadは、入出力操作をカプセル化するための関数型です。
type IOMonad func() string

// doInputは、標準入力から文字列を読み込むIOMonadを生成します。
func doInput() IOMonad {
	return func() string {
		sc := bufio.NewScanner(os.Stdin)
		sc.Scan()
		return sc.Text()
	}
}

// doPrintは、与えられた文字列を標準出力に出力するIOMonadを生成します。
func doPrint(s string) IOMonad {
	return func() string {
		fmt.Println(s)
		return ""
	}
}

// Bindは、IOMonadを実行し、その結果を別のIOMonadにバインド（適用）する関数です。
func bind(m IOMonad, f func(string) IOMonad) IOMonad {
	input := m()
	result := f(input)
	return result
}

func main() {
	// Bindで連結することで、時間依存のない標準入力と標準出力を実現できます。
	action := bind(doInput(), doPrint)
	action()
}
