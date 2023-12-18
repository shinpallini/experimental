package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/gocarina/gocsv"
)

type Score struct {
	Id    int    `csv:"id"`
	Name  string `csv:"name"`
	Score int    `csv:"score"`
}

const (
	filename = "out.txt"
	count    = 10_000_000
)

func writeStringBuilder() {
	var builder strings.Builder
	builder.WriteString("id,name,score")
	for i := 0; i < count; i++ {
		builder.WriteString(fmt.Sprintf("%d,user%d,%d\n", i, i, i))
	}
	f, err := os.Create(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	bw := bufio.NewWriter(f)
	bw.WriteString(builder.String())
}

func writeByteSlice() {
	f, err := os.Create(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	bw := bufio.NewWriter(f)
	_, err = bw.WriteString("id,name,score\n")
	if err != nil {
		log.Fatal(err)
	}

	userStr := "user"
	var b []byte
	for i := 0; i < count; i++ {
		// strconv を使用して整数をバイトスライスに変換
		b = strconv.AppendInt(b[:0], int64(i), 10)
		b = append(b, ',')

		b = append(b, userStr...)
		b = strconv.AppendInt(b, int64(i), 10)
		b = append(b, ',')

		b = strconv.AppendInt(b, int64(i), 10)
		b = append(b, '\n')

		// バイトスライスを bufio.Writer に書き込む
		if _, err = bw.Write(b); err != nil {
			log.Fatal(err)
		}
	}

	if err = bw.Flush(); err != nil {
		log.Fatal(err)
	}
}

func writeBufioWriter() {
	f, err := os.Create(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	bw := bufio.NewWriter(f)
	_, err = bw.WriteString("id,name,score\n")
	if err != nil {
		log.Fatal(err)
	}

	userStr := "user"
	for i := 0; i < count; i++ {
		// 直接フォーマットして書き込む
		_, err = fmt.Fprintf(bw, "%d,%s%d,%d\n", i, userStr, i, i)
		if err != nil {
			log.Fatal(err)
		}
	}

	if err = bw.Flush(); err != nil {
		log.Fatal(err)
	}
}

func writeGocsv() {
	f, err := os.Create("output.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	data := make([]Score, 0)
	for i := 1; i < count; i++ {
		data = append(data, Score{Id: i, Name: "user" + strconv.Itoa(i), Score: i})
	}
	gocsv.MarshalFile(data, f)
}

// func writeFile() {
// 	f, err := os.Create("output.txt")
// 	if err != nil {
// 		log.Fatal(err)
// 	}
// 	defer f.Close()
// 	for i := 0; i < count; i++ {
// 		f.Write([]byte(fmt.Sprintf("%d,user%d,%d\n", i, i, i)))
// 	}
// }

func main() {
	f, err := os.Create("output.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	var builder strings.Builder
	builder.WriteString("id,name,score")
	for i := 0; i < 10000; i++ {
		builder.WriteString(fmt.Sprintf("%d,user%d,%d\n", i, i, i))
	}
	for i := 0; i < 10000; i++ {
		f.Write([]byte(fmt.Sprintf("%d,user%d,%d\n", i, i, i)))
	}
}
