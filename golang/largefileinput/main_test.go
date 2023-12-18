package main

import (
	"testing"
)

func BenchmarkWriteStringBuilder(b *testing.B) {
	writeStringBuilder()
}

// func BenchmarkWriteBufioWriter(b *testing.B) {
// 	writeBufioWriter()
// }

func BenchmarkByteSlice(b *testing.B) {
	writeByteSlice()
}

func BenchmarkGocsv(b *testing.B) {
	writeGocsv()
}

// func BenchmarkWriteFile(b *testing.B) {
// 	writeFile()
// }
