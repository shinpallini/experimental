use encoding_rs::*;
use encoding_rs_io::DecodeReaderBytesBuilder;
use polars::prelude::*;
use std::fs::File;
use std::io::Cursor;
use std::io::{BufReader, Read};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Shift-JISエンコードされたCSVファイルを開く
    let file = File::open("収入・支出詳細_2024-01-01_2024-01-31.csv")?;
    let buf_reader = BufReader::new(file);

    // Shift-JISからUTF-8に変換するリーダーを作成
    let utf8_reader = DecodeReaderBytesBuilder::new()
        .encoding(Some(SHIFT_JIS))
        .build(buf_reader);

    // PolarsのCsvReaderに渡すためにCursorを使用
    let cursor = Cursor::new(utf8_reader.bytes().collect::<Result<Vec<_>, _>>()?);

    // Cursorを使ってDataFrameに読み込む
    let df = CsvReader::new(cursor)
        .infer_schema(None)
        .has_header(true)
        .finish()?;

    #[allow(deprecated)]
    let output_df = df
        .group_by(["大項目", "中項目", "内容"])?
        .select(["金額（円）"])
        .sum()?;

    // println!("{:?}", df);
    println!("{:?}", output_df.to_string());

    Ok(())
}
