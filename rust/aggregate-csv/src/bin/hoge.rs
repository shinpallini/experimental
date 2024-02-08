use csv::ReaderBuilder;
use encoding_rs::*;
use encoding_rs_io::DecodeReaderBytesBuilder;
use std::env;
use std::fs::File;
use std::io::{self, BufReader};

fn main() -> io::Result<()> {
    // CSVファイルを開く
    let path = env::current_dir()?;
    println!("{}", path.to_str().unwrap());
    let file = File::open("収入・支出詳細_2024-01-01_2024-01-31.csv")?;

    // ファイルリーダーを作成し、Shift-JISからUTF-8にデコードする
    let transcoded_reader = BufReader::new(
        DecodeReaderBytesBuilder::new()
            .encoding(Some(SHIFT_JIS))
            .build(file),
    );

    // CSVリーダーを作成し、UTF-8として処理する
    let mut rdr = ReaderBuilder::new().from_reader(transcoded_reader);

    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }

    Ok(())
}
