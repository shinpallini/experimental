import pandas as pd

# CSVファイルのパスを指定
file_path = '収入・支出詳細_2024-01-01_2024-01-31.csv'

# CSVファイルを読み込む
df = pd.read_csv(file_path, encoding='shift_jis')

index = ["大項目", "中項目", "内容"]
value_column = "金額（円）"
df = df.groupby(index, as_index=False)[value_column].sum().sort_index(ascending=False).set_index(index)

# 読み込んだデータを表示
print(df)

# 集計結果を出力
df.to_excel('集計結果.xlsx', sheet_name='集計結果')

