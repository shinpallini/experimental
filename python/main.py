import pandas as pd
from openpyxl.utils import get_column_letter
from openpyxl import Workbook

# CSVファイルのパスを指定
file_path = '収入・支出詳細_2024-01-01_2024-01-31.csv'

# CSVファイルを読み込む
df = pd.read_csv(file_path, encoding='shift_jis')

# 「計算対象」カラムが1である行のみを含むデータフレームにフィルタリング
df_filtered = df[df["計算対象"] == 1][["大項目", "中項目", "内容", "金額（円）"]]

# 大項目ごとの合計行と全体合計行を追加する関数
def add_subtotals_and_grand_total(df):
    result_df = pd.DataFrame()
    grand_total = 0
    for name, group in df.groupby("大項目"):
        subtotal = group["金額（円）"].sum()
        grand_total += subtotal
        # グループデータ
        result_df = pd.concat([result_df, group], ignore_index=True)
        # サブトータル行
        result_df = pd.concat([result_df, pd.DataFrame([{ "大項目": name, "中項目": "合計", "内容": "", "金額（円）": subtotal}])], ignore_index=True)
    # 全体合計行
    result_df = pd.concat([result_df, pd.DataFrame([{ "大項目": "全体合計", "中項目": "", "内容": "", "金額（円）": grand_total}])], ignore_index=True)
    return result_df

# Excelファイルに書き込み後、カラム幅を自動調整する関数
def auto_adjust_column_width(writer):
    for sheet_name in writer.sheets:
        worksheet = writer.sheets[sheet_name]
        for column_cells in worksheet.columns:
            max_length = max(len(str(cell.value)) if cell.value is not None else 0 for cell in column_cells)
            adjusted_width = (max_length + 7) * (1 + (2 * max_length / 100)) * 1.2
            worksheet.column_dimensions[get_column_letter(column_cells[0].column)].width = adjusted_width


# データ処理
income_df = df_filtered[df_filtered["金額（円）"] > 0]
expenses_df = df_filtered[df_filtered["金額（円）"] < 0]
income_with_totals = add_subtotals_and_grand_total(income_df)
expenses_with_totals = add_subtotals_and_grand_total(expenses_df)

# Excelファイルに書き込む関数、大項目間に空白行を挿入
def write_data_with_blank_rows(writer, sheet_name, data):
    start_row = 0
    for _, group in data.groupby("大項目", sort=False):
        group.to_excel(writer, sheet_name=sheet_name, startrow=start_row, index=False)
        start_row += len(group) + 2  # 空白行を挿入

# Excelファイルに結果を書き込む
with pd.ExcelWriter('集計結果_全体版.xlsx', engine='openpyxl') as writer:
    write_data_with_blank_rows(writer, '収入', income_with_totals)
    write_data_with_blank_rows(writer, '支出', expenses_with_totals)
    # カラム幅の自動調整を忘れずに実行
    auto_adjust_column_width(writer)
