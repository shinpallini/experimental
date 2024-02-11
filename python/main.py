import pandas as pd
from openpyxl.utils import get_column_letter

# CSVファイルのパスを指定
file_path = '収入・支出詳細_2024-01-01_2024-01-31.csv'

# CSVファイルを読み込む
df = pd.read_csv(file_path, encoding='shift_jis')

# 「計算対象」カラムが1である行のみを含むデータフレームにフィルタリング
df_filtered = df[df["計算対象"] == 1][["大項目", "中項目", "内容", "金額（円）"]]

# 大項目ごとの合計行と全体合計行を追加する関数
def add_subtotals_and_grand_total(df, index, values):
    result_df = pd.DataFrame()
    grand_total = 0
    for name, group in df.groupby(index[0]):
        subtotal = group[values].sum()
        grand_total += subtotal
        # グループとサブトータル行を追加（修正箇所）
        subtotal_df = pd.DataFrame({index[0]: [name], index[1]: ["合計"], index[2]: [""], values: [subtotal]})
        result_df = pd.concat([result_df, group, subtotal_df], ignore_index=True)
    
    # 全体合計行を追加（修正箇所）
    grand_total_df = pd.DataFrame({index[0]: ["全体合計"], index[1]: [""], index[2]: [""], values: [grand_total]})
    result_df = pd.concat([result_df, grand_total_df], ignore_index=True)
    
    # マルチインデックスを設定
    result_df.set_index(index, inplace=True)
    return result_df

# データ処理
index = ["大項目", "中項目", "内容"]
values = "金額（円）"
income_df = df_filtered[df_filtered[values] > 0]
expenses_df = df_filtered[df_filtered[values] < 0]
income_with_totals = add_subtotals_and_grand_total(income_df, index, values)
expenses_with_totals = add_subtotals_and_grand_total(expenses_df, index, values)

# Excelファイルに書き込み後、カラム幅を自動調整する関数
def auto_adjust_column_width(writer):
    for sheet_name in writer.sheets:
        worksheet = writer.sheets[sheet_name]
        for column_cells in worksheet.columns:
            max_length = max(len(str(cell.value)) if cell.value is not None else 0 for cell in column_cells)
            adjusted_width = (max_length + 7) * (1 + (2 * max_length / 100)) * 1.2
            worksheet.column_dimensions[get_column_letter(column_cells[0].column)].width = adjusted_width

# Excelファイルに結果を書き込む
with pd.ExcelWriter('集計結果_全体版.xlsx', engine='openpyxl') as writer:
    income_with_totals.to_excel(writer, sheet_name='収入', index=True)
    expenses_with_totals.to_excel(writer, sheet_name='支出', index=True)

    # カラム幅の自動調整
    auto_adjust_column_width(writer)
