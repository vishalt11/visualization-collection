# .\temp\Scripts\Activate.ps1 (powershell)

import pandas as pd

data = {
    'Name': ['Alice', 'Bob', 'Alice', 'Bob', 'Alice', 'Charlie'],
    'Product': ['Muffin', 'Coffee', 'Coffee', 'Muffin', 'Bagel', 'Muffin'],
    'Sales': [5, 3, 2, 4, 6, 7],
    'Date': pd.to_datetime([
        '2023-01-01', '2023-01-01',
        '2023-01-02', '2023-01-02',
        '2023-01-03', '2023-01-03'
    ])
}

df = pd.DataFrame(data)

grouped = df.groupby(['Name', 'Product'])['Sales'].sum().reset_index()

daily_sales = df.groupby('Date')['Sales'].sum().reset_index()

high_sales = df[df['Sales'] > 5]

paleo_df = pd.read_csv('d:\\UE_Applied_Sciences\\Projects\\data\\creta_ornithischia.csv', skiprows=15, header=0)

