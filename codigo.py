import pandas as pd


if __name__ == '__main__':
    df = pd.read_csv('~/Downloads/ea2_educacion.csv')
    df = df.drop(columns=['end_date', 'start_date', 'Incoming students (sum).']).copy()
    id_vars=['school_cycle', 'schooling_type', 'geography', 'oversight_type']
    value_vars=['holdback', 'groups', 'schools', 'teachers', 'students']
    df_grouped = df.groupby(by=id_vars)[value_vars].sum().reset_index()

    df_grouped.to_csv('~/Documents/Ultimo_Semestre/ea2/proyecto/datos.csv', index=False)
