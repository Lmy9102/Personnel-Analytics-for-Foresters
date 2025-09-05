import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
import warnings

warnings.filterwarnings('ignore')

def load_and_prepare_data():
    """Load and prepare the employee turnover data."""
    print("=== PARALLEL COORDINATES ANALYSIS: FACTORS INFLUENCING TURNOVER ===\n")
    
    # Load data
    df = pd.read_csv('employee_turnover_data.csv')
    
    # Convert turnover to binary
    df['Turnover_Binary'] = (df['Turnover'] == 'Yes').astype(int)
    
    print(f"Data loaded successfully!")
    print(f"Total employees: {len(df)}")
    print(f"Turnover rate: {df['Turnover_Binary'].mean()*100:.1f}%")
    print(f"Columns: {list(df.columns)}\n")
    
    return df

def create_parallel_coordinates_plot(df):
    """Create three separate parallel coordinates plots: All, Stayed Only, Left Only."""
    print("--- Creating Three Separate Interactive Parallel Coordinates Plots ---")
    
    # Prepare data for parallel coordinates
    df_parallel = df.copy()
    
    # Convert categorical variables to numeric codes
    df_parallel['Department_num'] = pd.Categorical(df_parallel['Department']).codes
    df_parallel['Salary_num'] = pd.Categorical(df_parallel['Salary_Level']).codes
    
    print(f"Stayed employees: {len(df_parallel[df_parallel['Turnover_Binary'] == 0])}")
    print(f"Left employees: {len(df_parallel[df_parallel['Turnover_Binary'] == 1])}")
    print(f"Total employees: {len(df_parallel)}")
    
    # Create separate dataframes
    df_stayed = df_parallel[df_parallel['Turnover_Binary'] == 0]
    df_left = df_parallel[df_parallel['Turnover_Binary'] == 1]
    
    # Create dimensions configuration
    dimensions_config = [
        dict(label='Age', range=[20, 60]),
        dict(label='Tenure (Years)', range=[0, 15]),
        dict(label='Job Satisfaction', range=[1, 5]),
        dict(label='Work Hours/Week', range=[30, 60]),
        dict(label='Training Hours/Year', range=[0, 50]),
        dict(label='Department', tickvals=[0, 1, 2, 3, 4], 
             ticktext=['HR', 'Finance', 'IT', 'Operations', 'Sales'], range=[0, 4]),
        dict(label='Salary Level', tickvals=[0, 1, 2], 
             ticktext=['Low', 'Medium', 'High'], range=[0, 2])
    ]
    
    # 1. Create "Show All" plot using single trace with color mapping
    fig_all = go.Figure()
    
    # Create combined dimensions with all data
    all_dims = [
        dict(label='Age', values=df_parallel['Age'], range=[20, 60]),
        dict(label='Tenure (Years)', values=df_parallel['Tenure_Years'], range=[0, 15]),
        dict(label='Job Satisfaction', values=df_parallel['Job_Satisfaction'], 
             tickvals=[1, 2, 3, 4, 5], range=[1, 5]),
        dict(label='Work Hours/Week', values=df_parallel['Avg_Hours_Week'], range=[30, 60]),
        dict(label='Training Hours/Year', values=df_parallel['Training_Hours_Year'], range=[0, 50]),
        dict(label='Department', values=df_parallel['Department_num'], 
             tickvals=[0, 1, 2, 3, 4], 
             ticktext=['HR', 'Finance', 'IT', 'Operations', 'Sales'], range=[0, 4]),
        dict(label='Salary Level', values=df_parallel['Salary_num'], 
             tickvals=[0, 1, 2], 
             ticktext=['Low', 'Medium', 'High'], range=[0, 2]),
        dict(label='Turnover Status', values=df_parallel['Turnover_Binary'],
             tickvals=[0, 1],
             ticktext=['Stayed', 'Left'],
             range=[0, 1])
    ]
    
    fig_all.add_trace(go.Parcoords(
        line=dict(color=df_parallel['Turnover_Binary'], 
                 colorscale=[[0, 'green'], [1, 'red']]),  # Green to Red
        dimensions=all_dims,
        name='All Employees'
    ))
    
    # 2. Create "Stayed Only" plot
    fig_stayed_only = go.Figure()
    fig_stayed_only.add_trace(go.Parcoords(
        line=dict(color='#006400'),
        dimensions=[
            dict(label='Age', values=df_stayed['Age'], range=[20, 60]),
            dict(label='Tenure (Years)', values=df_stayed['Tenure_Years'], range=[0, 15]),
            dict(label='Job Satisfaction', values=df_stayed['Job_Satisfaction'], 
                 tickvals=[1, 2, 3, 4, 5], range=[1, 5]),
            dict(label='Work Hours/Week', values=df_stayed['Avg_Hours_Week'], range=[30, 60]),
            dict(label='Training Hours/Year', values=df_stayed['Training_Hours_Year'], range=[0, 50]),
            dict(label='Department', values=df_stayed['Department_num'], 
                 tickvals=[0, 1, 2, 3, 4], 
                 ticktext=['HR', 'Finance', 'IT', 'Operations', 'Sales'], range=[0, 4]),
            dict(label='Salary Level', values=df_stayed['Salary_num'], 
                 tickvals=[0, 1, 2], 
                 ticktext=['Low', 'Medium', 'High'], range=[0, 2]),
            dict(label='Turnover Status', values=[0] * len(df_stayed),
                 tickvals=[0, 1],
                 ticktext=['Stayed', 'Left'],
                 range=[0, 1])
        ],
        name='Stayed'
    ))
    
    # 3. Create "Left Only" plot
    fig_left_only = go.Figure()
    fig_left_only.add_trace(go.Parcoords(
        line=dict(color='#D62728'),
        dimensions=[
            dict(label='Age', values=df_left['Age'], range=[20, 60]),
            dict(label='Tenure (Years)', values=df_left['Tenure_Years'], range=[0, 15]),
            dict(label='Job Satisfaction', values=df_left['Job_Satisfaction'], 
                 tickvals=[1, 2, 3, 4, 5], range=[1, 5]),
            dict(label='Work Hours/Week', values=df_left['Avg_Hours_Week'], range=[30, 60]),
            dict(label='Training Hours/Year', values=df_left['Training_Hours_Year'], range=[0, 50]),
            dict(label='Department', values=df_left['Department_num'], 
                 tickvals=[0, 1, 2, 3, 4], 
                 ticktext=['HR', 'Finance', 'IT', 'Operations', 'Sales'], range=[0, 4]),
            dict(label='Salary Level', values=df_left['Salary_num'], 
                 tickvals=[0, 1, 2], 
                 ticktext=['Low', 'Medium', 'High'], range=[0, 2]),
            dict(label='Turnover Status', values=[1] * len(df_left),
                 tickvals=[0, 1],
                 ticktext=['Stayed', 'Left'],
                 range=[0, 1])
        ],
        name='Left'
    ))
    
    # Common layout configuration
    layout_config = {
        'width': 1400,
        'height': 700,
        'font': dict(size=12),
        'title_font_size': 18,
        'margin': dict(l=80, r=450, t=150, b=50),
        'uirevision': "parcoords_v1"
    }
    
    # Update all figures with layout
    fig_all.update_layout(**layout_config, title="All Employees: Stayed (Green) + Left (Red)")
    fig_stayed_only.update_layout(**layout_config, title="Stayed Employees Only (Green)")
    fig_left_only.update_layout(**layout_config, title="Left Employees Only (Red)")
    
    # Add appropriate legends to each figure
    # All employees plot - show both colors
    fig_all.add_annotation(x=1.15, y=0.80, xref="paper", yref="paper",
                          text="<b>Employee Status</b>", showarrow=False, align="left", font=dict(size=12))
    fig_all.add_annotation(x=1.15, y=0.74, xref="paper", yref="paper",
                          text="<span style='color:green'>●</span> Stayed", showarrow=False, align="left", font=dict(size=12))
    fig_all.add_annotation(x=1.15, y=0.69, xref="paper", yref="paper",
                          text="<span style='color:red'>●</span> Left", showarrow=False, align="left", font=dict(size=12))
    
    # Stayed only plot - show only green
    fig_stayed_only.add_annotation(x=1.15, y=0.80, xref="paper", yref="paper",
                          text="<b>Employee Status</b>", showarrow=False, align="left", font=dict(size=12))
    fig_stayed_only.add_annotation(x=1.15, y=0.74, xref="paper", yref="paper",
                          text="<span style='color:#2E8B57'>●</span> Stayed", showarrow=False, align="left", font=dict(size=12))
    
    # Left only plot - show only red
    fig_left_only.add_annotation(x=1.15, y=0.80, xref="paper", yref="paper",
                          text="<b>Employee Status</b>", showarrow=False, align="left", font=dict(size=12))
    fig_left_only.add_annotation(x=1.15, y=0.69, xref="paper", yref="paper",
                          text="<span style='color:#DC143C'>●</span> Left", showarrow=False, align="left", font=dict(size=12))
    
    # Save all plots
    fig_all.write_html("parallel_coordinates_all.html")
    fig_stayed_only.write_html("parallel_coordinates_stayed.html")
    fig_left_only.write_html("parallel_coordinates_left.html")
    
    print(f"\nCreated three separate parallel coordinates plots:")
    print(f"  1. parallel_coordinates_all.html - All employees (both green and red)")
    print(f"     - Single trace: {len(fig_all.data[0].dimensions[0]['values'])} employees")
    print(f"     - Color mapping: 0= Green, 1=Dark Red")
    print(f"     - Turnover_Binary values: {df_parallel['Turnover_Binary'].value_counts().to_dict()}")
    print(f"  2. parallel_coordinates_stayed.html - Stayed employees only (green)")
    print(f"     - Stayed trace: {len(fig_stayed_only.data[0].dimensions[0]['values'])} employees")
    print(f"  3. parallel_coordinates_left.html - Left employees only (red)")
    print(f"     - Left trace: {len(fig_left_only.data[0].dimensions[0]['values'])} employees")
    
    return fig_all, fig_stayed_only, fig_left_only


def main():
    """Main function to run the parallel coordinates analysis."""
    try:
        # Load and prepare data
        df = load_and_prepare_data()
        
        # Create three separate parallel coordinates plots
        fig_all, fig_stayed, fig_left = create_parallel_coordinates_plot(df)
        
        print("\n=== PARALLEL COORDINATES ANALYSIS COMPLETE ===")
        print("\nKey Insights:")
        print("1. Interactive parallel coordinates show individual employee profiles across all factors")
        print("2. Three separate plots allow clear comparison between different employee groups")
        print("3. Red colors indicate higher turnover risk, green indicates lower risk")
        print("4. Use this analysis to identify high-risk employee profiles and intervention strategies")
        
        print(f"\nPlots saved as HTML files:")
        print(f"- parallel_coordinates_all.html (All employees)")
        print(f"- parallel_coordinates_stayed.html (Stayed employees only)")
        print(f"- parallel_coordinates_left.html (Left employees only)")
        
    except Exception as e:
        print(f"Error: {e}")
        print("Please ensure the 'employee_turnover_data.csv' file is in the current directory.")

if __name__ == "__main__":
    main()