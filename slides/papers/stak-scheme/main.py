import pandas as pd
import plotly.express as px

color_map = {
    "mstak": "#1f77b4",
    "stak": "#2980b9",
    "mstak (embed)": "#3498db",
    "stak (embed)": "#5dade2",
    "tr7i": "limegreen",
    "gsi": "orange",
    "chibi": "mediumpurple",
    "gosh": "tomato",
}

df = pd.read_csv("./benchmark.csv")
df_melted = df.melt(
    id_vars="Benchmark", var_name="Implementation", value_name="Relative time"
)

# Plot
fig = px.bar(
    df_melted,
    x="Benchmark",
    y="Relative time",
    color="Implementation",
    color_discrete_map=color_map,
    barmode="group",
)

fig.update_layout(
    legend_title="Implementation", template="plotly_dark", yaxis_range=[0, 2]
)

fig.add_hline(y=1, line_dash="dash", line_color="white")

fig.write_image("benchmark.svg", scale=2)
