import plotly.express as px
import pandas as pd

frame = pd.read_csv("./benchmark.csv")
fig = px.histogram(
    frame,
    x="Benchmark",
    y="Relative time",
    color="smoker",
    barmode="group",
    histfunc="avg",
    height=400,
)
fig.show()
