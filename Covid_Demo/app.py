import warnings
import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
import plotly.express as px

warnings.filterwarnings("ignore")

st.set_page_config(
    page_title="COVID-19 Bangladesh Forecasting",
    page_icon="🦠",
    layout="wide",
)

# ── Data ─────────────────────────────────────────────────────────────────────

_BASE = "https://raw.githubusercontent.com/Sahadat97/COVID-19-Bangladesh-EDA-and-Daily-Cases-Prediction/main/"
DATA_URL = _BASE + "covid19_bangladesh_cleaned.csv"
PRED_URL = _BASE + "predictions.csv"

@st.cache_data(show_spinner="Loading data…")
def load_data():
    df = pd.read_csv(DATA_URL)
    df = df.rename(columns={"final_date": "date"})
    df["date"] = pd.to_datetime(df["date"])
    df = df.sort_values("date").reset_index(drop=True)
    return df

@st.cache_data(show_spinner="Loading predictions…")
def load_predictions():
    pred = pd.read_csv(PRED_URL)
    pred["date"] = pd.to_datetime(pred["date"])
    return pred




# ── Layout ────────────────────────────────────────────────────────────────────

st.title("🦠 COVID-19 Bangladesh — EDA & Forecasting")
st.markdown(
    "Interactive demo of the "
    "[GitHub project](https://github.com/Sahadat97/COVID-19-Bangladesh-EDA-and-Daily-Cases-Prediction) "
    "by **Mohammad Sahadat Hossain**. "
    "Data loaded from the cleaned dataset used in the notebook."
)

df   = load_data()
pred = load_predictions()

tab_eda, tab_forecast, tab_compare = st.tabs(
    ["📊 Exploratory Analysis", "🔮 Forecast", "📈 Model Comparison"]
)

# ── Tab 1 · EDA ───────────────────────────────────────────────────────────────
with tab_eda:
    col1, col2, col3, col4 = st.columns(4)
    col1.metric("Total Cases",     f"{df['total_cases'].iloc[-1]:,}")
    col2.metric("Total Deaths",    f"{df['total_deaths'].iloc[-1]:,}")
    col3.metric("Total Recovered", f"{df['total_recovered'].iloc[-1]:,}")
    col4.metric("Days of Data",    str(len(df)))

    st.divider()

    # Cumulative curves
    fig1 = go.Figure()
    fig1.add_trace(go.Scatter(x=df["date"], y=df["total_cases"],
                              name="Total Cases", line=dict(color="#EF553B")))
    fig1.add_trace(go.Scatter(x=df["date"], y=df["total_recovered"],
                              name="Total Recovered", line=dict(color="#00CC96")))
    fig1.add_trace(go.Scatter(x=df["date"], y=df["total_deaths"],
                              name="Total Deaths", line=dict(color="#636EFA")))
    fig1.update_layout(
        title="Cumulative COVID-19 Statistics — Bangladesh",
        xaxis_title="Date", yaxis_title="Count", hovermode="x unified",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
    )
    st.plotly_chart(fig1, use_container_width=True)

    # Daily new cases + 7-day rolling average
    df["rolling_7"] = df["new_cases"].rolling(7).mean()
    fig2 = go.Figure()
    fig2.add_trace(go.Bar(x=df["date"], y=df["new_cases"],
                          name="Daily New Cases", marker_color="rgba(239,85,59,0.4)"))
    fig2.add_trace(go.Scatter(x=df["date"], y=df["rolling_7"],
                              name="7-day Rolling Avg",
                              line=dict(color="#EF553B", width=2)))
    fig2.update_layout(
        title="Daily New Cases & 7-Day Rolling Average",
        xaxis_title="Date", yaxis_title="New Cases",
        hovermode="x unified", barmode="overlay",
    )
    st.plotly_chart(fig2, use_container_width=True)

    # Case fatality rate
    df["cfr"] = df["total_deaths"] / df["total_cases"].replace(0, np.nan) * 100
    fig3 = px.line(df, x="date", y="cfr",
                   title="Case Fatality Rate (%) Over Time",
                   labels={"cfr": "CFR (%)", "date": "Date"},
                   color_discrete_sequence=["#636EFA"])
    st.plotly_chart(fig3, use_container_width=True)

    # New deaths
    fig4 = go.Figure()
    fig4.add_trace(go.Bar(x=df["date"], y=df["new_deaths"],
                          name="Daily New Deaths", marker_color="rgba(99,110,250,0.5)"))
    fig4.add_trace(go.Scatter(x=df["date"], y=df["new_deaths"].rolling(7).mean(),
                              name="7-day Rolling Avg",
                              line=dict(color="#636EFA", width=2)))
    fig4.update_layout(title="Daily New Deaths", xaxis_title="Date",
                       yaxis_title="Deaths", hovermode="x unified", barmode="overlay")
    st.plotly_chart(fig4, use_container_width=True)


# ── Tab 2 · Forecast ──────────────────────────────────────────────────────────
with tab_forecast:
    st.subheader("Interactive Time-Series Forecasting")

    FC_TEST_START = pd.Timestamp("2021-01-01")
    FC_TEST_END   = pd.Timestamp("2021-02-12")

    # Compute RMSE / MAE from the real notebook predictions file
    def _metrics(col):
        r = float(np.sqrt(np.mean((pred[col] - pred["actual"]) ** 2)))
        m = float(np.mean(np.abs(pred[col] - pred["actual"])))
        return r, m

    _MODEL_META = {
        "ARIMA":      {"col": "arima_pred",   "color": "#EF553B", "live": False},
        "H2O AutoML": {"col": "h2o_pred",     "color": "#636EFA", "live": False},
        "Prophet":    {"col": "prophet_pred",  "color": "#FFA15A", "live": False},
        "LSTM":       {"col": "lstm_pred",     "color": "#00CC96", "live": False},
    }

    c1, c2 = st.columns([1, 3])

    with c1:
        model_choice = st.selectbox(
            "Model",
            ["ARIMA", "H2O AutoML", "Prophet", "LSTM"],
        )
        meta = _MODEL_META[model_choice]

        if model_choice in ("LSTM", "H2O AutoML"):
            st.info(
                f"**{model_choice}** requires GPU training. "
                "Showing notebook results on the Jan 1 – Feb 12, 2021 test window."
            )
        else:
            st.info(
                f"Showing **{model_choice}** notebook results on the "
                "Jan 1 – Feb 12, 2021 test window."
            )

        run_btn = st.button("▶ Run Forecast", type="primary", use_container_width=True)

    with c2:
        if run_btn:
            mask_train = df["date"] < FC_TEST_START
            train_vals = df.loc[mask_train, "new_cases"].values.astype(float)

            rmse, mae = _metrics(meta["col"])

            fig = go.Figure()
            fig.add_trace(go.Scatter(
                x=df.loc[mask_train, "date"], y=train_vals,
                name="Training data", line=dict(color="#aaa", width=1),
            ))
            fig.add_trace(go.Scatter(
                x=pred["date"], y=pred["actual"],
                name="Actual (test window)", line=dict(color="#fff", width=2.5),
            ))
            fig.add_trace(go.Scatter(
                x=pred["date"], y=pred[meta["col"]],
                name=f"{model_choice} (notebook)",
                line=dict(color=meta["color"], dash="dash", width=2),
            ))
            fig.add_vrect(
                x0=FC_TEST_START, x1=FC_TEST_END,
                fillcolor="rgba(239,85,59,0.07)", line_width=0,
                annotation_text="Test window", annotation_position="top left",
            )
            fig.update_layout(
                title=f"{model_choice} — New Cases (Jan 1 – Feb 12, 2021)",
                xaxis_title="Date", yaxis_title="New Cases",
                hovermode="x unified",
            )
            st.plotly_chart(fig, use_container_width=True)
            col_r, col_m = st.columns(2)
            col_r.metric("RMSE", f"{rmse:,.2f}")
            col_m.metric("MAE",  f"{mae:,.2f}")
        else:
            st.info("Select a model on the left and click **▶ Run Forecast**.")


# ── Tab 3 · Model Comparison ──────────────────────────────────────────────────
with tab_compare:
    st.subheader("Model Comparison — Test Window: Jan 1 – Feb 12, 2021")
    st.markdown(
        "Exact results from the notebook — all metrics loaded from `predictions.csv`."
    )

    # ── Exact notebook results ─────────────────────────────────────────────────
    NOTEBOOK_RESULTS = [
        {"Rank": 1, "Model": "LSTM",            "RMSE": 158.00, "MAE": 133.59, "RMSE_Improvement_%": 69.2},
        {"Rank": 2, "Model": "H2O AutoML",       "RMSE": 200.47, "MAE": 157.40, "RMSE_Improvement_%": 61.0},
        {"Rank": 3, "Model": "Prophet",          "RMSE": 208.90, "MAE": 183.71, "RMSE_Improvement_%": 59.3},
        {"Rank": 4, "Model": "ARIMA (baseline)", "RMSE": 513.63, "MAE": 465.53, "RMSE_Improvement_%":  0.0},
    ]
    nb_df = pd.DataFrame(NOTEBOOK_RESULTS).set_index("Rank")

    ARIMA_BASELINE = 513.63
    TEST_START     = pd.Timestamp("2021-01-01")
    TEST_END       = pd.Timestamp("2021-02-12")

    # ── RMSE bar chart ─────────────────────────────────────────────────────────
    color_map = {
        "LSTM":            "#00CC96",
        "H2O AutoML":      "#636EFA",
        "Prophet":         "#FFA15A",
        "ARIMA (baseline)":"#EF553B",
    }

    fig_bar = go.Figure()
    for _, row in nb_df.iterrows():
        fig_bar.add_trace(go.Bar(
            x=[row["Model"]], y=[row["RMSE"]],
            name=row["Model"],
            marker_color=color_map.get(row["Model"], "#888"),
            text=[f"{row['RMSE']:.2f}"],
            textposition="outside",
        ))
    fig_bar.update_layout(
        title="RMSE by Model — Lower is Better",
        xaxis_title="Model", yaxis_title="RMSE",
        yaxis=dict(range=[0, ARIMA_BASELINE * 1.25]),
        showlegend=False,
        bargap=0.35,
    )
    st.plotly_chart(fig_bar, use_container_width=True)

    # ── MAE bar chart ──────────────────────────────────────────────────────────
    fig_mae = go.Figure()
    for _, row in nb_df.iterrows():
        fig_mae.add_trace(go.Bar(
            x=[row["Model"]], y=[row["MAE"]],
            name=row["Model"],
            marker_color=color_map.get(row["Model"], "#888"),
            text=[f"{row['MAE']:.2f}"],
            textposition="outside",
        ))
    fig_mae.update_layout(
        title="MAE by Model — Lower is Better",
        xaxis_title="Model", yaxis_title="MAE",
        yaxis=dict(range=[0, nb_df["MAE"].max() * 1.25]),
        showlegend=False,
        bargap=0.35,
    )
    st.plotly_chart(fig_mae, use_container_width=True)

    # ── Improvement chart ──────────────────────────────────────────────────────
    fig_imp = go.Figure(go.Bar(
        x=nb_df["Model"],
        y=nb_df["RMSE_Improvement_%"],
        marker_color=[color_map.get(m, "#888") for m in nb_df["Model"]],
        text=[f"{v:.1f}%" for v in nb_df["RMSE_Improvement_%"]],
        textposition="outside",
    ))
    fig_imp.update_layout(
        title="RMSE Improvement vs ARIMA Baseline",
        xaxis_title="Model", yaxis_title="Improvement (%)",
        yaxis=dict(range=[0, 80]),
        bargap=0.35,
    )
    st.plotly_chart(fig_imp, use_container_width=True)

    # ── Full results table ─────────────────────────────────────────────────────
    st.markdown("**Full Results Table**")
    st.dataframe(nb_df, use_container_width=True)

    st.success(
        "✅ Best model: **LSTM** — RMSE **158.00**, MAE **133.59**, "
        "**69.2% improvement** over ARIMA baseline (513.63)"
    )

    # ── All-model forecast overlay on the exact test window ───────────────────
    st.divider()
    st.markdown("#### All Models — Test Window (Jan 1 – Feb 12, 2021)")
    st.caption(
        "ARIMA and Prophet trained on data up to Dec 31, 2020. "
        "LSTM and H2O AutoML predictions are from the original notebook (GPU-trained)."
    )

    if st.button("▶ Show All Forecasts", type="primary"):
        mask_train = df["date"] < TEST_START
        train_vals = df.loc[mask_train, "new_cases"].values.astype(float)

        # Compute live RMSE from predictions file
        arima_rmse   = float(np.sqrt(np.mean((pred["arima_pred"]   - pred["actual"])**2)))
        prophet_rmse = float(np.sqrt(np.mean((pred["prophet_pred"] - pred["actual"])**2)))

        fig_live = go.Figure()
        fig_live.add_trace(go.Scatter(
            x=df.loc[mask_train, "date"], y=train_vals,
            name="Training data", line=dict(color="#aaa", width=1),
        ))
        fig_live.add_trace(go.Scatter(
            x=pred["date"], y=pred["actual"],
            name="Actual (test)", line=dict(color="#fff", width=2.5),
        ))
        fig_live.add_trace(go.Scatter(
            x=pred["date"], y=pred["arima_pred"],
            name=f"ARIMA (RMSE {arima_rmse:.0f})",
            line=dict(color="#EF553B", dash="dash", width=2),
        ))
        fig_live.add_trace(go.Scatter(
            x=pred["date"], y=pred["prophet_pred"],
            name=f"Prophet (RMSE {prophet_rmse:.0f})",
            line=dict(color="#FFA15A", dash="dot", width=2),
        ))
        fig_live.add_trace(go.Scatter(
            x=pred["date"], y=pred["lstm_pred"],
            name="LSTM (RMSE 158, notebook)",
            line=dict(color="#00CC96", dash="dashdot", width=2),
        ))
        fig_live.add_trace(go.Scatter(
            x=pred["date"], y=pred["h2o_pred"],
            name="H2O AutoML (RMSE 200, notebook)",
            line=dict(color="#636EFA", dash="longdash", width=2),
        ))
        fig_live.add_vrect(
            x0=TEST_START, x1=TEST_END,
            fillcolor="rgba(255,255,255,0.05)", line_width=1,
            line_color="#555",
            annotation_text="Test window", annotation_position="top left",
        )
        fig_live.update_layout(
            title="All Models — New Cases Forecast vs Actual",
            xaxis_title="Date", yaxis_title="New Cases", hovermode="x unified",
        )
        st.plotly_chart(fig_live, use_container_width=True)

        c1, c2, c3, c4 = st.columns(4)
        c1.metric("ARIMA RMSE",   f"{arima_rmse:.1f}")
        c2.metric("H2O RMSE",     "200.47", help="Notebook value")
        c3.metric("Prophet RMSE", f"{prophet_rmse:.1f}")
        c4.metric("LSTM RMSE",    "158.00", help="Notebook value")
    else:
        st.info("Click **▶ Show All Forecasts** to overlay all 4 models on the test window.")


st.divider()
st.caption(
    "Data: covid19_bangladesh_cleaned.csv · Predictions: predictions.csv · "
    "LSTM & H2O AutoML from original notebook · Built with Streamlit"
)
