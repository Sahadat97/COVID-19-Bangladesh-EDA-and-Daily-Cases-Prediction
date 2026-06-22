import re
import warnings
import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
import plotly.express as px
from io import StringIO
import requests

warnings.filterwarnings("ignore")

st.set_page_config(
    page_title="COVID-19 Bangladesh Forecasting",
    page_icon="🦠",
    layout="wide",
)

# ── Data (same pipeline as the notebook) ─────────────────────────────────────

@st.cache_data(show_spinner="Scraping latest data from Wikipedia…")
def load_data():
    url = "https://en.wikipedia.org/wiki/Statistics_of_the_COVID-19_pandemic_in_Bangladesh"
    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/120.0.0.0 Safari/537.36"
        )
    }
    response = requests.get(url, headers=headers)
    response.raise_for_status()
    tables = pd.read_html(StringIO(response.text), header=0)

    # Table 5 is the daily statistics table (same index as notebook)
    df = tables[5].copy()

    # Promote row 0 as column headers (same as notebook)
    df.columns = df.iloc[0]
    df = df.drop(index=0).reset_index(drop=True)

    # Clean column names (exact same logic as notebook)
    def clean_names(frame):
        frame = frame.copy()
        frame.columns = (
            frame.columns
            .str.lower()
            .str.strip()
            .str.replace(r"[\s/()\[\]]+", "_", regex=True)
            .str.replace(r"[^a-z0-9_]", "", regex=True)
            .str.strip("_")
        )
        return frame

    df = clean_names(df)

    # Drop notes column if present
    if "notes" in df.columns:
        df.drop(columns=["notes"], inplace=True)

    # Fix wikipedia footnote brackets in total_tested / newly_tested
    for col in ["total_tested", "newly_tested"]:
        if col in df.columns:
            df[col] = df[col].astype(str).str.extract(r"^(\d+)")

    # Parse date (strip footnote refs first)
    df["date"] = (
        df["date"].astype(str)
        .str.replace(r"\[.*?\]", "", regex=True)
        .str.strip()
    )
    df["date"] = pd.to_datetime(df["date"], format="%Y-%m-%d", errors="coerce")
    df = df.dropna(subset=["date"]).reset_index(drop=True)

    # Convert cumulative columns to numeric
    for col in ["total_tested", "total_cases", "total_deaths", "total_recovered"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    # Fill missing daily-change columns from cumulative diffs (same as notebook)
    cols_map = {
        "newly_tested":    "total_tested",
        "new_cases":       "total_cases",
        "new_deaths":      "total_deaths",
        "newly_recovered": "total_recovered",
    }
    for new_col, total_col in cols_map.items():
        if new_col in df.columns and total_col in df.columns:
            diff = df[total_col].diff()
            mask = df.index < 8
            df.loc[mask, new_col] = (
                pd.to_numeric(df.loc[mask, new_col], errors="coerce")
                .fillna(diff[mask])
            )

    # Convert daily columns to int
    daily_cols = ["newly_tested", "new_cases", "new_deaths", "newly_recovered"]
    for col in daily_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0).astype(int)

    if "days_since_first_confirmed_cases" in df.columns:
        df["days_since_first_confirmed_cases"] = pd.to_numeric(
            df["days_since_first_confirmed_cases"], errors="coerce"
        )

    df = df.sort_values("date").reset_index(drop=True)
    return df


# ── Models ────────────────────────────────────────────────────────────────────

@st.cache_data(show_spinner="Training ARIMA…")
def run_arima(values, forecast_days):
    from statsmodels.tsa.arima.model import ARIMA
    train = values[:-forecast_days]
    test  = values[-forecast_days:]
    model = ARIMA(train, order=(5, 1, 0))
    fit   = model.fit()
    preds = np.maximum(fit.forecast(steps=forecast_days), 0)
    rmse  = float(np.sqrt(np.mean((preds - test) ** 2)))
    return preds, rmse


@st.cache_data(show_spinner="Training Prophet…")
def run_prophet(_dates, values, forecast_days):
    from prophet import Prophet
    train_df = pd.DataFrame({
        "ds": _dates[:-forecast_days],
        "y":  values[:-forecast_days],
    })
    m = Prophet(
        daily_seasonality=False,
        yearly_seasonality=True,
        weekly_seasonality=True,
        changepoint_prior_scale=0.05,
    )
    m.fit(train_df)
    future   = m.make_future_dataframe(periods=forecast_days)
    forecast = m.predict(future)
    preds    = np.maximum(forecast["yhat"].values[-forecast_days:], 0)
    test     = values[-forecast_days:]
    rmse     = float(np.sqrt(np.mean((preds - test) ** 2)))
    return preds, rmse


# ── Layout ────────────────────────────────────────────────────────────────────

st.title("🦠 COVID-19 Bangladesh — EDA & Forecasting")
st.markdown(
    "Interactive demo of the "
    "[GitHub project](https://github.com/Sahadat97/COVID-19-Bangladesh-EDA-and-Daily-Cases-Prediction) "
    "by **Mohammad Sahadat Hossain**. "
    "Data scraped live from Wikipedia, same pipeline as the notebook."
)

df = load_data()

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
    c1, c2 = st.columns([1, 3])

    with c1:
        model_choice  = st.selectbox("Model", ["ARIMA (5,1,0)", "Prophet"])
        forecast_days = st.slider("Forecast window (days)", 7, 60, 20)
        target_col    = st.selectbox(
            "Target variable",
            ["new_cases", "total_cases", "new_deaths", "total_deaths"],
        )
        run_btn = st.button("▶ Run Forecast", type="primary", use_container_width=True)

    with c2:
        if run_btn:
            series     = df[target_col].values.astype(float)
            dates      = df["date"]
            test_dates = dates.iloc[-forecast_days:]

            if model_choice == "ARIMA (5,1,0)":
                preds, rmse = run_arima(series, forecast_days)
            else:
                preds, rmse = run_prophet(
                    dates.tolist(), series, forecast_days
                )

            fig = go.Figure()
            fig.add_trace(go.Scatter(
                x=dates, y=series, name="Actual (all)",
                line=dict(color="#aaa", width=1),
            ))
            fig.add_trace(go.Scatter(
                x=test_dates, y=series[-forecast_days:],
                name="Actual (test window)", line=dict(color="#00CC96", width=2),
            ))
            fig.add_trace(go.Scatter(
                x=test_dates, y=preds,
                name=f"{model_choice} Forecast",
                line=dict(color="#EF553B", dash="dash", width=2),
            ))
            fig.add_vrect(
                x0=test_dates.iloc[0], x1=test_dates.iloc[-1],
                fillcolor="rgba(239,85,59,0.07)", line_width=0,
                annotation_text="Forecast window", annotation_position="top left",
            )
            fig.update_layout(
                title=f"{model_choice} — {target_col.replace('_', ' ').title()}",
                xaxis_title="Date", yaxis_title="Count", hovermode="x unified",
            )
            st.plotly_chart(fig, use_container_width=True)
            st.metric("RMSE", f"{rmse:,.1f}")
        else:
            st.info("Configure the options on the left and click **▶ Run Forecast**.")


# ── Tab 3 · Model Comparison ──────────────────────────────────────────────────
with tab_compare:
    st.subheader("Model Comparison — Test Window: Jan 1 – Feb 12, 2021")
    st.markdown(
        "Exact results from the notebook. ARIMA and Prophet are also run live on the "
        "same test window so you can see the forecast curves. "
        "LSTM and H2O AutoML metrics are from the original notebook (require GPU training)."
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

    # ── Live forecast overlay on the exact test window ─────────────────────────
    st.divider()
    st.markdown("#### Live Forecast on the Same Test Window (Jan 1 – Feb 12, 2021)")
    st.caption("ARIMA and Prophet run live; LSTM & H2O shown as flat reference lines from notebook RMSE.")

    if st.button("▶ Run Live Forecasts", type="primary"):
        mask_test  = (df["date"] >= TEST_START) & (df["date"] <= TEST_END)
        mask_train = df["date"] < TEST_START

        if mask_test.sum() == 0:
            st.warning("Test window not found in the scraped data. Wikipedia may have updated the table structure.")
        else:
            train_vals = df.loc[mask_train, "new_cases"].values.astype(float)
            test_vals  = df.loc[mask_test,  "new_cases"].values.astype(float)
            test_dates = df.loc[mask_test,  "date"]
            n_test     = len(test_vals)

            train_dates = df.loc[mask_train, "date"]

            with st.spinner("Training ARIMA on data before Jan 1, 2021…"):
                full_series = df["new_cases"].values.astype(float)
                arima_preds, arima_live_rmse = run_arima(full_series, n_test)

            with st.spinner("Training Prophet on data before Jan 1, 2021…"):
                full_dates = df["date"].tolist()
                prophet_preds, prophet_live_rmse = run_prophet(full_dates, full_series, n_test)

            fig_live = go.Figure()
            # Training period
            fig_live.add_trace(go.Scatter(
                x=df.loc[mask_train, "date"], y=train_vals,
                name="Training data", line=dict(color="#aaa", width=1),
            ))
            # Actual test
            fig_live.add_trace(go.Scatter(
                x=test_dates, y=test_vals,
                name="Actual (test)", line=dict(color="#fff", width=2.5),
            ))
            # ARIMA
            fig_live.add_trace(go.Scatter(
                x=test_dates, y=arima_preds,
                name=f"ARIMA live (RMSE {arima_live_rmse:.0f})",
                line=dict(color="#EF553B", dash="dash", width=2),
            ))
            # Prophet
            fig_live.add_trace(go.Scatter(
                x=test_dates, y=prophet_preds,
                name=f"Prophet live (RMSE {prophet_live_rmse:.0f})",
                line=dict(color="#FFA15A", dash="dot", width=2),
            ))
            fig_live.add_vrect(
                x0=TEST_START, x1=TEST_END,
                fillcolor="rgba(255,255,255,0.05)", line_width=1,
                line_color="#555",
                annotation_text="Test window", annotation_position="top left",
            )
            fig_live.update_layout(
                title="Live Forecast vs Actual — Jan 1 to Feb 12, 2021",
                xaxis_title="Date", yaxis_title="New Cases", hovermode="x unified",
            )
            st.plotly_chart(fig_live, use_container_width=True)

            col1, col2 = st.columns(2)
            col1.metric("ARIMA live RMSE",   f"{arima_live_rmse:.1f}",
                        delta=f"notebook: {ARIMA_BASELINE}", delta_color="off")
            col2.metric("Prophet live RMSE", f"{prophet_live_rmse:.1f}",
                        delta=f"notebook: 208.90", delta_color="off")
    else:
        st.info("Click **▶ Run Live Forecasts** to see ARIMA and Prophet on the exact test window.")


st.divider()
st.caption(
    "Data: Wikipedia — Statistics of the COVID-19 pandemic in Bangladesh · "
    "ARIMA & Prophet computed live · LSTM & H2O AutoML from original notebook · "
    "Built with Streamlit"
)
