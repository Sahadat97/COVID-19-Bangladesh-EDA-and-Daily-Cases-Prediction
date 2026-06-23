import warnings
import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
import plotly.express as px

warnings.filterwarnings("ignore")

# ── Optional live-model imports ───────────────────────────────────────────────
try:
    from pmdarima import auto_arima as _auto_arima
    _ARIMA_AVAILABLE = True
except ImportError:
    _ARIMA_AVAILABLE = False

try:
    from prophet import Prophet as _Prophet
    _PROPHET_AVAILABLE = True
except ImportError:
    _PROPHET_AVAILABLE = False


# ── Live model functions ──────────────────────────────────────────────────────

@st.cache_data(show_spinner="Training ARIMA…")
def run_arima(train_series: tuple, n_periods: int):
    """Fit ARIMA(1,1,1) on training data and return n_periods forecasts."""
    import numpy as _np
    from pmdarima import auto_arima as _aa
    series = list(train_series)
    model  = _aa(series, start_p=1, start_q=1, max_p=3, max_q=3, d=1,
                 seasonal=False, stepwise=True, suppress_warnings=True,
                 error_action="ignore", information_criterion="aic")
    fc, _  = model.predict(n_periods=n_periods, return_conf_int=True)
    return _np.maximum(fc, 0)


@st.cache_data(show_spinner="Training Prophet…")
def run_prophet(train_df_records: tuple, future_dates: tuple):
    """Fit Prophet on training data and return forecasts for future_dates."""
    import pandas as _pd
    import numpy as _np
    train = _pd.DataFrame(list(train_df_records), columns=["ds", "y"])
    m = _Prophet(
        yearly_seasonality=True,
        weekly_seasonality=False,
        daily_seasonality=False,
        changepoint_prior_scale=0.05,
    )
    m.fit(train)
    future = _pd.DataFrame({"ds": list(future_dates)})
    fc = m.predict(future)["yhat"].values
    return _np.maximum(fc, 0)

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
        "ARIMA":      {"col": "arima_pred",   "color": "#EF553B", "live": True},
        "H2O AutoML": {"col": "h2o_pred",     "color": "#636EFA", "live": False},
        "Prophet":    {"col": "prophet_pred",  "color": "#FFA15A", "live": True},
        "LSTM":       {"col": "lstm_pred",     "color": "#00CC96", "live": False},
    }

    c1, c2 = st.columns([1, 3])

    with c1:
        model_choice = st.selectbox(
            "Model",
            ["ARIMA", "H2O AutoML", "Prophet", "LSTM"],
        )
        meta = _MODEL_META[model_choice]

        if model_choice == "ARIMA":
            if _ARIMA_AVAILABLE:
                st.info("**ARIMA(1,1,1)** will train live on data up to Dec 31, 2020.")
            else:
                st.warning("pmdarima not installed — showing notebook results.")
        elif model_choice == "Prophet":
            if _PROPHET_AVAILABLE:
                st.info("**Prophet** will train live on data up to Dec 31, 2020.")
            else:
                st.warning("prophet not installed — showing notebook results.")
        else:
            st.info(
                f"**{model_choice}** requires GPU training. "
                "Showing notebook results (Jan 1 – Feb 12, 2021 test window)."
            )

        run_btn = st.button("▶ Run Forecast", type="primary", use_container_width=True)

    with c2:
        if run_btn:
            mask_train = df["date"] < FC_TEST_START
            train_vals = df.loc[mask_train, "new_cases"].values.astype(float)
            n_periods  = int((pred["date"].max() - pred["date"].min()).days) + 1

            # ── Decide: live train or load from CSV ───────────────────────────
            if model_choice == "ARIMA" and _ARIMA_AVAILABLE:
                with st.spinner("Training ARIMA…"):
                    fc_vals = run_arima(tuple(train_vals), n_periods)
                label  = "ARIMA (live)"
                source = "live"
            elif model_choice == "Prophet" and _PROPHET_AVAILABLE:
                train_dates = df.loc[mask_train, "date"].values
                records = tuple(zip(train_dates, train_vals))
                future  = tuple(pred["date"].values)
                with st.spinner("Training Prophet…"):
                    fc_vals = run_prophet(records, future)
                label  = "Prophet (live)"
                source = "live"
            else:
                fc_vals = pred[meta["col"]].values
                label   = f"{model_choice} (notebook)"
                source  = "notebook"

            # ── Metrics ───────────────────────────────────────────────────────
            actual = pred["actual"].values.astype(float)
            rmse = float(np.sqrt(np.mean((fc_vals - actual) ** 2)))
            mae  = float(np.mean(np.abs(fc_vals - actual)))

            # ── Plot ──────────────────────────────────────────────────────────
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
                x=pred["date"], y=fc_vals,
                name=label,
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
            if source == "live":
                st.caption(f"✅ Trained live in this session — not pre-computed.")
        else:
            st.info("Select a model on the left and click **▶ Run Forecast**.")


# ── Tab 3 · Model Comparison ──────────────────────────────────────────────────
with tab_compare:
    st.subheader("Model Comparison — Test Window: Jan 1 – Feb 12, 2021")
    st.markdown(
        "Exact results from the notebook — all metrics loaded from `predictions.csv`."
    )

    # ── Compute all metrics dynamically from predictions.csv ──────────────────
    TEST_START = pd.Timestamp("2021-01-01")
    TEST_END   = pd.Timestamp("2021-02-12")

    _model_cols = [
        ("LSTM",             "lstm_pred"),
        ("H2O AutoML",       "h2o_pred"),
        ("Prophet",          "prophet_pred"),
        ("ARIMA (baseline)", "arima_pred"),
    ]
    _computed = {}
    for _name, _col in _model_cols:
        _rmse = float(np.sqrt(np.mean((pred[_col] - pred["actual"]) ** 2)))
        _mae  = float(np.mean(np.abs(pred[_col] - pred["actual"])))
        _computed[_name] = {"RMSE": _rmse, "MAE": _mae}

    ARIMA_BASELINE = _computed["ARIMA (baseline)"]["RMSE"]
    for _name in _computed:
        _imp = (ARIMA_BASELINE - _computed[_name]["RMSE"]) / ARIMA_BASELINE * 100
        _computed[_name]["RMSE_Improvement_%"] = round(max(_imp, 0.0), 1)

    nb_df = pd.DataFrame([
        {"Rank": i + 1, "Model": _name, **_vals}
        for i, (_name, _vals) in enumerate(_computed.items())
    ]).set_index("Rank")

    color_map = {
        "LSTM":             "#00CC96",
        "H2O AutoML":       "#636EFA",
        "Prophet":          "#FFA15A",
        "ARIMA (baseline)": "#EF553B",
    }

    # ── RMSE bar chart ─────────────────────────────────────────────────────────
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
    _imp_max = nb_df["RMSE_Improvement_%"].max()
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
        yaxis=dict(range=[0, _imp_max * 1.25]),
        bargap=0.35,
    )
    st.plotly_chart(fig_imp, use_container_width=True)

    # ── Full results table ─────────────────────────────────────────────────────
    st.markdown("**Full Results Table**")
    st.dataframe(nb_df, use_container_width=True)

    _best      = nb_df.loc[nb_df["RMSE"].idxmin()]
    _best_name = _best["Model"]
    _best_rmse = _best["RMSE"]
    _best_mae  = _best["MAE"]
    _best_imp  = _best["RMSE_Improvement_%"]
    st.success(
        f"✅ Best model: **{_best_name}** — RMSE **{_best_rmse:.2f}**, "
        f"MAE **{_best_mae:.2f}**, "
        f"**{_best_imp:.1f}% improvement** over ARIMA baseline ({ARIMA_BASELINE:.2f})"
    )

    # ── All-model forecast overlay on the exact test window ───────────────────
    st.divider()
    st.markdown("#### All Models — Test Window (Jan 1 – Feb 12, 2021)")
    st.caption("All predictions loaded from predictions.csv (original notebook run).")

    if st.button("▶ Show All Forecasts", type="primary"):
        mask_train = df["date"] < TEST_START
        train_vals = df.loc[mask_train, "new_cases"].values.astype(float)

        lstm_rmse    = _computed["LSTM"]["RMSE"]
        h2o_rmse     = _computed["H2O AutoML"]["RMSE"]
        prophet_rmse = _computed["Prophet"]["RMSE"]
        arima_rmse   = _computed["ARIMA (baseline)"]["RMSE"]

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
            name=f"LSTM (RMSE {lstm_rmse:.0f})",
            line=dict(color="#00CC96", dash="dashdot", width=2),
        ))
        fig_live.add_trace(go.Scatter(
            x=pred["date"], y=pred["h2o_pred"],
            name=f"H2O AutoML (RMSE {h2o_rmse:.0f})",
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
        c2.metric("H2O RMSE",     f"{h2o_rmse:.1f}")
        c3.metric("Prophet RMSE", f"{prophet_rmse:.1f}")
        c4.metric("LSTM RMSE",    f"{lstm_rmse:.1f}")
    else:
        st.info("Click **▶ Show All Forecasts** to overlay all 4 models on the test window.")


st.divider()
st.caption(
    "Data: covid19_bangladesh_cleaned.csv · Predictions: predictions.csv · "
    "All metrics computed from predictions.csv · Built with Streamlit"
)
