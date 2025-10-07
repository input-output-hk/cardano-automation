# Cardano Prometheus Tracker

*A lightweight Haskell tool to follow the heartbeat of Cardano nodes through their Prometheus metrics.*

## ✨ Overview

Cardano nodes expose rich operational metrics via a Prometheus-compatible HTTP endpoint.
This project provides a **tracker** that connects to that endpoint, parses the Prometheus text exposition format, and allows you to:

* Scrape node metrics at a fixed interval
* Validate and parse the Prometheus exposition format (including `HELP`, `TYPE`, and samples)
* Store metrics over time for analysis (CSV/SQLite planned)
* Expose tracker health and scrape status as Prometheus metrics themselves

The goal is to make **long-term metric tracking** simple and composable in Cardano environments.

---

## 🚀 Getting Started

### Prerequisites

* GHC ≥ 9.6
* Cabal ≥ 3.10
* A running Cardano node with metrics enabled (default at `127.0.0.1:12798/metrics`)

Enable metrics in your node’s `config.json`:

```json
{
  "TraceOptions": {
    "": {
      "backends": [
        "PrometheusSimple 127.0.0.1 12798",
      ],
      "detail": "DNormal",
      "severity": "Notice"
    }
  }
}
```

### Build

```bash
git clone https://github.com/input-output-hk/cardano-automation.git
cd cardano-automation
cabal build cardano-prometheus-tracker
```

### Run

```bash
cabal run cardano-prometheus-tracker -- \
  --target-url http://127.0.0.1:12798/metrics \
  --interval 5s \
  --store csv
```

---


## 📊 Example

Scraping a node metric:

```text
# HELP cardano_node_metrics_utxoSize_int Number of unspent transaction outputs
# TYPE cardano_node_metrics_utxoSize_int gauge
cardano_node_metrics_utxoSize_int 809442
```

Tracked output (CSV mode):

```csv
timestamp,metric,value,labels
2025-09-26T12:00:00Z,cardano_node_metrics_utxoSize_int,809442,"{}"
```

---

## 🔮 Roadmap

* [ ] Improved functionality and config, e.g. regex match metric names, set timeouts
* [ ] SQLite backend for persistent time-series
* [ ] Plotting capabilities

---

