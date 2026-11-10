Code for estimation, part-worth plots and k-means.

With Python 3.7+ and R installed, run:

```sh
python3 run.py
```

Inputs are in `data/`, including the model fits in `data/model_fits/`. Tables and plots are written to `output/`.

To run estimation, install the Python 2 environment with Conda on Linux x86-64:

```sh
bash code/setup.sh
python3 run.py --estimate --python "$PWD/runtime/hddm_py27/bin/python" --workers 4
```

Estimation saves to `results/` and resumes completed participants when repeated. Use `--workers 1` to run sequentially.
