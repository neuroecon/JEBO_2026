#!/usr/bin/env python3
import argparse
from pathlib import Path
import subprocess
import sys

parser = argparse.ArgumentParser(description="Run the tables and part-worth plots.")
parser.add_argument("--estimate", action="store_true", help="Also run model estimation")
parser.add_argument("--python", help="Python 2 executable for estimation")
parser.add_argument("--workers", type=int, default=4)
args = parser.parse_args()
root = Path(__file__).resolve().parent
if args.estimate:
    if not args.python:
        parser.error("--estimate requires --python /path/to/python2")
    subprocess.run([sys.executable, str(root / "code/estimate.py"),
                    "--python", args.python, "--workers", str(args.workers)], check=True)
subprocess.run(["Rscript", str(root / "code/analysis.R")], check=True)
