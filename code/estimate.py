#!/usr/bin/env python3
import argparse
from concurrent.futures import ThreadPoolExecutor
import json
import os
from pathlib import Path
import shutil
import subprocess


def main():
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description="Estimate the DDM and Logit models.")
    parser.add_argument("--python", required=True)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--subjects", help="Comma-separated participant IDs")
    parser.add_argument("--base-seed", type=int, default=20261029)
    parser.add_argument("--output-root", type=Path, default=root / "results")
    args = parser.parse_args()
    retained = sorted(set(range(1, 64)) - {11, 25, 27, 41})
    try:
        subjects = [int(x) for x in args.subjects.split(",")] if args.subjects else retained
    except ValueError:
        parser.error("--subjects must be comma-separated integers")
    if not subjects or len(set(subjects)) != len(subjects) or not set(subjects) <= set(retained):
        parser.error("Choose distinct retained participant IDs")
    if not 1 <= args.workers <= 32 or not 0 <= args.base_seed <= 2**32 - 65:
        parser.error("Use 1..32 workers and a base seed in 0..2**32-65")
    executable = shutil.which(args.python)
    if executable is None:
        parser.error("Python executable not found: " + args.python)
    executable = str(Path(executable).resolve())
    destination = args.output_root.resolve()

    def fit(subject):
        run = destination / "runs" / str(subject)
        run.mkdir(parents=True, exist_ok=True)
        record = run / "run.json"
        seed = args.base_seed + subject
        details = json.loads(record.read_text()) if record.exists() else {}
        if details.get("status") != "completed" or details.get("seed") != seed:
            details = dict(subject=subject, seed=seed, status="running")
            record.write_text(json.dumps(details, indent=2) + "\n")
            env = os.environ.copy()
            env.update(MPLBACKEND="Agg", MPLCONFIGDIR=str(run / "matplotlib"),
                       MKL_NUM_THREADS="1", OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1")
            command = [executable, str(root / "code/model.py"),
                       "--data", str(root / "data/yn.csv"), "--output", str(run),
                       "--subjects", str(subject), "--seed", str(seed)]
            with (run / "estimation.log").open("w") as log:
                result = subprocess.run(command, cwd=str(run), env=env,
                                        stdout=log, stderr=subprocess.STDOUT)
            details.update(status="completed" if result.returncode == 0 else "failed")
            record.write_text(json.dumps(details, indent=2) + "\n")
            if result.returncode:
                print("Participant %s failed; see %s" % (subject, run / "estimation.log"), flush=True)
                return False
        out = destination / "model_fits" / str(subject)
        out.mkdir(parents=True, exist_ok=True)
        for suffix in ("ddm_estimates", "logit_estimates", "RTthresh"):
            name = "%s_%s.csv" % (subject, suffix)
            sources = sorted(run.glob("session3_correct/*/%s/%s" % (subject, name)),
                             key=lambda path: path.stat().st_mtime)
            if not sources:
                raise RuntimeError("Missing model output: " + name)
            shutil.copyfile(sources[-1], out / name)
        print("Participant %s completed" % subject, flush=True)
        return True

    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        return int(not all(list(pool.map(fit, subjects))))


if __name__ == "__main__":
    raise SystemExit(main())
