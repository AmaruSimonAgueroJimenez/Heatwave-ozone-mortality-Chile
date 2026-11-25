"""run_python.py
Renders the reports written in Python (docs/py_*.qmd) with one command.

The Python reports rebuild parts of the analysis independently and compare the
result with the tables and datasets written by R, so the R pipeline
(Code/run_all.R) has to run first. Nothing in the R pipeline depends on Python.

Usage, from a terminal in the project folder:
    python3 Code/run_python.py                everything
    python3 Code/run_python.py --no-install   stop if a package is missing

Nothing else has to be done by hand. The packages of requirements.txt are
looked for in the Python that runs this file. If they are not there with those
versions, an environment for this project is created outside the project
folder, in the cache folder of the user, and the packages are installed in it.
Quarto is looked for where it is usually installed.

Outputs
    docs/py_*.html                        the rendered reports
    Output/run_environment_python.txt     Python, packages and Quarto of the run
    Data/Derived/run_python_log.txt       what every step printed
"""

import json
import os
import platform
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
if not (ROOT / "_quarto.yml").exists():
    sys.exit("Code/run_python.py is not inside the project folder.")
LOG = ROOT / "Data" / "Derived" / "run_python_log.txt"
INSTALL = "--no-install" not in sys.argv[1:]
unknown = [a for a in sys.argv[1:] if a != "--no-install"]
if unknown:
    sys.exit("Unknown argument: " + " ".join(unknown))


def say(text=""):
    """Print a line and keep it in the log."""
    print(text, flush=True)
    with open(LOG, "a", encoding="utf-8") as log:
        log.write(text + "\n")


def stop(text):
    """Leave the reason of a failure in the log and end with an error."""
    say(text)
    sys.exit(1)


def run(command, **options):
    """Run a command, echo what it prints into the log, and return whether it succeeded."""
    done = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                          text=True, errors="replace", **options)
    if done.stdout.strip():
        say(done.stdout.rstrip())
    return done.returncode == 0


def requirements():
    """Packages of requirements.txt and their versions."""
    pinned = {}
    for line in (ROOT / "requirements.txt").read_text(encoding="utf-8").splitlines():
        line = line.split("#")[0].strip()
        if line:
            name, separator, version = line.partition("==")
            if not separator:
                stop("requirements.txt: every package needs a version (name==version): " + line)
            pinned[name] = version
    return pinned


def versions_in(python, names):
    """Versions of `names` in one Python: None when it cannot run, and None for a package it lacks."""
    code = ("import importlib.metadata as m, json, platform, sys\n"
            "out = {'python': platform.python_version()}\n"
            "for name in sys.argv[1:]:\n"
            "    try:\n"
            "        out[name] = m.version(name)\n"
            "    except m.PackageNotFoundError:\n"
            "        out[name] = None\n"
            "print(json.dumps(out))\n")
    try:
        done = subprocess.run([str(python), "-c", code, *names], capture_output=True, text=True)
    except OSError:
        return None
    if done.returncode != 0:
        return None
    return json.loads(done.stdout)


def environment_folder():
    """Folder of the environment of this project, outside the project folder."""
    if sys.platform == "darwin":
        base = Path.home() / "Library" / "Caches"
    elif os.name == "nt":
        base = Path(os.environ.get("LOCALAPPDATA", Path.home() / "AppData" / "Local"))
    else:
        base = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache"))
    return base / "heatwave-ozone-mortality-chile" / "python"


def python_of(environment):
    return environment / ("Scripts/python.exe" if os.name == "nt" else "bin/python")


def newest_python():
    """The newest Python 3 on this machine: recent package versions need a recent Python."""
    names = ["python3.%d" % minor for minor in range(20, 8, -1)] + ["python3", "python"]
    folders = ["/opt/homebrew/bin", "/usr/local/bin", "/usr/bin",
               str(Path.home() / "miniconda3" / "bin"), str(Path.home() / "miniforge3" / "bin"),
               str(Path.home() / "anaconda3" / "bin")]
    candidates = [sys.executable]
    for name in names:
        candidates.append(shutil.which(name))
        candidates.extend(str(Path(folder) / name) for folder in folders)
    best, best_version = sys.executable, sys.version_info[:3]
    for candidate in dict.fromkeys(c for c in candidates if c and Path(c).exists()):
        found = versions_in(candidate, [])
        if found is None:
            continue
        version = tuple(int(part) for part in found["python"].split(".")[:3] if part.isdigit())
        if version[0] == 3 and version > best_version:
            best, best_version = candidate, version
    return best


def quarto_command():
    """Path of the Quarto program."""
    named = [os.environ.get("QUARTO_PATH", ""), os.environ.get("RSTUDIO_QUARTO", "")]
    named = [str(Path(n) / "quarto") if n and Path(n).is_dir() else n for n in named]
    candidates = [shutil.which("quarto"), *named,
                  "/usr/local/bin/quarto", "/opt/homebrew/bin/quarto", "/opt/quarto/bin/quarto",
                  "/Applications/quarto/bin/quarto",
                  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
                  "/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto"]
    for candidate in candidates:
        if candidate and Path(candidate).is_file():
            return candidate
    stop("Quarto was not found. Install it from https://quarto.org, or give the path of "
         "its program in the environment variable QUARTO_PATH.")


def prepare_python(pinned):
    """The Python that renders the reports, and a word on where its packages come from."""
    names = list(pinned)
    here = versions_in(sys.executable, names)
    if all(here[name] == pinned[name] for name in names):
        return sys.executable, "the Python that ran Code/run_python.py"
    environment = environment_folder()
    python = python_of(environment)
    there = versions_in(python, names) if python.exists() else None
    if there is not None and all(there[name] == pinned[name] for name in names):
        return str(python), "the environment of the project"
    if not INSTALL:
        # Without installing anything: any Python that has the packages, whatever their versions
        if all(here[name] is not None for name in names):
            return sys.executable, "the Python that ran Code/run_python.py"
        if there is not None and all(there[name] is not None for name in names):
            return str(python), "the environment of the project"
        missing = [name for name in names if here[name] is None]
        stop("Python packages not installed: " + ", ".join(missing)
             + "\nInstall them with: python3 -m pip install -r requirements.txt")
    if not python.exists():
        base = newest_python()
        say("Creating the Python environment of the project, outside the project folder.")
        environment.parent.mkdir(parents=True, exist_ok=True)
        if not run([base, "-m", "venv", str(environment)]):
            stop("The Python environment could not be created.")
    say("Installing the packages of requirements.txt (a few minutes the first time).")
    run([str(python), "-m", "pip", "install", "--quiet", "--upgrade", "pip"])
    if run([str(python), "-m", "pip", "install", "--quiet", "-r", str(ROOT / "requirements.txt")]):
        return str(python), "the environment of the project"
    say("The versions of requirements.txt cannot be installed in this Python. "
        "Installing the current versions of the same packages instead.")
    if run([str(python), "-m", "pip", "install", "--quiet", *names]):
        return str(python), "the environment of the project, with other versions than requirements.txt"
    stop("The Python packages could not be installed.")


def main():
    LOG.parent.mkdir(parents=True, exist_ok=True)
    LOG.write_text("Run of Code/run_python.py\n", encoding="utf-8")
    needed = ROOT / "Data" / "Derived" / "exposure_2009_2020.csv"
    if not needed.exists():
        stop("The R pipeline has not run yet: " + needed.relative_to(ROOT).as_posix() + " is missing."
             "\nRun Code/run_all.R first.")
    pinned = requirements()
    python, origin = prepare_python(pinned)
    found = versions_in(python, list(pinned))
    quarto = quarto_command()
    quarto_version = subprocess.run([quarto, "--version"], capture_output=True, text=True).stdout.strip()

    record = ["Software of the machine that last rendered the Python reports (Code/run_python.py)", "",
              "Python: %s (%s)" % (found["python"], origin),
              "System: %s %s" % (platform.system(), platform.machine()),
              "Quarto: %s" % quarto_version, "",
              "Python packages (version here, version of requirements.txt when it differs):"]
    width = max(len(name) for name in pinned)
    for name, version in pinned.items():
        note = "" if found[name] == version else "   (requirements.txt: %s)" % version
        record.append("  %-*s %s%s" % (width, name, found[name], note))
    (ROOT / "Output" / "run_environment_python.txt").write_text("\n".join(record) + "\n", encoding="utf-8")
    say("\n".join(record))

    reports = sorted((ROOT / "docs").glob("py_*.qmd"))
    failed = []
    for report in reports:
        say("\n===== %s =====" % report.name)
        if not run([quarto, "render", str(report)], env={**os.environ, "QUARTO_PYTHON": python}):
            say("%s FAILED" % report.name)
            failed.append(report.name)
    say("")
    if failed:
        say("Reports that could not be rendered: " + ", ".join(failed))
        sys.exit(1)
    say("The %d Python reports were rendered. Each one stops if its results differ from those of R."
        % len(reports))


if __name__ == "__main__":
    main()
