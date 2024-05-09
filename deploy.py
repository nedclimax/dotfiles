#!/usr/env/python3

import subprocess
import json
import os

WINDOWS = os.name == "nt"

POWERSHELL_HOME_SCRIPT = """
$HomeExists = Test-Path Env:HOME
if ($HomeExists -ne $true) {
    echo "Creating HOME environnement variable targeted to $env:USERPROFILE"
    [Environment]::SetEnvironmentVariable("HOME", $env:USERPROFILE, "User")
} else {
    Write-Warning "HOME environnement variable already exists."
}
"""


def convert_home_path(path):
    home = os.getenv("HOME")
    sep = "\\" if WINDOWS else "/"
    path = path.split("~")[1]
    path = f"{home}{sep}{path}"
    path = os.path.abspath(path)
    return path


if WINDOWS:
    subprocess.call(["powershell", POWERSHELL_HOME_SCRIPT])

f = open("./manifest.json", "r", encoding="utf8")
contents = f.read()
f.close()

data = json.loads(contents)
links = data["links"]

for link in links:
    src = os.path.abspath(link["name"])
    dest = None
    if isinstance(link["destination"], str):
        dest = link["destination"]
    elif os.name == "nt":
        dest = link["destination"]["windows"]
    else:
        dest = link["destination"]["linux"]
    is_directory = os.path.isdir(src)
    print(f"{src}: is_directory = {is_directory}")
    print(f"dest = {convert_home_path(dest)}", end="\n\n")
