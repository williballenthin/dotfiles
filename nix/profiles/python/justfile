SRC := "tool.py"

isort:
    uvx isort --length-sort --profile black --line-length 120 {{SRC}}

black:
    uvx black --line-length 120 {{SRC}}

ruff:
    uvx ruff check --line-length 120 {{SRC}}

ty:
    uvx ty check --ignore unresolved-import

mypy:
    uvx mypy --check-untyped-defs --ignore-missing-imports {{SRC}}

lint:
    -just isort
    -just black
    -just ruff
    -just ty
    -just mypy
