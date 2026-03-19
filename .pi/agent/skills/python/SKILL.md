# Python language skill
Use this skill when writing Python code.

- Pydantic for data validation and serialization
- Rich for nice text output
- Jinja for rendering human-readable output
- json for machine readable output (rendered from Pydantic models)
- logging for debug/verbose/status messages.
  - rich logging handler to stderr
  - in each file, create a global logger like `logger = logging.getLogger(__name__)`
  - `--verbose` mode sets logging level to DEBUG, enables tracebacks for exceptions. otherwise exceptions printed like `error: failed to open file: ...` to stdout with status code non-zero.
  - provide a `--verbose` flag to enable verbose logging (log level DEBUG), and `--quiet` to disable logging (log level ERROR)
- stdout strictly for command output, either human readable report or json.
  - pass explicit rich.Console instance to any routine that prints. allocate this in the main routine. allocate a global console for writing to STDERR (primarily for status spinners and logging).
- status code non-zero for errors. zero for success.
- use click when the tool supports many subcommands, otherwise argparse when the tool has a single purpose.
- type hints for function signatures.
- google style docstrings for documentation, but don't repeat the type annotation. only explain things that aren't obvious. use the `raises` section to document how the function can fail.
- when a docstring is multiline, ensure there is a trailing newline so the triple-quote is on its own line at the end of the end of the docstring.
- pytest for testing. 
  - DO NOT USE MOCKS, instead layer and architect the code so that it composes nicely. prefer data/value-oriented designs.
  - during development, write tests before implementing a function.
  - no dumb tests. create tests that demonstrate functionality
  - keep the test suite fast. use session-scoped fixtures to cache expensive resources. and tempfile directories (contextmanager fixtures) for test-local resources.
- use rich.Spinner with the stderr console and transient=True for any long running operations. use the contextmanager style. its ok to nest these.
- prefer to use dataclasses when possible, and use `@classmethod from_foo(cls, foo)` style constructors. pydantic dataclasses are ok too.
- raise exceptions rather than returning None or error sentinal value. document the exceptions when they're not obvious, especially when they bubble up from callees.
- use pathlib.Path for any file system paths
- use ruff for formatting and linting, mypy for type checking
- functions should be named starting with verbs. `get_` when it returns, `validate_` no return - just raise exception on error, `render_` returns string representation of some combined data, `output_` writes to stdout.
