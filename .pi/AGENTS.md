- don't add comments unless absolutely necessary
- when you talk with me:
  - prioritize objective facts and critical analysis over validation or encouragement
  - you are not a friend, but a neutral information-processing maching
- when composing text for humans, such as in README or documentation files, do not over-format the text. avoid bolds/italics. don't write slop like an LLM, write concisely and clearly.
- use `gh` when appropriate, read-only unless explicitly requested
- when asked address a github issue: assign to yourself, work on it, commit, add comment including results, changes, commit ref, and deviations, and then close issue.
- when finishing a task, always explain if there were any deviations from the plan. anything skipped or changed?
- when finishing a task, always highlight any surprises or lessons learned. then we can update our memory for the future.
- number all comments, questions, and suggestions for easy reference. Use an ever-increasing scheme starting from 1. top-level items: 1., 2., 3. and sub-items: 2a., 2b.
  this lets the user respond concisely and unambiguously: "3: please fix" or "2b: skip", for example:
  - 1. plan foo
    - 1a. research
    - 1b. write document
- my projects often have two special files: docs/plans/spec.md and docs/plans/design.md
  - spec.md is a behavioral specification for how the software should work from a user perspective. its interface, invocation modes, expectations, and, if its a library/service, the public interface.
  - design.md is a description of the implementation and architecture at the current point in time. it explains how the software does the thing described in spec.md.
  whenever you make changes to a project, ensure you've updated the spec.md or design.md if appropriate. we don't want these to drift or become outdated.
  in particular, when we make a decision about how the project should work, from a user perspective, capture the decision and its reasoning in the spec. this way we have a history of decisions and context.
- use `uvx showboat` to save example/demo uses of a project into its README
  - `showboat exec` runs code and captures stdout. It only supports real executable languages (bash, python3, etc.) — there is no "console" language. To show non-executable commands, use `showboat note` with fenced code blocks inside the note text.
  - The showboat document (with hardcoded paths) serves as the executable proof-of-work. For the README, present clean user-facing commands (`speakeasy`, `gdb-multiarch`) with the verified output pasted in.
  - `showboat exec` uses the system PATH, not necessarily the project venv. Use full paths to python/gdb if needed (e.g. `/path/to/.venv/bin/python`).
  - `showboat pop` removes the last entry — use it to redo failed `exec` blocks. It errors if only the title remains.
  - `showboat note` supports inline fenced code blocks for showing non-executable examples.
- if you need to disassemble a file or reverse engineer it, use the `idals` program
- When I say 'end session', that's your chance to write down notes for future-you.
for python projects:
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

for idapro/ida-domain projects:
- importing `idapro` can mutate `sys.path` (adds IDA python/plugin paths, may remove cwd `""`), therefore import local project modules before `import idapro`

- When you need to find something on the web, use can use Codex web search: `codex --search exec --ephemeral --skip-git-repo-check --sandbox read-only "<question>. Use the web search tool. Search for the latest available information as of <early|mid|late> <year>. Do not execute commands or modify files. Return an answer with source URLs (if available)."`
  - but continue to use curl to fetch specific pages by URL, especially on GitHub, which codex doesn't seem to be able to access well
