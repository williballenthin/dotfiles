- don't add comments unless absolutely necessary
- when you talk with me:
  - prioritize objective facts and critical analysis over validation or encouragement
  - you are not a friend, but a neutral information-processing maching
- when i say "find work", you should search for comments tagged "TODO(ai)" or "ai:" and address them
- use `gh` when appropriate, read-only unless explicitly requested
- use git worktrees for each new branch, avoid pulling submodules unless required. put new worktrees in .claude/worktree/*.
- NEVER use `git checkout --`, `git restore`, `git reset`, or `git stash` on files you didn't modify. Other people may be working on the project at the same time. When staging for commits, use hunk-level staging (`git add -p` or specific file paths) to only include your own changes.
- when asked to address a github issue: assign to yourself, work on it, commit, create a PR that references the issue (e.g. "Closes #N" in the PR body) so it auto-closes on merge. Do NOT close the issue directly — let the maintainer close it via PR merge.
- when finishing a task, always:
  - explain if there were any deviations from the plan. anything skipped or changed?
  - highlight any surprises or lessons learned. then we can update our memory for the future.
- use `uvx --from slop-guard sg --verbose` against markdown content you edit to keep your writing style concise and understandable. Avoid many section titles and heads, prefering that text and content flow together naturally.
- my projects often have two special files: docs/plans/spec.md and docs/plans/design.md
  - spec.md is a behavioral specification for how the software should work from a user perspective. its interface, invocation modes, expectations, and, if its a library/service, the public interface.
  - design.md is a description of the implementation and architecture at the current point in time. it explains how the software does the thing described in spec.md.
  whenever you make changes to a project, ensure you've updated the spec.md or design.md if appropriate. we don't want these to drift or become outdated.
  in particular, when we make a decision about how the project should work, from a user perspective, capture the decision and its reasoning in the spec. this way we have a history of decisions and context.
- code navigation: prefer LSP over Grep/Glob/Read
  - `goToDefinition` / `goToImplementation` to jump to source
  - `findReferences` to see all usages before renaming or changing a signature
  - `workspaceSymbol` to find where something is defined
  - `documentSymbol` to list all symbols in a file
  - `hover` for type info without reading the file
  - `incomingCalls` / `outgoingCalls` for call hierarchy
  - use Grep/Glob only for text/pattern searches (comments, strings, config values)
  - after editing code, check LSP diagnostics and fix any type errors or missing imports immediately
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
  - type hints for function signatures. use dataclasses, TypedDict, or pydantic instead of untyped dicts.
  - google style docstrings for documentation, but don't repeat the type annotation. only explain things that aren't obvious. use the `raises` section to document how the function can fail.
  - when a docstring is multiline, ensure there is a trailing newline so the triple-quote is on its own line at the end of the end of the docstring.
  - pytest for testing. 
    - DO NOT USE MOCKS or monkeypatch, instead layer and architect the code so that it composes nicely. prefer data/value-oriented designs.
    - during development, write tests before implementing a function.
    - no dumb tests. create tests that demonstrate functionality.
    - keep the test suite fast. use session-scoped fixtures to cache expensive resources. and tempfile directories (contextmanager fixtures) for test-local resources.
    - use pytets functional style, not class-based.
  - use rich.Spinner with the stderr console and transient=True for any long running operations. use the contextmanager style. its ok to nest these.
  - prefer to use dataclasses when possible, and use `@classmethod from_foo(cls, foo)` style constructors. pydantic dataclasses are ok too.
  - raise exceptions rather than returning None or error sentinal value. document the exceptions when they're not obvious, especially when they bubble up from callees.
  - use pathlib.Path for any file system paths
  - use ruff for formatting and linting, mypy for type checking
  - functions should be named starting with verbs. `get_` when it returns, `validate_` no return - just raise exception on error, `render_` returns string representation of some combined data, `output_` writes to stdout.
  - for standalone scripts, use inline dependencies from PEP 723. we'll use `uv run`.
  - document env vars in argparse help epilog:
    ```
    Environment variables:

    LOG_LEVEL        Logging level (default: disabled)
    API_TOKEN        Token to use to authenticate to xxxx (prompt if missing)
    ```
  - log stack trace only when --verbose or --debug:
    ```py
    def on_crash(exctype, value, traceback):
      if logger.isEnabledFor(logging.DEBUG):
          logger.error("Uncaught exception", exc_info=(exctype, value, tb))

    sys.excepthook = on_crash
- comments should be treated the same as production code, they shouldn't leak the content of conversations and stand the test of time
- no memorializing decisions in output artifacts
- only describe what is, never what was in documentation
- all summaries need to adhere to ASD-STE100 Simplified Technical English standards
- do not use the following words/phrases: blast radius, land, landed, lands, spine, earned its keep, grammar, spike, cutover, bake, seams, honest, honestly, honesty, long pole, long poles, register, grain, dissolve, floor, ladder, dear, seal, sealed, in anger, resent, amazing, incredible, perfect, robust, comprehensive, rigorous, surgical, elegant, systematic, dive, deep-dive, delve, unpack, leverage, streamline, surface, it's worth noting, to be clear, importantly, that said, the moment, in one breath, the thing itself, here's the thing, not just X but Y, not X it's Y, em-dashes
