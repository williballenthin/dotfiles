import os

if os.environ.get("IDA_IS_INTERACTIVE") == "1":
    # when running within IDA Pro...
    # see: https://community.hex-rays.com/t/how-to-check-if-idapythonrc-py-is-running-in-ida-pro-or-idalib/297/2

    ###############################################################################
    # Activate virtualenv at ~/.idapro/venv
    #
    # see also: https://williballenthin.com/post/using-a-virtualenv-for-idapython/
    from pathlib import Path

    import idaapi

    def activate_virtualenv(virtualenv_path: Path):
        for bindir in ("Scripts", "bin"):
            activate_this_path = virtualenv_path / bindir / "activate_this.py"

            if not activate_this_path.exists():
                continue

            if not activate_this_path.is_file():
                continue

            exec(activate_this_path.read_text(), dict(__file__=str(activate_this_path)))
            print("activated virtual environment: " + str(virtualenv_path))
            break

        else:
            raise ValueError('Could not find "activate_this.py" in ' + str(virtualenv_path))


    IDAUSR = Path(idaapi.get_user_idadir())
    activate_virtualenv(IDAUSR / "venv")


    ###############################################################################
    # Configure logging
    import logging
    logging.basicConfig(level=logging.INFO)
    # logging.basicConfig(level=logging.DEBUG)
