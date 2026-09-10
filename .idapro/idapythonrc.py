import os

if os.environ.get("IDA_IS_INTERACTIVE") == "1":
    ###############################################################################
    # Configure logging
    import logging
    logging.basicConfig(level=logging.INFO)
    # logging.basicConfig(level=logging.DEBUG)
