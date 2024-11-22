"""This is a Python main entry point that calls drivers.pylsp.main().

We create this entry point here instead of in pylsp.py to avoid the pylsp.py module
getting imported twice: once as "__main__" and once as "drivers.pylsp".
"""

from drivers.pylsp import main

if __name__ == "__main__":
    main()
