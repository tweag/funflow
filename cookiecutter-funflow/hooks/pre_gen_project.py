import re
import sys


MODULE_REGEX = r'^[-a-zA-Z0-9]+$'

project_name = '{{ cookiecutter.project_name }}'

if not re.match(MODULE_REGEX, project_name):
    print('ERROR: %s is not a valid Haskell project name! It must only contain alphanumeric characters and dashes.' % project_name)
    # exits with status 1 to indicate failure
    sys.exit(1)
