import os
import os.path
import sys

dir=os.path.dirname(sys.argv[0])
gpr=os.path.join (dir, 'subproject.gpr')
os.rename(gpr, gpr + '.old')
os.rename(gpr + '.fixed', gpr)
