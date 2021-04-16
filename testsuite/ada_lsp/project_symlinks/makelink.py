import os
import os.path
import sys

dir=os.path.dirname(sys.argv[0])
link=os.path.join (dir, 'link')
if not os.path.exists(link):
    os.symlink('prj', link)