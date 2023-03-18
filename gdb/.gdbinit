set disassembly-flavor intel
set history save on
set history filename ~/.gdb_history
set debuginfod enabled on


python

import sys
from os.path import expanduser
sys.path.insert(0, expanduser('~/.dotfiles/gdb'))

from qt import register_qt_printers
register_qt_printers (None)

from kde import register_kde_printers
register_kde_printers (None)

end
