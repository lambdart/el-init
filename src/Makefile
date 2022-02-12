# Defines phony targets all and clean
.PHONY = all compile clean

# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch -L ./lisp -L ./site-lisp

# Remove command
RM = rm -f

# Define Compile Command (CC)
# Call batch-byte-compile function: -f
# CC := -f batch-native-compile
CC := -f batch-byte-compile

# Tangle script
TANGLE_SCRIPT = tangle.el

# default target
all: tangle compile

# run tangle script (org related)
# the script will tangle the source code blocks from vlm.org
# to compose the init.el final script
tangle:
	${EMACS} ${CC} ${TANGLE_SCRIPT}
	${EMACS} -l ${TANGLE_SCRIPT} -f tangle-and-rename

# Compile init.el and early-init.el
compile:
	 ${EMACS} ${CC} init.el early-init.el

# Remove all byte compiled files
clean:
	${RM} *.elc
