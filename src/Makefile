# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch

# Remove command
RM = rm -f

# Define Compile Command (CC)
# Call batch-byte-compile function: -f
CC := -f batch-byte-compile

# Define eval and script commands
EC := --eval
SC := --script

# Tangle script
TANGLE_SCRIPT = tangle.el

# Init script
INIT_SCRIPT = init.el

# Elisp auxiliary source files
EL := tangle.el early-init.el

# Source-Compiled files mapping
ELC := $(EL:.el=.elc)

# Translate lisp (.el) in byte compiled (.elc) files
$(ELC): $(EL)
	${EMACS} ${CC} ${.ALLSRC}

# Run tangle script (org related)
# the script will tangle the source code blocks from vlm.org
# to compose the init.el final script
tangle:
	${EMACS} ${SC} ${TANGLE_SCRIPT}

# Compile generated init.el final generated script
init:
	${EMACS} ${CC} ${INIT_SCRIPT}

# Entry point
all: tangle init compile

# Compile *.el files
compile: $(ELC)

# Remove all byte compiled files
clean:
	${RM} *.elc
