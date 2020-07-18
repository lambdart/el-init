# Emacs invocation
EMACS = emacs -Q -q --batch

# Remove command
RM = rm -f

# Define Compile Command (CC)
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
# Call batch-byte-compile function: -f
CC := -f batch-byte-compile

# Define eval and script commands
EC := --eval
SC := --script

# Packages paths (temporary!)
VEX       = ~/.emacs.d/lisp/vex
ASYNC     = ~/.emacs.d/site-lisp/async-20200113.1745
XELB      = ~/.emacs.d/site-lisp/xelb-20190914.0
EXWM      = ~/.emacs.d/site-lisp/exwm-20200524.0
EMMS      = ~/.emacs.d/site-lisp/emms-20200528.2116
DASH      = ~/.emacs.d/site-lisp/dash-20200524.1947
DASH-DOCS = ~/.emacs.d/site-lisp/dash-docs-20200516.1943
COMPANY   = ~/.emacs.d/site-lisp/company-20200525.101
FLYCHECK  = ~/.emacs.d/site-lisp/flycheck-20200605.1351

# Additional emacs load-path
LOAD_PATH := -L $(VEX) \
		-L $(ASYNC) \
		-L $(DASH) \
		-L $(XELB) \
		-L $(EXWM) \
		-L $(EMMS) \
		-L $(DASH-DOCS) \
		-L $(FLYCHECK) \
		-L $(COMPANY)

# Tangle script
TANGLE_SCRIPT = tangle.el

# Init script
INIT_SCRIPT = init.el

# Elisp auxiliary source files
EL := tangle.el custom.el trash.el

# Source-Compiled files mapping
ELC := $(EL:.el=.elc)

# Translate lisp (.el) in byte compiled (.elc) files
$(ELC): $(EL)
	${EMACS} ${CC} ${.ALLSRC}

# Run tangle script (org related)
# the script will tangle the source code blocks from EOS.org
# to compose the init.el final script
tangle:
	${EMACS} ${SC} ${TANGLE_SCRIPT}

# Compile generated init.el final generated script
init:
	${EMACS} ${LOAD_PATH} ${CC} ${INIT_SCRIPT}

# Entry point
all: tangle init compile

# Compile *.el files
compile: $(ELC)

# Remove all byte compiled files
clean:
	${RM} *.elc
