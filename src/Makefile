# Emacs invocation
EMACS_COMMAND := emacs

# do not load an init file: -q
# avoid processing X resources: -Q
# send messages to stderr: --batch
EMACS := $(EMACS_COMMAND) -Q -q --batch

EVAL   := $(EMACS) --eval
SCRIPT := $(EMACS) --script

VEX       := ~/.emacs.d/lisp/vex
ASYNC     := ~/.emacs.d/site-lisp/async-20200113.1745
XELB      := ~/.emacs.d/site-lisp/xelb-20190914.0
EXWM      := ~/.emacs.d/site-lisp/exwm-20200524.0
EMMS      := ~/.emacs.d/site-lisp/emms-20200528.2116
DASH      := ~/.emacs.d/site-lisp/dash-20200524.1947
DASH-DOCS := ~/.emacs.d/site-lisp/dash-docs-20200516.1943
COMPANY   := ~/.emacs.d/site-lisp/company-20200525.101
FLYCHECK  := ~/.emacs.d/site-lisp/flycheck-20200605.1351

# Additional emacs loadpath
LOADPATH := -L $(VEX) \
		-L $(ASYNC) \
		-L $(DASH) \
		-L $(XELB) \
		-L $(EXWM) \
		-L $(EMMS) \
		-L $(DASH-DOCS) \
		-L $(FLYCHECK) \
		-L $(COMPANY)

# Scritps to eval
TANGLE := tangle.el

# Files to compile
INIT := init.el

# Compiled files
ELC := $(EL:.el=.elc)

.PHONY: all

tangle:
	$(SCRIPT) $(TANGLE)

# Compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(INIT)

all: tangle batch-compile

$(ELC): %.elc: %.el
	$(EMACS) $(LOADPATH) -f batch-byte-compile $<

# Compile needed files
compile: $(ELC)
