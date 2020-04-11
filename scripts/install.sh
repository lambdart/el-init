#!/usr/bin/env sh

# colors
_RED=""
_GREEN=""
_RESET=""
_YELLOW=""

# repositories
_CASK_REPO="https://github.com/cask/cask"
_EOS_REPO="https://github.com/esac-io/eos"

# default directories
_EOS_DIR="$HOME/.eos"
_CASK_DIR="$HOME/.cask"
_EMACS_DIR="$HOME/.emacs.d"

# emacs backup
_EMACS_BACKUP=

# flags
# _FLAG_BACKUP=1
_FLAG_SYMLINK=1

# defaults
_DEFAULT_DIR=
_DEFAULT_FILES="adapt.el Cask"
_DEFAULT_PREFIX="default."

# banner
banner() {
    # print help message
    cat <<-'EOF'
       __________________
      / ____// __ \/ ___/
     / __/  / / / /\__ \
    / /___ / /_/ /___/ /
   /_____/ \____//____/
                 ... installation script
EOF
}

# show help message
usage() {
    # print help message
    printf $_GREEN
    cat <<-'EOF'

  sh install.sh [-hlx] [-m PATH] [-c PATH]

  -h    show help message and exit
  -x    enable debugging
  -v    enable verbose
  -l    disables symlink (.emacs.d -> eos/src)

  -m PATH
      defines eos default directory

  -c PATH
        defines cask default directory

  Examples:

  [1] : ./install.sh
  [2] : ./install.sh -h
  [3] : ./install.sh -m .
  [3] : cd eos/scripts; ./install.sh -m ..
  [4] : ./install.sh -c ~/.local/share/cask -x

EOF
    printf $_RESET
}

# die message stdout
die() {
    printf "$_RED [-] Error: $@" >&2; exit 1
}

# verify if a command exists
command_exists() {
    command -v "$@" >/dev/null 2>&1
}

# check dependencies
check_deps() {
    command_exists git || die 'git not found';
    command_exists emacs || die "emacs not found";
    command_exists python || die "python not found";
}

# git clone
git_clone() {
    # params
    local _url=$1
    local _folder=$2

    # if folder do not exists, git clone
    test ! -d $_folder && \
        git clone $_url $_folder > /dev/null 2>&1

    # cloned?
    test ! -d $_folder && \
        die "was not possible to clone eos repo"

    # ok: cloned!
    return 0
}

# generate random string
get_rand_str() {
    # generate
    cat /dev/urandom | \
        env LC_CTYPE=C tr -dc a-z0-9 | \
        head -c 8; \
        echo
}

# backup ~/.emacs.d dir
backup_emacs() {
    # already a symlink?
    test -h ${_EMACS_DIR} && {
        unlink ${_EMACS_DIR}
        return 0
    }

    # backup
    test -d ${_EMACS_DIR} && \
        mv ${_EMACS_DIR} $_EMACS_BACKUP
}

# handle eos/src symlink
handle_symlink() {
    # make symlink
    ln -s ${_EOS_DIR}/src ${_EMACS_DIR};
}

# handle eos/src copy
copy_eos() {
    # if .emacs.d do not exists, copy
    test ! -d ${_EMACS_DIR} && \
        cp -rp ${_EOS_DIR}/src ${_EMACS_DIR}
}

# this function will copy the default files
# to the right location: ~/.emacs.d
copy_defaults() {
    # copy default files
    for _f in $_DEFAULT_FILES; do
        # default file
        _file=${_DEFAULT_DIR}/${_DEFAULT_PREFIX}${_f}

        # destination
        _dest=${_EMACS_DIR}/${_f}

        # if exist, continue
        test -f $_dest && \
            continue;

        # else copy
        cp -p $_file $_dest
    done
}

# set colors if file descriptor is associated with a terminal
setup_colors() {
    # True if the file whose file descriptor number is
    # file_descriptor is open and is associated with a terminal.
    # test(1)
    test -t 1 && {
        _RED="\033[31m"
        _GREEN="\033[32m"
        _YELLOW="\033[33m"
        _RESET="\033[m"
    }
}

# handle cask
setup_cask() {
    # clone cask repository (if necessary)
    test ! -d $_CASK_DIR && {
        git_clone $_CASK_REPO $_CASK_DIR

        # if directory was clonned,
        test ! $? -eq 0 && \
            die "was not possible to dump cask repository."
    }
}

# download eos
setup_eos() {
    # clone eos repository (if necessary)
    test ! -d $_EOS_DIR && {
        # dump eos
        git_clone $_EOS_REPO $_EOS_DIR

        test ! $? -eq 0 && \
            die "was not possible to clone eos repository."
    }
}

# install emacs packages listed on Cask file
# OBS: uses cask binary
install_packages () {
    (cd ${_EMACS_DIR}; ${_CASK_DIR}/bin/cask install)
}

# build: org tangle and compile
build_eos() {
    # load build.sh
    . ${_EOS_DIR}/scripts/build.sh

    # calls cask build
    (cd ${_EMACS_DIR}; ${_CASK_DIR}/bin/cask build)
}

# todo: define options
parse_opts() {
    # get options
    for opt in "$@"; do
        case $opt in
            # enable debugging
            -x) shift 1; set -x ;;

            # enable verbose
            -v) shift 1; set -v ;;

            # set cask target directory
            -c)
                shift 1; _CASK_DIR=`readlink -f "${1}"`; shift 1 ;;

            # set eos target directory
            -m)
                shift 1; _EOS_DIR=`readlink -f "${1}"`; shift 1 ;;

            # disable symlink? yes!
            -l)
                shift 1; _FLAG_SYMLINK=0 ;;

            # help message and exit
            --help|-h)
                banner; usage; exit 0 ;;
        esac
    done

    # update params
    _DEFAULT_DIR=${_EOS_DIR}/default

    # the get_rand_str contat is used to avoid conflicts
    # set emacs backup directory
    _EMACS_BACKUP=${_EMACS_DIR}.old.`get_rand_str`
}

show_opt_values() {
    # todo: replace this for a better syntax
    # todo: research sh(1) Parameter Expansion
    local _a="yes"

    # show banner
    banner

    # verify flags
    test $_FLAG_SYMLINK -eq 0 && \
        _a="no";

    test -h ${_EMACS_DIR} && \
        _EMACS_BACKUP="unlink"

    # print values
    printf $_GREEN
    printf \
        "[*] Options values:
  EOS dir  : $_EOS_DIR
  Cask dir : $_CASK_DIR
  Backup   : $_EMACS_BACKUP
  Symlink  : $_a
  \n"
    printf $_RESET
}

# show values and operations that will be executed to the user
# if we receive user's confirmation, then install eos
should_install() {
    local _answer=

    # question
    printf "[*] [h] help\n"
    printf "[*] Can we proceed with the installation? [y/n/h] : "

    while read _answer;
    do
        case $_answer in
            # possible answsers (yes, no, undefined (ask again!)
            y|yes) return 0 ;;

            # just leave
            n|no)
                printf "cya :)\n"; exit 0 ;;

            # show help and leave
            h|help) usage; exit 0 ;;

            *)
                # ask again
                printf "[*] Can we proceed with the installation? [y/n/h] : "
                ;;
        esac
    done
}

# install main
main() {
    # check dependencies (emacs, git and python)
    check_deps

    # parse options
    parse_opts $@

    # show options
    show_opt_values

    # install?
    should_install || exit 1;

    # setup eos and cask (package manager)
    setup_eos; setup_cask

    # backup ~/.emacs.d folder
    backup_emacs

    # handle symlink or copy?
    test $_FLAG_SYMLINK -eq 1 && \
        handle_symlink || copy_eos

    # copy default files
    copy_defaults

    # setup cask (package manager) and install emacs packages
    install_packages

    # build eos: tangle and compile
    build_eos

    # final message
    printf $_GREEN
    cat <<-'EOF'
     [+] Done!
     [+] Run emacs and enjoy! :)
EOF
}

# setup colors
setup_colors

# install.sh entry point
main $@
