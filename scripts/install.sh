#!/usr/bin/env sh

# colors
_RED=""
_GREEN=""
_RESET=""
_YELLOW=""

_VLM_URL="https://github.com/lambdart/vlm"

# primary directories
_VLM_DEST="$HOME/.vlm"
_EMACS_DIR="$HOME/.emacs.d"

# default directory
_VLM_DEFAULT_DIR=${_VLM_DEST}/default/

# lisp directories
_VLM_SITE_DIR=${_VLM_DEST}/src/site-lisp

# emacs backup
_EMACS_BACKUP=

# flags
# create .emacs.d backup?
# create .emacs.d symlink?
# 0 means no
# 1 means yes
_BACKUP_FLAG=1
_SYMLINK_FLAG=1

# set colors if file descriptor is associated with a terminal
setup_colors()
{
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

# print banner
banner()
{
    cat <<-'EOF'
      (`-.             _   .-')
    _(OO  )_          ( '.( OO )_
,--(_/   ,. \ ,--.     ,--.   ,--.)
\   \   /(__/ |  |.-') |   `.'   |
 \   \ /   /  |  | OO )|         |
  \   '   /,  |  |`-' ||  |'.'|  |
   \     /__)(|  '---.'|  |   |  |
    \   /     |      | |  |   |  |
     `-'      `------' `--'   `--'
                 ... installation script
EOF
}

# show help message
usage()
{
    printf $_GREEN
    cat <<-'EOF'

  sh install.sh [-hlx] dest

  -h    show help message and exit
  -x    enable debugging
  -v    enable verbose
  -l    disables symlink (.emacs.d -> vlm/src)

  dest  vlm default destination path

  Examples:

  [1] : ./install.sh
  [2] : ./install.sh -h
  [3] : ./install.sh .
  [3] : cd vlm/scripts; ./install.sh ..

EOF
    printf $_RESET
}

# die message stdout
die()
{
    printf "$_RED [-] Error: $@" >&2; exit 1
}

# verify if a command exists
command_exists()
{
    command -v "$@" >/dev/null 2>&1
}

# check dependencies
check_deps()
{
    command_exists git || die 'git not found';
    command_exists emacs || die "emacs not found";
}

# git clone
git_clone()
{
    # params
    local _url=$1
    local _folder=$2

    # if folder do not exists, git clone
    test ! -d $_folder && \
        git clone -q $_url $_folder

    # cloned?
    test ! -d $_folder && \
        die "was not possible to clone EOS"
}

# generate random string
gen_rand_str()
{
    # generate
    cat /dev/urandom | \
        env LC_CTYPE=C tr -dc a-z0-9 | \
        head -c 8; \
        echo
}

# backup emacs default folder
# usually (~/.emacs.d)
backup_emacs()
{
    # already a symlink, if so, just unlink it
    test -h ${_EMACS_DIR} && {
        unlink ${_EMACS_DIR}
        return 0
    }

    # otherwise rename .emacs.d folder
    test -d ${_EMACS_DIR} && \
        mv ${_EMACS_DIR} $_EMACS_BACKUP
}

# create symlink
create_symlink()
{
    ln -s ${_VLM_DEST}/src ${_EMACS_DIR};
}

# copy vlm/src directory
copy_vlm()
{
    # if .emacs.d do not exists, copy vlm source
    test ! -d ${_EMACS_DIR} && \
        cp -rp ${_VLM_DEST}/src ${_EMACS_DIR}
}

# this function will copy the default files
# to the right location: ~/.emacs.d
copy_subdir()
{
    # create directories if they do not exists (site-lisp, lisp)
    mkdir ${_VLM_SITE_DIR}

    # copy subdirs to lisp and site-lisp directories
    cp -p ${_VLM_DEFAULT_DIR}/default.subdirs.el ${_VLM_SITE_DIR}/subdirs.el
}

# download vlm
clone_vlm()
{
    # clone vlm repository (if necessary)
    test ! -d $_VLM_DEST && {
        # dump vlm
        git_clone $_VLM_URL $_VLM_DEST

        test ! $? -eq 0 && \
            die "was not possible to clone EOS"
    }
}

# git clone list of packages
# clone_packages ()
# {
#     # add packages (libraries) list
#     # the list of git repositories
#     . packages.sh

#     for package in $@; do
#         git_clone package;
#     done;
# }

# build vlm, i.e, tangle the source code blocks
# from the org file and byte-compile the generated
# init.el final script
make_vlm()
{
    make -C core/dev/vlm/src all
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

            # disable symlink? yes!
            -l)
                shift 1; _SYMLINK_FLAG=0 ;;

            # help message and exit
            --help|-h)
                banner; usage; exit 0 ;;
        esac
    done

    # set vlm target directory
    _VLM_DEST=`readlink -f "${1}"`;;

    # update vlm default directory
    _VLM_DEFAULT_DIR=${_VLM_DEST}/default

    # the gen_rand_str contat is used to avoid conflicts
    # set emacs backup directory
    _EMACS_BACKUP=${_EMACS_DIR}.old.`gen_rand_str`
}

show_opts()
{
    local _flag="no"

    # show banner
    banner

    # verify flags
    test $_SYMLINK_FLAG -gt 0 && \
        _flag="yes";

    test -h ${_EMACS_DIR} && \
        _EMACS_BACKUP="unlink"

    # print values
    printf $_GREEN
    printf \
        "[*] Options values:
  EOS_DIR : $_VLM_DEST
  BACKUP  : $_EMACS_BACKUP
  SYMLINK : $_flag
  \n"
    printf $_RESET
}

# show values and operations that will be executed to the user
# if we receive user's confirmation, then install vlm
confirm()
{
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

install_vlm()
{
    # clone vlm repository
    clone_vlm;

    # handle symlink or copy?
    test $_SYMLINK_FLAG -eq 1 && \
        create_symlink || copy_vlm

    # copy default files
    copy_defaults

    # clone packages
    clone_packages

    # build vlm: call make to tangle and compile
    make_vlm
}

# main: handle logic
main()
{
    # parse options
    parse_opts $@

    # show options
    show_opts

    # should install?
    confirm || exit 1;

    # handle emacs backup
    backup_emacs

    # install vlm, i.e, download the repositories
    # and copy files to the right location
    install_vlm

    # final message
    printf $_GREEN
    cat <<-'EOF'
     [+] Done!
     [+] Run emacs and enjoy! :)
EOF
}

# check dependencies (emacs and git)
check_deps

# setup colors
setup_colors

# install.sh entry point
main $@
