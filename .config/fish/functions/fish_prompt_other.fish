# count the numer of files in current the directory
# funxction _count_files
#     ls -1 | wc -l | sed 's/\ //g'
# end

function _git_branch_name
    echo (command git symbolic-ref HEAD 2> /dev/null | sed -e 's|^refs/heads/||')
end

function _git_is_dirty
    echo (command git status -s --ignore-submodules=dirty 2> /dev/null)
end

function fish_prompt
    # set last status (shell return)
    set -l last_status $status

    # setup colors
    set -l white (set_color cccccc)
    set -l cyan (set_color cyan)
    set -l yellow (set_color yellow)
    set -l red (set_color red)
    set -l blue (set_color blue)
    set -l green (set_color green)
    set -l normal (set_color normal)

    # get current working directory
    set -l cwd $blue(pwd | sed "s:^$HOME:~:")

    # display [venvname] if in a virtualenv
    if set -q VIRTUAL_ENV
        echo -n -s (set_color -b cyan black) '[' (basename "$VIRTUAL_ENV") ']' $normal ' '
    end

    # show git branch and status
    if [ (_git_branch_name) ]
        set -l git_branch (_git_branch_name)

        # default git info
        set git_info '(' $green $git_branch $normal ')'

        # set colors if git is dirty
        if [ (_git_is_dirty) ]
            set git_info '(' $yellow $git_branch "±" $normal ')'
        end
    end

    # default prompt color (red, error)
    set -l prompt_color $red

    # set prompt color if last status was ok
    if test $last_status = 0
        set prompt_color $normal
    end

    # set prompt default symbol
    set -l prompt_symbol ' » '

    # clean prompt format if TERM it's equals to dump
    if test $TERM = "dumb"
        set promt_symbol "\$ "
    end

    if test $USER = "root"
        set prompt_symbol ' # '
    end

    # finaly set the promt format
    set -l prompt_format $prompt_color $cwd $cyan $prompt_symbol $normal

    # display prompt
    echo -e -n -s $prompt_format

end
