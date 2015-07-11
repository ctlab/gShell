# Dont forget to compile with debug = false from Debug.hs

gshell () {
    if [[ -z $GSHELL_EXECUTABLE ]]
    then
        unset GSHELL
        echo "Set \$GSHELL_EXECUTABLE"
        return 1
    fi
    case $1 in
    off)
        unset GSHELL
        ;;
    on)
        GSHELL=true
        ;;
    checkout)
        to_cd=`$GSHELL_EXECUTABLE checkout $(pwd) $2 | tail -1 | awk '{print $2;}'` # add checking for Left
        cd "$to_cd"
        ;;
    enter | enterRev)
        to_cd=`$GSHELL_EXECUTABLE $@ | tail -1 | awk '{print $2;}'` # add checking for Left
        GSHELL=true OLD_ZDOTDIR="$ZDOTDIR" ZDOTDIR="`pwd`" TOCD="$to_cd" zsh -i
        ;;
    rollback)
        $GSHELL_EXECUTABLE rollback -p "`pwd`"
        cd "`pwd`"
        ;;
    pull)
        $GSHELL_EXECUTABLE pull -p "`pwd`"
        cd "`pwd`"
        ;;
    clear)
        unset GSHELL
        $GSHELL_EXECUTABLE $@
        ;;
    *)
        $GSHELL_EXECUTABLE $@
        ;;
    esac
}

preexec_gshell () {
    unset GSHELL_DONE
}

precmd_gshell () {
    if [[ -n $GSHELL_DONE ]]
    then
        return 0
    else
        isgshell=$(echo `fc -n -l -1` | awk '{print $1;}')
        if [[ "$isgshell" = "gshell" ]]
        then
            return 0
        else
            if [[ -n $GSHELL ]]
            then
                gshell commit "`fc -n -l -1`"
                cd "`pwd`"
                GSHELL_DONE=true
            fi
        fi
    fi
}

fpath=("`pwd`/completion" $fpath)

autoload -U compinit
compinit
 
# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2

autoload -U add-zsh-hook

add-zsh-hook preexec preexec_gshell
add-zsh-hook precmd precmd_gshell
