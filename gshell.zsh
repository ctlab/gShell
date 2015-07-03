autoload -U add-zsh-hook

add-zsh-hook preexec preexec_gshell
add-zsh-hook precmd precmd_gshell

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
        to_cd=`$GSHELL_EXECUTABLE checkout $(pwd) $2 | tail -1 | awk '{print $2;}'` # add cheching for Left
        cd ${to_cd}
        ;;
    enter | enterRev)
        to_cd=`$GSHELL_EXECUTABLE $@ | tail -1 | awk '{print $2;}'` # add cheching for Left
        cd ${to_cd}
        export GSHELL=true
        ;;
    log)
        $GSHELL_EXECUTABLE log `pwd`
        ;;
    rollback)
        $GSHELL_EXECUTABLE rollback `pwd`
        cd `pwd`
        ;;
    graph)
        $GSHELL_EXECUTABLE graph `pwd`
        ;;
    push)
        $GSHELL_EXECUTABLE push `pwd`
        ;;
    pull)
        $GSHELL_EXECUTABLE pull `pwd`
        cd `pwd`
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
                gshell commit "`pwd`" "`fc -n -l -1`" # TODO Current directory
                cd `pwd`
                GSHELL_DONE=true
            fi
        fi
    fi
}

fpath=(`pwd`/completion $fpath)

autoload -U compinit
compinit
 
# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2
