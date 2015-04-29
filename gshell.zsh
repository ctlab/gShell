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
    enter)
        to_cd=`$GSHELL_EXECUTABLE $@ | tail -1 | sed -e 's/.*\s.*\s//'` # add cheching for Left
        cd ${to_cd:0:-2}
        export GSHELL=true
        ;;
    log)
        $GSHELL_EXECUTABLE log `pwd`
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

preexec () {
    unset GSHELL_DONE
}

precmd() {
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
                gshell commit "`pwd`" "`fc -n -l -1`"
                cd `pwd`
                GSHELL_DONE=true
            fi
        fi
    fi
}
