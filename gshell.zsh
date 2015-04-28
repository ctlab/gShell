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
    # set subsequent variables
    # gshell init -> nothing
    # gshell enter -> cd to output and set $GSHELL

    # manually switch gshell off by `unset GSHELL`
    isgshell=$(echo $1 | awk '{print $1;}')
    if [[ "$isgshell" = "gshell" ]]
    then
        return 0
    else
        if [[ -n $GSHELL ]]
        then
            gshell commit `pwd` "$1"
            cd `pwd`
        fi
    fi
}
