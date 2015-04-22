gshell () {
    if [[ -z $GSHELL_EXECUTABLE ]]
    then
        echo "Set \$GSHELL_EXECUTABLE"
        return 1
    fi
    case $1 in
    enter)
        to_cd=`$GSHELL_EXECUTABLE $@ | sed -e 's/.*\s.*\s//'` # add cheching for Left
        export GSHELL=true
        cd ${to_cd:0:-1}
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
    if [[ -n $GSHELL ]]
    then
        gshell commit `pwd` "`fc -n -l -1`"
        cd `pwd`
    fi
}
