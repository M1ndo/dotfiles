#!/usr/bin/env bash

# a scratchpad terminal used for lyrics

tag="${1:-}"
hc() { "${herbstclient_command[@]:-herbstclient}" "$@" ;}

termwidth_percent=${WIDTH_PERC:-100}
mrect=( $(hc monitor_rect -p "" ) )
termwidth=$(( (${mrect[2]} * termwidth_percent) / 100 ))
termheight=${HEIGHT_PIXELS:-400}
echo $termwidth $termheight

rect=(
    $((500))
    $((700))
    $(( ${mrect[0]} + (450) ))
    $(( ${mrect[1]} + (550) ))
)

y_line=${mrect[1]}


hc add "$tag"


monitor=q3terminal

exists=false
if ! hc add_monitor $(printf "%dx%d%+d%+d" "${rect[@]}") \
                    "$tag" $monitor 2> /dev/null ; then
    exists=true
else
    # remember which monitor was focused previously
    hc chain \
        , new_attr string monitors.by-name."$monitor".my_prev_focus \
        , substitute M monitors.focus.index \
            set_attr monitors.by-name."$monitor".my_prev_focus M
fi

update_geom() {
    local geom=$(printf "%dx%d%+d%+d" "${rect[@]}")
    hc move_monitor "$monitor" $geom
}

steps=${ANIMATION_STEPS:-5}
interval=${ANIMATION_INTERVAL:-0.01}

animate() {
    progress=( "$@" )
    for i in "${progress[@]}" ; do
        rect[3]=$((y_line - (i * termheight) / steps))
        update_geom
        sleep "$interval"
    done
}

show() {
    hc lock
    hc raise_monitor "$monitor"
    hc focus_monitor "$monitor"
    hc unlock
    hc lock_tag "$monitor"
    animate $(seq $steps -1 0)
}

hide() {
    rect=( $(hc monitor_rect "$monitor" ) )
    local tmp=${rect[0]}
    rect[0]=${rect[2]}
    rect[2]=${tmp}
    local tmp=${rect[1]}
    rect[1]=${rect[3]}
    rect[3]=${tmp}
    termheight=${rect[1]}
    y_line=${rect[3]} # height of the upper screen border

    animate $(seq 0 +1 $steps)
    # if q3terminal still is focused, then focus the previously focused monitor
    # (that mon which was focused when starting q3terminal)
    hc substitute M monitors.by-name."$monitor".my_prev_focus \
        and + compare monitors.focus.name = "$monitor" \
            + focus_monitor M
    hc remove_monitor "$monitor"
}

[ $exists = true ] && hide || show

