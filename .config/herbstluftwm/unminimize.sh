#!/usr/bin/env bash
Mod=${Mod:-Mod4}
Minimizekey=n
Unminimizekey=Shift-n

hc() { "${herbstclient_command[@]:-herbstclient}" "$@" ;}


# initialize a global minimization counter
hc silent new_attr uint my_minimized_counter 1

hc keybind $Mod-$Minimizekey and \
  . try new_attr uint clients.focus.my_minimized_age \
  . substitute C my_minimized_counter set_attr clients.focus.my_minimized_age C \
  . set_attr my_minimized_counter "+=1" \
  . set_attr clients.focus.minimized true

# minimize focused window
# hc keybind $Mod-n chain \
#   . substitute C my_minimized_counter new_attr int clients.focus.my_minimized_age C \
#   . set_attr my_minimized_counter $(($(herbstclient substitute C my_minimized_counter echo C)+1)) \
#   . set_attr clients.focus.minimized true \

# hc keybind $Mod-n chain and \
  # . substitute C my_minimized_counter new_attr uint clients.focus.my_minimized_age C \
  # . set_attr my_minimized_counter "+=1" \
  # . set_attr clients.focus.minimized true

# unminimize last window of a tag
# if the `my_minimized_age` attribute does not exist (i.e. the window has not been
#  minimized with this script), use arbitrary order to unminimize
hc keybind $Mod-Shift-n mktemp string LASTCLIENTATT mktemp uint LASTAGEATT chain \
  . set_attr LASTAGEATT 0 \
  . foreach CLIENT clients. and \
    , sprintf MINATT "%c.minimized" CLIENT \
        compare MINATT "=" "true" \
    , sprintf TAGATT "%c.tag" CLIENT substitute FOCUS "tags.focus.name" \
        compare TAGATT "=" FOCUS \
    , sprintf AGEATT "%c.my_minimized_age" CLIENT or \
      case: and \
         : ! get_attr AGEATT \
         : compare LASTAGEATT "=" 0 \
      case: and \
         : substitute LASTAGE LASTAGEATT \
             compare AGEATT 'gt' LASTAGE \
         : substitute AGE AGEATT \
             set_attr LASTAGEATT AGE \
    , set_attr LASTCLIENTATT CLIENT \
  . and \
    , compare LASTCLIENTATT "!=" "" \
    , substitute CLIENT LASTCLIENTATT chain \
      : sprintf MINATT "%c.minimized" CLIENT \
          set_attr MINATT false \
      : sprintf AGEATT "%c.my_minimized_age" CLIENT \
          try remove_attr AGEATT
