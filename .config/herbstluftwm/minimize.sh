#!/bin/bash

herbstclient silent new_attr int my_minimized_counter 1

herbstclient chain \
  . substitute C my_minimized_counter new_attr int clients.focus.my_minimized_age C \
  . set_attr my_minimized_counter $(($(herbstclient substitute C my_minimized_counter echo C)+1)) \
  . set_attr clients.focus.minimized true \
