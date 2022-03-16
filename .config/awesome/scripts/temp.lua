awful = require("awful")
local update_interval = 5
local temp_script = [[
bash -c "
cat /sys/class/thermal/thermal_zone0/temp | xargs -I % python -c 'print(%/1000)' | cut -f1 -d .
"]]

awful.widget.watch(temp_script, update_interval, function(_, stdout)
  awesome.emit_signal("evil::temp", tonumber(stdout))
end)
