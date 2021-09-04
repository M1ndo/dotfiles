diff --git a/signal/playerctl/playerctl_cli.lua b/signal/playerctl/playerctl_cli.lua
index 93959f7..d1bfa87 100644
--- a/signal/playerctl/playerctl_cli.lua
+++ b/signal/playerctl/playerctl_cli.lua
@@ -58,9 +58,9 @@ if [ ! -d $tmp_dir  ]; then
     mkdir -p $tmp_dir
 fi
 
-link="$(playerctl metadata mpris:artUrl)"
+link="$(playerctl metadata mpris:artUrl | cut -d/ -f5)"
 
-curl -s "$link" --output $tmp_cover_path
+curl -s "https://i.scdn.co/image/$link" --output $tmp_cover_path
 
 echo "$tmp_cover_path"
 ']]
