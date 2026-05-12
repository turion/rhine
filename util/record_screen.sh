#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full xorg.xwininfo

echo "Select the window to record"

xwininfo

echo "Copy paste window id:"

read WID

echo "Enter file name (without ending):"

read FILENAME

echo "Press q when done"

ffmpeg -f x11grab -window_id $WID -show_region 1 -i :0.0 -c:v libx264rgb -crf 0 $FILENAME.mpg

echo "Wait now, GIF is being generated, don't press q"

ffmpeg -i $FILENAME.mpg -vf "fps=30,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" $FILENAME.gif
