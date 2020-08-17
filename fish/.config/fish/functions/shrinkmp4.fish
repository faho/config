# Defined in /tmp/fish.VjExq6/shrinkmp4.fish @ line 2
function shrinkmp4
    set -q argv[2]; or return 1
    #     ffmpeg -i $argv[1] -c:v libx264 -crf 30 -preset medium -c:a copy -b:a 128k \
    # -movflags +faststart -vf scale=-2:720,format=yuv420p $argv[2]
    # ffmpeg -vaapi_device /dev/dri/renderD128 -hwaccel vaapi -hwaccel_output_format vaapi -threads 8 -i $argv[1] -c:a copy -b:a 128k -movflags +faststart -crf 30 -preset medium -vcodec h264_vaapi -vf format='nv12,hwupload,scale_vaapi=-2:720' $argv[2]
    ffmpeg -hwaccel vaapi -hwaccel_device /dev/dri/renderD128 -threads 8 -i $argv[1] -c:a copy -b:a 128k -c:v libx264 -movflags +faststart -crf 30 -preset medium -vf 'scale=-2:720' $argv[2]
end
