function tompeg
	for i in $argv
		set -l IFS "="
		# set -l vc (mediainfo $i | grep "Video" -A 2 | grep "Format " | while read a b; echo $b; end)
		# set -l ac (mediainfo $i | grep "Audio" -A 2 | grep "Format " | while read a b; echo $b; end)
		set -l vc (ffprobe $i -show_entries "stream=codec_name" ^/dev/null -select_streams v | while read a b; echo $b; end)
		set -l ac (ffprobe $i -show_entries "stream=codec_name" ^/dev/null -select_streams a | while read a b; echo $b; end)
		set -e IFS
		# Remove extraneous characters
		echo $vc | read vc _
		echo $ac | read ac _
		set -l vcodec "libx264"
		set -l acodec "aac"
		# Don't re-encode unless necessary
		set -l goodaudio "aac"
		set -l goodvideo "h264"
		if contains -- $ac $goodaudio
			set acodec "copy"
		end
		if contains -- $vc $goodvideo
			set vcodec "copy"
		end
		echo "Video: " $vcodec $vc
		echo "Audio: " $acodec $ac
		ffmpeg -i $i -strict experimental -acodec $acodec -vcodec $vcodec -copyts $i.mp4
	end
end
