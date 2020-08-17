function hdmi_off
    pactl set-card-profile 0 output:analog-stereo
end
