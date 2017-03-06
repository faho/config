# NAME
#   battery [arguments]
#
# SYNOPSIS
#   OS X, Linux and termux compatible battery utility.
#
# USAGE
#   if available battery
#     battery
#   end
#
# ENV #   BATTERY_IS_PLUGGED
#   BATTERY_IS_CHARGING
#   BATTERY_TIME_LEFT     Time left in `HH:MM` format.
#   BATTERY_SLOTS         Number of slots/gauges in base 10.
#   BATTERY_MAX_CAP       Battery maximum capacity.
#   BATTERY_CUR_CAP       Battery current capacity.
#   BATTERY_PCT           Current battery life in %.
#
# ARGUMENTS
#   filled_slot_ch    ▮
#   empty_slot_ch     ▯
#   show_empty_slots  true

function init --on-event init_battery
  battery_update_info
end

function battery_update_info
	if command -s ioreg >/dev/null
		battery_update_info_darwin
	else if command -s upower >/dev/null
		battery_update_info_linux
    else if command -s termux-battery-status >/dev/null
        # Termux, an Android terminal emulator and debian-chrooty-thingy.
        # Warning: If the "termux:api" system package isn't installed,
        # `termux-battery-status` will hang forever.
        battery_update_info_termux
	end
end

function battery_update_info_termux
    # This is json output, looks like
    # {
    #     "health": "GOOD",
    #     "percentage": 32,
    #     "plugged": "UNPLUGGED", # Or "PLUGGED_AC"
    #     "status": "DISCHARGING", # Or "CHARGING"
    #     "temperature": 15.199999809265137
    # }
    set -l output (termux-battery-status)
    string match '"CHARGING"' -- $output
    and set -g BATTERY_IS_PLUGGED
    or set -e BATTERY_IS_PLUGGED

    # No capacity info availabe, so just fake it.
    set -g BATTERY_MAX_CAP 100
    set -g BATTERY_CUR_CAP (string match -r '"percentage":.*' -- $output \
    | string replace -r '"percentage".* (\d+),.*' '$1')
    set -g BATTERY_PCT $BATTERY_CUR_CAP
	set -g BATTERY_SLOTS (math "$BATTERY_PCT" / 10)
end

function battery_update_info_linux
    # Cache the devices, they usually won't change.
    if not set -q __battery_upower_devices
	    set -g __battery_upower_devices (upower -e | string match '*battery*') # This can theoretically be multiple
    end
	set -l upo (upower -i $__battery_upower_devices)

	# Mind the space in " charging", we want to match "charging" but not "discharging"
	printf "%s"\n $upo | string match -rq "state:.* charging\|state:.*fully-charged"
	and set -g BATTERY_IS_PLUGGED
	or set -e  BATTERY_IS_PLUGGED

	printf "%s"\n $upo | string match -rq "state:.* charging"
    and set -g BATTERY_IS_CHARGING
    or set -e  BATTERY_IS_CHARGING

	set -g BATTERY_MAX_CAP (printf "%s\n" $upo \
	| string match -r "\s*energy-full:.*" \
	| string replace -r '\s*energy-full:\s*' '')

	set -g BATTERY_CUR_CAP (printf "%s\n" $upo \
	| string match -r "\s*energy:.*" \
	| string replace -r '\s*energy:\s*' '')

	set -g BATTERY_PCT (printf "%s\n" $upo \
	| string match -r '\s*percentage:.*' \
	| string replace -r '\s*percentage:\s*(.*)%' '$1')

	set -g BATTERY_TIME_LEFT (printf "%s\n" $upo \
	| string match -r ".*time to.*:.*" \
	| string replace -r '.*time to.*:\s*' '')

    string match -qr '^[0-9]+$' -- $BATTERY_PCT; or return
	set -g BATTERY_SLOTS (math "$BATTERY_PCT" / 10)
end

function battery_update_info_darwin
  set -l ioreg (ioreg -rc "AppleSmartBattery")

  printf "%s"\n $ioreg \
  | grep -q "\"ExternalConnected\" = Yes"
    and set -g BATTERY_IS_PLUGGED
    or set -e BATTERY_IS_PLUGGED

  printf "%s"\n $ioreg \
  | grep -q "\"IsCharging\" = Yes"
    and set -g BATTERY_IS_CHARGING
    or set -e  BATTERY_IS_CHARGING

  set -g BATTERY_MAX_CAP (printf "%s\n" $ioreg \
  | grep "\"MaxCapacity\" =" \
  | sed -e 's/^.*"MaxCapacity"\ =\ //')

  set -g BATTERY_CUR_CAP (printf "%s\n" $ioreg \
  | grep "\"CurrentCapacity\" =" \
  | sed -e 's/^.*CurrentCapacity"\ =\ //')
  set -g BATTERY_PCT (printf "%.1f" \
    (echo "$BATTERY_CUR_CAP / $BATTERY_MAX_CAP * 100" | bc -l))

  set -g BATTERY_TIME_LEFT (printf "%s\n" $ioreg \
  | grep "\"AvgTimeToEmpty\" =" \
  | sed -e 's/^.*"AvgTimeToEmpty"\ =\ //')
  set BATTERY_TIME_LEFT (printf "%02d:%02d" \
    (echo "$BATTERY_TIME_LEFT / 60" | bc) \
    (echo "$BATTERY_TIME_LEFT % 60" | bc))

  set -g BATTERY_SLOTS (math $BATTERY_PCT / 10)
end

function battery -a \
  filled_slot_ch    \
  empty_slot_ch     \
  show_empty_slots  \
  red yellow green

  if test -z "$filled_slot_ch"
	set filled_slot_ch "█|"
	set filled_slot_ch "▮"
  end

  if test -z "$empty_slot_ch"
    set empty_slot_ch " |"
    set empty_slot_ch "▯"
  end

  if test -z "$show_empty_slots"
    set show_empty_slots true
  end

  if test -z "$red"
    set red (set_color -o f00)
  end

  if test -z "$yellow"
    set yellow (set_color -o ff0)
  end

  if test -z "$green"
    set green (set_color -o 0f0)
  end

  set -l normal   (set_color normal)
  set -l color    $green

  battery_update_info
  set -q BATTERY_SLOTS; or return
  string match -qr '^[0-9]+$' -- $BATTERY_SLOTS; or return
  switch "$BATTERY_SLOTS"
    case 0 1 2; set color $red
    case 3 4;   set color $yellow
    case "*";   set color $green
  end

  for n in (seq 10)
    if test $n -eq "$BATTERY_SLOTS"
		if type -q bar
			printf "$color"
			bar (string sub -s -1 -l 1 $BATTERY_PCT)0
			printf "$normal"
		else
			printf "$color$filled_slot_ch$normal"
		end
    else if test $n -lt "$BATTERY_SLOTS"
		printf "$color$filled_slot_ch$normal"
	else
      if test $show_empty_slots = true
        printf "$color$empty_slot_ch$normal"
      end
    end
  end
end
