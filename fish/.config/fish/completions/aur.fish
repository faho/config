set -l cmds search clone download promote demote rm build install update log show
complete -c aur -f -n "not __fish_seen_subcommand_from $cmds" -a "$cmds"
complete -c aur -f -n "__fish_seen_subcommand_from promote" -a "(aur list --queueonly)"
complete -c aur -f -n "__fish_seen_subcommand_from demote" -a "(aur list --pkgsonly)"
complete -c aur -f -n "__fish_seen_subcommand_from rm" -a "(aur list --queueonly)"
complete -c aur -f -n "__fish_seen_subcommand_from build install" -a "(aur list)"
complete -c aur -f -n "__fish_seen_subcommand_from update" -a "(aur list)"
complete -c aur -f -n "__fish_seen_subcommand_from log" -a "(aur list)"
complete -c aur -f -n "__fish_seen_subcommand_from show" -a "(aur list)"
