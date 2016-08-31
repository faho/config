set -l cmds search clone download promote demote rm build install update log show
complete -c aur -f -n "not __fish_seen_subcommand_from $cmds" -a "$cmds"
complete -c aur -f -n "__fish_seen_subcommand_from promote" -a "(aur ls --queueonly)"
complete -c aur -f -n "__fish_seen_subcommand_from demote" -a "(aur ls --pkgsonly)"
complete -c aur -f -n "__fish_seen_subcommand_from rm" -a "(aur ls --queueonly)"
complete -c aur -f -n "__fish_seen_subcommand_from build install" -a "(aur ls)"
complete -c aur -f -n "__fish_seen_subcommand_from update" -a "(aur ls)"
complete -c aur -f -n "__fish_seen_subcommand_from log" -a "(aur ls)"
complete -c aur -f -n "__fish_seen_subcommand_from show" -a "(aur ls)"
