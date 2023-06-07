function machine
    qemu-system-x86_64 ~/Machines/$argv[1] -m 1280 -enable-kvm -net user,hostfwd=tcp::10022-:22 -net nic
end
