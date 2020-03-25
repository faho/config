# Defined in /tmp/fish.BbZ5a6/withnvidia.fish @ line 2
function withnvidia
    set -lx __NV_PRIME_RENDER_OFFLOAD 1
    set -lx __GLX_VENDOR_LIBRARY_NAME nvidia
    set -lx vblank_mode 0
    set -lx VK_ICD_FILENAMES /usr/share/vulkan/icd.d/nvidia_icd.json
    set -lx LIBVA_DRIVER_NAME vdpau
    set -lx VDPAU_DRIVER nvidia
    set -lx __NV_PRIME_RENDER_OFFLOAD_PROVIDER NVIDIA-G0
    set -lx __VK_LAYER_NV_optimus NVIDIA_only
    $argv
end
