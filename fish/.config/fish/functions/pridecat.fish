function pridecat
    set -l lgbt ff0018 ffa52c ffff41 008018 0000f9 86007d
    set -l lgbt_1978 ff69b4 ff0000 ff8e00 ffff00 008e00 00c0c0 400098 8e008e
    set -l lgbtpoc 000000 784F17 E40303 FF8C00 FFED00 008026 004DFF 750787
    set -l trans 5BCEFA F5A9B8 FFFFFF F5A9B8 5BCEFA
    set -l bi D60270 D60270 9B4F96 0038A8 0038A8
    set -l ace 000000 A3A3A3 FFFFFF 800080
    set -l aro 3DA642 A8D379 FFFFFF A9A9A9 000000
    set -l pan FF218C FF218C FFD800 FFD800 21B1FF 21B1FF
    set -l nb FFF430 FFFFFF 9C59D1 000000
    set -l lipstick A40061 B75592 D063A6 EDEDEB E4ACCF C54E54 8A1E04
    set -l new_lesbian D52D00 EF7627 FF9A56 FFFFFF D162A4 B55590 A30262
    set -l community_lesbian D52D00 FF9A56 FFFFFF D362A4 A30262
    set -l genderqueer B57EDC B57EDC FFFFFF FFFFFF 4A8123 4A8123
    set -l poly 0000ff 0000ff ff0000 ff0000 000000 000000

    set -l vars lgbt{,_1978,poc} trans bi ace aro pan nb lipstick {new,community}_lesbian genderqueer poly
    set -l var (set -q argv[1]; and echo $argv[1]; or random choice $vars)
    if not set -q $var
        echo Wrong color $var
        return
    end
        
    set -l cols $$var

    while read -l line
        if not string trim -- $line | string length -q
            echo
            continue
        end
        set -l col 0x(string match -ra '..' -- $cols[1])
        set -l bg ffffff
        if test (math 0.2126 x $col[1] + 0.7152 x $col[2] + 0.0722 x $col[3]) -gt 150
            set bg 000000
        end


        echo (set_color $bg -b $cols[1])$line(set_color reset)
        set -e cols[1]
        if not set -q cols[1]
            set cols $$var
        end
    end
end
