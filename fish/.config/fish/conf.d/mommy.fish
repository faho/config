function mommy --on-event fish_postexec
    set -l stat $status
    set -l duration $CMD_DURATION
    # Don't show if the status didn't change (because of `set`).
    if test "$mommy_status_generation" = $status_generation
        return
    end
    set -g mommy_status_generation $status_generation

    set --default=SHELL_MOMMY_NEGATIVE_RESPONSES -l NEGATIVE_RESPONSES "do you need MOMMYS_ROLE's help~?" \
        "Don't give up, my love~" \
        "Don't worry, MOMMYS_ROLE is here to help you~" \
        "I believe in you, my sweet AFFECTIONATE_TERM~" \
        "It's okay to make mistakes, my dear~" \
        "just a ADJECTIVE further, sweetie~" \
        "Let's try again together, okay~?" \
        "MOMMYS_ROLE believes in you, and knows you can overcome this~" \
        "MOMMYS_ROLE believes in you~" \
        "MOMMYS_ROLE is always here for you, no matter what~" \
        "MOMMYS_ROLE is here to help you through it~" \
        "MOMMYS_ROLE is proud of you for trying, no matter what the outcome~" \
        "MOMMYS_ROLE knows it's tough, but you can do it~" \
        "MOMMYS_ROLE knows MOMMYS_PRONOUN ADJECTIVE AFFECTIONATE_TERM can do better~" \
        "MOMMYS_ROLE knows you can do it, even if it's tough~" \
        "MOMMYS_ROLE knows you're feeling down, but you'll get through it~" \
        "MOMMYS_ROLE knows you're trying your best~" \
        "MOMMYS_ROLE loves you, and is here to support you~" \
        "MOMMYS_ROLE still loves you no matter what~" \
        "You're doing your best, and that's all that matters to MOMMYS_ROLE~" \
        "MOMMYS_ROLE is always here to encourage you~ "

    set --default=SHELL_MOMMYS_POSITIVE_RESPONSES -l POSITIVE_RESPONSES "*pets your head*" \
        "awe, what a good AFFECTIONATE_TERM~\nMOMMYS_ROLE knew you could do it~" \
        "good AFFECTIONATE_TERM~\nMOMMYS_ROLE's so proud of you~" \
        "Keep up the good work, my love~" \
        "MOMMYS_ROLE is proud of the progress you've made~" \
        "MOMMYS_ROLE is so grateful to have you as MOMMYS_PRONOUN ADJECTIVE AFFECTIONATE_TERM~" \
        "I'm so proud of you, my love~" \
        "MOMMYS_ROLE is so proud of you~" \
        "MOMMYS_ROLE loves seeing MOMMYS_PRONOUN ADJECTIVE AFFECTIONATE_TERM succeed~" \
        "MOMMYS_ROLE thinks MOMMYS_PRONOUN ADJECTIVE AFFECTIONATE_TERM earned a big hug~" \
        "that's a good AFFECTIONATE_TERM~" \
        "you did an amazing job, my dear~" \
        "you're such a smart cookie~ "

    set --default=SHELL_MOMMYS_ROLE -l role mommy
    set --default=SHELL_MOMMYS_ADJECTIVE -l adjective little good lovely cute
    set --default=SHELL_MOMMYS_LITTLE -l little boy
    set --default=SHELL_MOMMYS_PRONOUNS -l pronouns her
    set --default=SHELL_MOMMYS_COLOR -l mommy_color FFB6C1 # lightpink
    set --default=SHELL_MOMMYS_ONLY_NEGATIVE -l only_negative false
    set --default=SHELL_MOMMYS_CHANCE -l mommy_chance_percent 10
    set --default=SHELL_MOMMYS_ALWAYS_SEC -l mommy_always_sec 30

    # My mushy brain's attempt to make mommy less likely to show up for shorter commands.
    set -l res $(math $mommy_chance_percent x "(($duration + 50) / 1000)")
    if test $(math $duration / 1000) -lt $mommy_always_sec && test "$(random 0 100)" -gt $res
        return
    end

    set -l response
    if test $stat = 0
        if test "$only_negative" = true
            return
        end
        set response (random choice $POSITIVE_RESPONSES)
    else
        set response (random choice $NEGATIVE_RESPONSES)
    end

    # This has a format injection vulnerability - if $SHELL_MOMMYS_PRONOUN includes the string "MOMMYS_ROLE"
    # that will be replaced by MOMMYS_ROLE. I've decided that we're gonna trust mommy.
    set response (string replace -a -- ADJECTIVE "$(random choice $adjective)" $response)
    set response (string replace -a -- AFFECTIONATE_TERM "$(random choice $little)" $response)
    set response (string replace -a -- MOMMYS_PRONOUN "$(random choice $pronouns)" $response)
    set response (string replace -a -- MOMMYS_ROLE "$(random choice $role)" $response)

    set_color $mommy_color
    printf $response\n
    set_color reset
end
