def __do_this():
    try:
        import tty
    except ImportError:
        pass
    else:
        import sys
        import termios

        new_settings = termios.tcgetattr(0)
        new_settings[3] = new_settings[3] | termios.ECHO
        termios.tcsetattr(0, termios.TCSADRAIN, new_settings)


__do_this()
del __do_this
