create_clock -period 10.000 -name CLK -waveform {0.000 5.000} [list CLK [get_ports -filter { NAME =~"*CLK*" && DIRECTION == "IN" }]]

