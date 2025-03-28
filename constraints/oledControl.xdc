set_property PACKAGE_PIN Y9 [get_ports clk]
set_property IOSTANDARD LVCMOS18 [get_ports clk]
set_property PACKAGE_PIN U10 [get_ports oled_dc_n]
set_property PACKAGE_PIN U9 [get_ports oled_reset_n]
set_property PACKAGE_PIN AB12 [get_ports oled_spi_clk]
set_property PACKAGE_PIN AA12 [get_ports oled_spi_data]
set_property PACKAGE_PIN U11 [get_ports oled_vbat]
set_property PACKAGE_PIN U12 [get_ports oled_vdd]
set_property PACKAGE_PIN P16 [get_ports rst]
set_property IOSTANDARD LVCMOS18 [get_ports oled_dc_n]
set_property IOSTANDARD LVCMOS18 [get_ports oled_reset_n]
set_property IOSTANDARD LVCMOS18 [get_ports oled_spi_clk]
set_property IOSTANDARD LVCMOS18 [get_ports oled_spi_data]
set_property IOSTANDARD LVCMOS18 [get_ports oled_vbat]
set_property IOSTANDARD LVCMOS18 [get_ports oled_vdd]
set_property IOSTANDARD LVCMOS18 [get_ports rst]
set_property PACKAGE_PIN T22     [get_ports {console_logger_active}];  # "LD0"
set_property IOSTANDARD LVCMOS18 [get_ports {console_logger_active}];  # "LD0"
create_clock -period 10.000 -name clk -waveform {0.000 5.000} clk
