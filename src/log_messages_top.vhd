
library ieee;
use ieee.std_logic_1164.all;
use work.oled.all;

entity log_messages_top is
  port (
    clk                   : in  std_logic;
    rst                   : in  std_logic;
  
    console_logger_active : out std_logic;
    oled_spi_clk          : out std_logic;
    oled_spi_data         : out std_logic;
    oled_vdd              : out std_logic;
    oled_vbat             : out std_logic;
    oled_reset_n          : out std_logic;
    oled_dc_n             : out std_logic
  );
end log_messages_top;

architecture rtl of log_messages_top is
  signal oled : oled_t;
begin

  oled_user: entity work.oled_user
    port map (
      clk  => clk,
      rst  => rst,
      oled => oled
    );

  oled_controller: entity work.console_logger_v2
    port map (
      clk                   => clk,
      rst                   => rst,

      console_logger_active => console_logger_active,

      oled                  => oled,

      oled_spi_clk          => oled_spi_clk         ,
      oled_spi_data         => oled_spi_data        ,
      oled_vdd              => oled_vdd             ,
      oled_vbat             => oled_vbat            ,
      oled_reset_n          => oled_reset_n         ,
      oled_dc_n             => oled_dc_n            
    );

end rtl;
