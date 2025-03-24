
library ieee;
use ieee.std_logic_1164.all;

package oled is
  type oled_t is record
    vld_out  : std_logic;
    data_out : std_logic_vector(4*8-1 downto 0);
  end record;
end oled;
