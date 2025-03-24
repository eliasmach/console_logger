
--
-- Description : Prints strings to console_logger by calling
--               procedure 'log(...)'
--
-- Author      : Elias Machairas
-- Date        : 20/3/2025
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.oled.all;

entity oled_user is
  port   (
    clk              : in  std_logic;
    rst              : in  std_logic;

    -- console_logger interface
    oled             : out oled_t
    -- commit_data_out  : out std_logic_vector(7 downto 0);
    -- commit_vld_out   : out std_logic;
    -- commit_fifo_full : in  std_logic
  );
end oled_user;

architecture simple of oled_user is

  type state_t is (Idle, PrintString1, PrintString2, PrintString3, Done);
  signal current_state : state_t;
  signal next_state    : state_t;

  procedure log_init(signal debug_port: out oled_t) is
  begin
    debug_port.vld_out  <= '0';
    debug_port.data_out <= (others => '0');
  end procedure;

  -- Copied 'to_slv()' below from StackOverflow:
  -- https://stackoverflow.com/questions/22900938/vhdl-is-there-a-convenient-way-to-assign-ascii-values-to-std-logic-vector?noredirect=1&lq=1
  function to_slv(s: string) return std_logic_vector is 
    constant ss: string(1 to s'length) := s; 
    --variable answer: std_logic_vector(1 to 8 * s'length);
    variable answer: std_logic_vector(1 to 7 * s'length); 
    variable p: integer; 
    variable c: integer; 
  begin 
    for i in ss'range loop
      --p := 8 * i;
      p := 7 * i;
      c := character'pos(ss(i));
      --answer(p - 7 to p) := std_logic_vector(to_unsigned(c,8));
      answer(p - 6 to p) := std_logic_vector(to_unsigned(c,7)); 
    end loop; 
    return answer; 
  end function;

  procedure log(str: string; signal debug_port: out oled_t) is
  begin
    debug_port.vld_out  <= '1';
    -- TODO: Convert string to std_logic_vector
    debug_port.data_out <= to_slv(str) & "100" & "0";
  end procedure;

begin

  process(all) begin

    next_state     <= current_state;

    log_init(oled);

    case current_state is
      when Idle =>
        next_state <= PrintString1;
      when PrintString1 =>
        log("sta1", oled);
        next_state <= PrintString2;
      when PrintString2 =>
        log("sta2", oled);
        next_state <= PrintString3;
      when PrintString3 =>
        log("sta3", oled);
        next_state <= Done;
      when Done => -- halt
    end case;

  end process;

  process(clk) begin
    if rising_edge(clk) then
      if rst = '1' then
        current_state <= Idle;
      else
        current_state <= next_state;
      end if;
    end if;
  end process;

end simple;
