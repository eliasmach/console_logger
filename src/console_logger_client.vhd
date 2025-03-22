
--
-- Description : Just prints a constant string to console_logger
--
-- Author      : Elias Machairas
-- Date        : 20/3/2025
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity console_logger_client is
  port (
    clk              : in  std_logic;
    rst              : in  std_logic;

    -- console_logger interface
    commit_data_out  : out std_logic_vector(7 downto 0);
    commit_vld_out   : out std_logic;
    commit_fifo_full : in  std_logic
  );
end console_logger_client;

architecture simple of console_logger_client is

  type state_t is (Idle, PrintConstantString, Done);
  signal current_state : state_t;
  signal next_state    : state_t;

  -- signal output        : std_logic_vector(39 downto 0);
  constant msg         : string(14 downto 0)   := "client says hi ";
  constant msg_length  : unsigned(15 downto 0) := to_unsigned(14, 16);
  signal chars_cnt     : unsigned(15 downto 0);
  signal chars_cnt_en  : std_logic;
  signal chars_cnt_rst : std_logic;

begin

  process(all) begin

    next_state     <= current_state;
    commit_vld_out <= '0';
    chars_cnt_en   <= '0';
    chars_cnt_rst  <= '0';

    -- output(39 downto 32) <= std_logic_vector(to_unsigned(character'pos(msg(1)), 8));
    -- output(31 downto 24) <= std_logic_vector(to_unsigned(character'pos(msg(2)), 8));
    -- output(23 downto 16) <= std_logic_vector(to_unsigned(character'pos(msg(3)), 8));
    -- output(15 downto 8 ) <= std_logic_vector(to_unsigned(character'pos(msg(4)), 8));
    -- output( 7 downto 0 ) <= std_logic_vector(to_unsigned(character'pos(msg(5)), 8));
    -- commit_data_out <= msg(chars_cnt);
    commit_data_out <= std_logic_vector(to_unsigned(character'pos(msg(to_integer(chars_cnt))), 8)); 

    case current_state is
      when Idle =>
        next_state <= PrintConstantString;
      when PrintConstantString =>
        if chars_cnt = 0 then
          chars_cnt_rst <= '1';
          next_state    <= Done;
        elsif commit_fifo_full = '0' then
          commit_vld_out <= '1';
          chars_cnt_en   <= '1';
        end if;
      when Done => -- halt
    end case;

  end process;

  process(clk) begin
    if rising_edge(clk) then
      if rst = '1' or chars_cnt_rst = '1' then
        chars_cnt <= msg_length;
      elsif chars_cnt_en = '1' then
        chars_cnt <= chars_cnt - 1;
      end if;
    end if;
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
