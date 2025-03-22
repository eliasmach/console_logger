-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:expandtab
--------------------------------------------------------------------------------
-- Module Name : p_fifo
-- Description : Generic size, depth, BRAM/LUTRAM, FWFT/non-FWFT FIFO.
-- Author      : antts, DSCAL NKUA
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
-- Xilinx UG901 page 93:
-- Dual port dual clock BRAM
--------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
entity generic_BRAM_dual is
  generic (g_dwidth : integer := 16;
           g_awidth : integer := 6);
  port(clka         : in  std_logic;
       clkb         : in  std_logic;
       ena          : in  std_logic;
       enb          : in  std_logic;
       wea          : in  std_logic;
       addra, addrb : in  std_logic_vector(g_awidth-1 downto 0);
       dia          : in  std_logic_vector(g_dwidth-1 downto 0);
       dob          : out std_logic_vector(g_dwidth-1 downto 0));
end generic_BRAM_dual;
architecture syn of generic_BRAM_dual is
  type ram_type is array ((2**g_awidth)-1 downto 0) of std_logic_vector (g_dwidth-1 downto 0);
  shared variable RAM        : ram_type;
  attribute ram_style        : string;
  attribute ram_style of RAM : variable is "block";
  -- Synopsis
  attribute syn_ramstyle        : string;
  attribute syn_ramstyle of RAM : variable is "block_ram";
begin
  -- pragma translate_off
	-- TODO this looks like a bug, these assertions SHOULD NOT trigger
	-- The input assertions indicate a bug in the fifo source logic so these
	-- should be warnings BUT the assertions on X in the output indicate a bug in
	-- the FIFO logic!
	-- TODO check this!
  process(clka)
  begin
    if rising_edge(clka) then
      if wea = '1'  and ena = '1' then
        assert (not is_x(addra))
          report syn'instance_name &
          "Write address in is undefined during an input write enable."
          severity warning;
        assert (not is_x(dia))
          report syn'instance_name &
          "Data in is undefined during an input write enable."
          severity warning;
      end if;
    end if;
  end process;
  process(clkb)
  begin
    if rising_edge(clkb) then
      if enb = '1' then
        assert (not is_x(addrb))
          report syn'instance_name &
          "Read address is undefined during a output read enable."
          severity warning;
        assert (not is_x(dob))
          report syn'instance_name &
          "Data out is undefined during a output read enable."
          severity warning;
      end if;
    end if;
  end process;
  -- pragma translate_on

  process (clka)
  begin
    if rising_edge(clka) then
      if ena = '1' then
        if wea = '1' then
          RAM(conv_integer(addra)) := dia;
        end if;
      end if;
    end if;
  end process;
  process (clkb)
  begin
    if rising_edge(clkb) then
      if enb = '1' then
        dob <= RAM(conv_integer(addrb));
      end if;
    end if;
  end process;
end syn;
--------------------------------------------------------------------------------
-- Xilinx UG901 page 98:
-- Distributed Dual-Port RAM with Asynchronous Read
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity generic_lutram is
  generic (C_DWIDTH : integer := 32;
           C_AWIDTH : integer := 16);
  port (clk   : in  std_logic;
        wea   : in  std_logic;
        addra : in  std_logic_vector(C_AWIDTH-1 downto 0);
        enb   : in  std_logic;
        addrb : in  std_logic_vector(C_AWIDTH-1 downto 0);
        dia   : in  std_logic_vector(C_DWIDTH-1 downto 0);
        dob   : out std_logic_vector(C_DWIDTH-1 downto 0));
end generic_lutram;

architecture syn of generic_lutram is
  type ram_type is array ((2**C_AWIDTH)-1 downto 0) of
    std_logic_vector ((C_DWIDTH-1) downto 0);
  signal RAM                    : ram_type;
  attribute ram_style           : string;
  attribute ram_style of RAM    : signal is "distributed";
  -- Synopsis
  attribute syn_ramstyle        : string;
  attribute syn_ramstyle of RAM : signal is "select_ram";

  signal read_dpra : std_logic_vector(C_AWIDTH-1 downto 0);
begin
  -- pragma translate_off
	-- TODO this looks like a bug, these assertions SHOULD NOT trigger
	-- The input assertions indicate a bug in the fifo source logic so these
	-- should be warnings BUT the assertions on X in the output indicate a bug in
	-- the FIFO logic!
	-- TODO check this!
  process(clk)
  begin
    if rising_edge(clk) then
      if wea = '1' then
        assert (not is_x(addra))
          report syn'instance_name &
          "Write address in is undefined during an input write enable."
          severity warning;
        assert (not is_x(dia))
          report syn'instance_name &
          "Data in is undefined during an input write enable."
          severity warning;
      end if;
      if enb = '1' then
        assert (not is_x(addrb))
          report syn'instance_name &
          "Read address is undefined during a output read enable."
          severity warning;
        assert (not is_x(dob))
          report syn'instance_name &
          "Data out is undefined during a output read enable."
          severity warning;
      end if;
    end if;
  end process;
  -- pragma translate_on

  process (clk)
  begin
    if rising_edge(clk) then
      if (wea = '1') then
        RAM(to_integer(unsigned(addra))) <= dia;
      end if;
      if enb = '1' then
        read_dpra <= addrb;
      end if;
    end if;
  end process;
  dob <= RAM(to_integer(unsigned(read_dpra)));
end syn;
--------------------------------------------------------------------------------
-- Module Name : basic_fifo
-- Description : Simple fifo (non-fwft)
-- Author      : antts, DSCAL NKUA
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.numeric_std.all;
entity basic_fifo is
  generic (g_wordsize  : integer              := 16;  -- Word size in bits
           g_fifodepth : integer              := 16;  -- FIFO depth in slots
           g_fifotech  : integer range 0 to 1 := 1;  -- 0 ditributed RAM, 1 BlockRAM
           g_pfull     : integer              := 0;  -- Programmable full threshold
           g_pempty    : integer              := 0);  -- Programmable empty threshold
  port (clk    : in  std_logic;
        srst   : in  std_logic;         -- synchronous reset
        wr_en  : in  std_logic;         -- write enable
        din    : in  std_logic_vector(g_wordsize-1 downto 0);  -- data in
        full   : out std_logic;         -- FIFO full
        rd_en  : in  std_logic;         -- read enable
        dout   : out std_logic_vector(g_wordsize-1 downto 0);  -- data out
        empty  : out std_logic;         -- FIFO empty
        afull  : out std_logic;         -- FIFO almost full
        aempty : out std_logic;         -- FIFO almost empty
        pfull  : out std_logic;         -- FIFO programmable full
        pempty : out std_logic);        -- FIFO programmable empty
end basic_fifo;

architecture rtl of basic_fifo is

  function log2(x : natural) return integer is
    variable i : integer := 0;
  begin
    if x = 0 then return 0;
    else
      while 2**i < x loop
        i := i+1;
      end loop;
      return i;
    end if;
  end function log2;

  constant c_address_width : integer := log2(g_fifodepth);

  signal s_read_addr  : std_logic_vector(0 to c_address_width-1) := (others => '0');
  signal s_write_addr : std_logic_vector(0 to c_address_width-1) := (others => '0');

  -- Qualified write/read enable
  signal s_we_q : std_logic;
  signal s_rd_q : std_logic;

  type regs is record
    rd_addr_ptr : natural range 0 to 2**c_address_width-1;
    wr_addr_ptr : natural range 0 to 2**c_address_width-1;
    full        : std_logic;
    empty       : std_logic;
    afull       : std_logic;
    aempty      : std_logic;
    pempty      : std_logic;
    pfull       : std_logic;
    fifo_len    : natural range 0 to g_fifodepth;
  end record;

  -- Nullary record
  constant c_regs_none : regs := (
    rd_addr_ptr => 0,
    wr_addr_ptr => 0,
    full        => '0',
    empty       => '1',
    afull       => '0',
    aempty      => '1',
    pempty      => '1',
    pfull       => '0',
    fifo_len    => 0);

  signal r, s_rin : regs := c_regs_none;

begin

  ------------------------------------------------------------------------------
  -- Control process in gaisler style, we load the register into the variable at
  -- the beginning and write the variable to the register input at the end. Only
  -- sensitive to the qualified read and write enables which avoids illegal
  -- reading or writing.
  ------------------------------------------------------------------------------
  FIFO_ctrl_proc : process(r, s_we_q, s_rd_q)
    variable v : regs;
  begin
    -- load state
    v := r;

    assert g_pempty < g_fifodepth and g_pempty >= 0
      report "basic_fifo programmable empty must be smaller than fifo depth"
      severity failure;
    assert g_pfull < g_fifodepth and g_pfull >= 0
      report "basic_fifo programmable full must be smaller than fifo depth"
      severity failure;

    ----------------------------------------------------------------------------
    -- Fifo length, we calculate this first and everything else is either
    -- derived from this value or the previous fifo_len in the register.
    ----------------------------------------------------------------------------
    if (s_we_q = '1' and s_rd_q = '0' and r.full = '0') then
      -- Write and no read => increment length
      -- Don't increment length when full
      v.fifo_len := r.fifo_len + 1;
    elsif (s_we_q = '0' and s_rd_q = '1' and r.empty = '0') then  -- OK
      -- Read and no write => decrement length
      -- Don't decrement length when empty
      v.fifo_len := r.fifo_len - 1;
    end if;

    ----------------------------------------------------------------------------
    -- Full flag
    ----------------------------------------------------------------------------
    if ((s_rd_q = '1') and (s_we_q = '0') and r.full = '1') then
      -- we were previously full and just got a read
      -- this is the first non-full
      v.full := '0';
    elsif ((s_rd_q = '0') and (s_we_q = '1') and
           ((r.fifo_len = (g_fifodepth - 1)))) then
      -- we are reading out the last word
      v.full := '1';
    elsif (r.fifo_len = g_fifodepth) then
      -- normal operation
      v.full := '1';
    else
      v.full := '0';
    end if;
    -- Almost full
    if ((s_rd_q = '1') and (s_we_q = '0') and r.full = '1') then
      v.afull := '1';
    elsif ((s_rd_q = '0') and (s_we_q = '1') and
           ((r.fifo_len = (g_fifodepth - 1)))) then
      v.afull := '1';
    elsif ((s_rd_q = '1') and (s_we_q = '0') and
           r.fifo_len = (g_fifodepth - 1)) then
      v.afull := '0';
    elsif ((s_rd_q = '0') and (s_we_q = '1') and
           ((r.fifo_len = (g_fifodepth - 2)))) then
      v.afull := '1';
    elsif (r.fifo_len = g_fifodepth-1) then
      v.afull := '1';
    elsif (r.full = '1') then
      v.afull := '1';
    else
      v.afull := '0';
    end if;
    ----------------------------------------------------------------------------
    -- Empty flag
    ----------------------------------------------------------------------------
    if ((s_rd_q = '0') and (s_we_q = '1') and r.empty = '1') then
      -- we were previously empty and just got a write
      -- this is the first non-empty
      v.empty := '0';
    elsif ((s_rd_q = '1') and (s_we_q = '0') and (r.fifo_len = 1)) then
      -- we are reading out the last word
      v.empty := '1';
    elsif (r.empty = '1') then
      -- normal operation
      v.empty := '1';
    else
      v.empty := '0';
    end if;
    -- Almost empty
    if ((s_rd_q = '0') and (s_we_q = '1') and r.empty = '1') then
      v.aempty := '1';
    elsif ((s_rd_q = '0') and (s_we_q = '1') and r.fifo_len = 1) then
      v.aempty := '0';
    elsif ((s_rd_q = '1') and (s_we_q = '0') and ((r.fifo_len = 2))) then
      v.aempty := '1';
    elsif ((s_rd_q = '1') and (s_we_q = '0') and ((r.fifo_len = 1))) then
      v.aempty := '1';
    elsif (r.fifo_len = 1) then
      v.aempty := '1';
    elsif (r.empty = '1') then
      v.aempty := '1';
    else
      v.aempty := '0';
    end if;
    ----------------------------------------------------------------------------
    -- Programmable flags
    -- When fifo level is greater or equal to g_pfull or less or equal to
    -- g_pempty.
    ----------------------------------------------------------------------------
    if g_pfull > 0 then
      if (r.fifo_len >= g_pfull) then
        v.pfull := '1';
      else
        v.pfull := '0';
      end if;
    else
      v.pfull := '0';
    end if;
    if g_pempty > 0 then
      if (r.fifo_len <= g_pempty) then
        v.pempty := '1';
      else
        v.pempty := '0';
      end if;
    else
      v.pempty := '0';
    end if;
    ----------------------------------------------------------------------------
    -- Pointers
    ----------------------------------------------------------------------------
    -- Increment read address pointer
    if s_rd_q = '1' then
      v.rd_addr_ptr := (v.rd_addr_ptr + 1) mod (2 ** c_address_width);
    end if;
    -- Increment write address pointer
    if (s_we_q = '1') then  -- dont check full, we're using the qualified wr_en
      v.wr_addr_ptr := (v.wr_addr_ptr + 1) mod (2 ** c_address_width);
    end if;

    -- next state
    s_rin <= v;
  end process;

  ------------------------------------------------------------------------------
  -- Gaisler style register process
  ------------------------------------------------------------------------------
  regs_proc : process (clk)
  begin
    if rising_edge(clk) then
      if (srst = '1') then
        r <= c_regs_none;
      else
        r <= s_rin;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  full   <= r.full;
  empty  <= r.empty;
  afull  <= r.afull;
  aempty <= r.aempty;
  pfull  <= r.pfull;
  pempty <= r.pempty;

  s_write_addr <= std_logic_vector(to_unsigned(r.wr_addr_ptr, c_address_width));
  s_read_addr  <= std_logic_vector(to_unsigned(r.rd_addr_ptr, c_address_width));

  s_we_q <= '1' when wr_en = '1' and r.full = '0'  else '0';
  s_rd_q <= '1' when rd_en = '1' and r.empty = '0' else '0';

  ------------------------------------------------------------------------------

  BRAM_generate : if g_fifotech = 1 generate
    generic_BRAM_dual_1 : entity work.generic_BRAM_dual
      generic map (g_dwidth => g_wordsize,
                   g_awidth => c_address_width)
      port map (clka  => clk,
                clkb  => clk,
                ena   => '1',
                enb   => s_rd_q,
                wea   => s_we_q,
                addra => s_write_addr,
                addrb => s_read_addr,
                dia   => din,
                dob   => dout);
  end generate;
  distram_generate : if g_fifotech = 0 generate
    I_generic_lutram : entity work.generic_lutram
      generic map (C_DWIDTH => g_wordsize,
                   C_AWIDTH => c_address_width)
      port map (clk   => clk,
                wea   => s_we_q,
                addra => s_write_addr,
                dia   => din,
                enb   => s_rd_q,
                addrb => s_read_addr,
                dob   => dout);
  end generate;

end rtl;
--------------------------------------------------------------------------------
-- Module Name : fifo_nofwft
-- Description : basic_fifo wrapper with extra output regs to improve timing
-- Author      : antts, DSCAL NKUA
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.numeric_std.all;
entity fifo_nofwft is
  generic (g_wordsize  : integer              := 16;
           g_fifodepth : integer              := 16;
           g_fifotech  : integer range 0 to 1 := 1;
           g_pfull     : integer              := 0;
           g_pempty    : integer              := 0);
  port (clk    : in  std_logic;
        srst   : in  std_logic;
        wr_en  : in  std_logic;
        din    : in  std_logic_vector(g_wordsize-1 downto 0);
        full   : out std_logic;
        rd_en  : in  std_logic;
        dout   : out std_logic_vector(g_wordsize-1 downto 0);
        empty  : out std_logic;
        afull  : out std_logic;
        aempty : out std_logic;
        pfull  : out std_logic;
        pempty : out std_logic);
end fifo_nofwft;

architecture rtl of fifo_nofwft is

  -- we neet to modify these because we add depth to the basic fifo
  constant c_bfifo_depth  : integer := g_fifodepth-2;
  constant c_bfifo_pempty : integer := g_pempty;
  constant c_bfifo_pfull  : integer := g_pfull;

  -- regs
  signal was_empty, s_empty       : std_logic;
  signal fifo_valid, middle_valid : std_logic;
  signal middle_dout              : std_logic_vector(g_wordsize-1 downto 0);
  -- wires
  signal fifo_dout                : std_logic_vector(g_wordsize-1 downto 0);
  signal fifo_empty, fifo_rd_en   : std_logic;
  signal will_update_middle       : std_logic;
  signal will_update_dout         : std_logic;

begin

  basic_fifo_1 : entity work.basic_fifo
    generic map (g_wordsize  => g_wordsize,
                 g_fifodepth => c_bfifo_depth,
                 g_fifotech  => g_fifotech,
                 g_pfull     => c_bfifo_pfull,
                 g_pempty    => c_bfifo_pempty)
    port map (clk    => clk,
              srst   => srst,
              wr_en  => wr_en,
              din    => din,
              full   => full,
              rd_en  => fifo_rd_en,
              dout   => fifo_dout,
              empty  => fifo_empty,
              aempty => open,
              afull  => afull,
              pempty => pempty,
              pfull  => pfull);

  process(fifo_valid, middle_valid, will_update_dout, rd_en, fifo_empty, s_empty, was_empty)
    variable v_empty : std_logic;
  begin

    if (fifo_valid = '1') and (middle_valid = will_update_dout) then
      will_update_middle <= '1';
    else
      will_update_middle <= '0';
    end if;

    if ((middle_valid or fifo_valid) and rd_en) = '1' then
      will_update_dout <= '1';
    else
      will_update_dout <= '0';
    end if;

    if (fifo_empty = '0') and ((middle_valid and fifo_valid) = '0') then
      fifo_rd_en <= '1';
    else
      fifo_rd_en <= '0';
    end if;

    if (not (fifo_valid or middle_valid)) = '1' then
      v_empty := '1';
    else
      v_empty := '0';
    end if;
    s_empty <= v_empty;
    empty   <= s_empty;

    if ((fifo_valid and fifo_empty) xor (middle_valid and fifo_empty)) = '1' then
      aempty <= '1';
    elsif was_empty = '1' and v_empty = '0' then  -- empty falling edge
      aempty <= '1';
    else
      aempty <= v_empty;
    end if;

  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      if (srst = '1') then
        fifo_valid   <= '0';
        middle_valid <= '0';
        dout         <= (others => '0');
        middle_dout  <= (others => '0');
        was_empty    <= '0';
      else
        was_empty <= s_empty;

        if (will_update_middle = '1') then
          middle_dout <= fifo_dout;
        end if;

        if (will_update_dout = '1' and middle_valid = '1') then
          dout <= middle_dout;
        elsif (will_update_dout = '1' and middle_valid = '0') then
          dout <= fifo_dout;
        end if;

        if (fifo_rd_en = '1') then
          fifo_valid <= '1';
        elsif (will_update_middle or will_update_dout) = '1' then
          fifo_valid <= '0';
        end if;

        if (will_update_middle = '1') then
          middle_valid <= '1';
        elsif (will_update_dout = '1') then
          middle_valid <= '0';
        end if;

      end if;
    end if;
  end process;
end rtl;

--------------------------------------------------------------------------------
-- Module Name : fifo_fwft
-- Description : basic_fifo wrapper with extra output regs to improve timing
--                 and convert to fwft.
-- Author      : antts, DSCAL NKUA
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.numeric_std.all;
entity fifo_fwft is
  generic (g_wordsize  : integer              := 16;
           g_fifodepth : integer              := 16;
           g_fifotech  : integer range 0 to 1 := 1;
           g_pfull     : integer              := 0;
           g_pempty    : integer              := 0);
  port (clk    : in  std_logic;
        srst   : in  std_logic;
        wr_en  : in  std_logic;
        din    : in  std_logic_vector(g_wordsize-1 downto 0);
        full   : out std_logic;
        rd_en  : in  std_logic;
        dout   : out std_logic_vector(g_wordsize-1 downto 0);
        empty  : out std_logic;
        afull  : out std_logic;
        aempty : out std_logic;
        pfull  : out std_logic;
        pempty : out std_logic);
end fifo_fwft;

architecture rtl of fifo_fwft is

  -- we neet to modify these because we add depth to the basic fifo
  constant c_bfifo_depth  : integer := g_fifodepth-3;
  constant c_bfifo_pempty : integer := g_pempty;
  constant c_bfifo_pfull  : integer := g_pfull;

  -- regs
  signal fifo_valid, middle_valid : std_logic;
  signal dout_valid               : std_logic;
  signal middle_dout              : std_logic_vector(g_wordsize-1 downto 0);
  -- wires
  signal fifo_dout                : std_logic_vector(g_wordsize-1 downto 0);
  signal fifo_empty               : std_logic;
  signal fifo_rd_en               : std_logic;
  signal will_update_middle       : std_logic;
  signal will_update_dout         : std_logic;

begin

  basic_fifo_1 : entity work.basic_fifo
    generic map (g_wordsize  => g_wordsize,
                 g_fifodepth => c_bfifo_depth,
                 g_fifotech  => g_fifotech,
                 g_pfull     => c_bfifo_pfull,
                 g_pempty    => c_bfifo_pempty)
    port map (clk    => clk,
              srst   => srst,
              wr_en  => wr_en,
              din    => din,
              full   => full,
              rd_en  => fifo_rd_en,
              dout   => fifo_dout,
              empty  => fifo_empty,
              aempty => open,
              afull  => afull,
              pempty => pempty,
              pfull  => pfull);

  process(fifo_valid, middle_valid, will_update_dout, rd_en, fifo_empty, dout_valid)
    variable v_single_valid : std_logic;
  begin

    if ((fifo_valid = '1') and (middle_valid = will_update_dout)) then
      will_update_middle <= '1';
    else
      will_update_middle <= '0';
    end if;

    if ((middle_valid or fifo_valid) and (rd_en or (not dout_valid))) = '1' then
      will_update_dout <= '1';
    else
      will_update_dout <= '0';
    end if;

    if (fifo_empty = '0') and ((middle_valid and dout_valid and fifo_valid) = '0') then
      fifo_rd_en <= '1';
    else
      fifo_rd_en <= '0';
    end if;

    if (dout_valid = '0') then
      empty <= '1';
    else
      empty <= '0';
    end if;

    if (dout_valid = '1' and fifo_valid = '0' and middle_valid = '0') or
      (dout_valid = '0' and fifo_valid = '1' and middle_valid = '0') or
      (dout_valid = '0' and fifo_valid = '0' and middle_valid = '1') then
      v_single_valid := '1';
    else
      v_single_valid := '0';
    end if;

    -- if ((dout_valid and (not fifo_valid) and fifo_empty) xor (middle_valid and fifo_empty)) = '1' then
    if (v_single_valid and fifo_empty) = '1' then
      aempty <= '1';
    else
      aempty <= not dout_valid;
    end if;

  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      if (srst = '1') then
        fifo_valid   <= '0';
        middle_valid <= '0';
        dout_valid   <= '0';
        dout         <= (others => '0');
        middle_dout  <= (others => '0');
      else
        if (will_update_middle = '1') then
          middle_dout <= fifo_dout;
        end if;


        if (will_update_dout) = '1' then
          if middle_valid = '1' then
            dout <= middle_dout;
          else
            dout <= fifo_dout;
          end if;
        end if;

        if (fifo_rd_en = '1') then
          fifo_valid <= '1';
        elsif (will_update_middle or will_update_dout) = '1' then
          fifo_valid <= '0';
        end if;

        if (will_update_middle = '1') then
          middle_valid <= '1';
        elsif (will_update_dout = '1') then
          middle_valid <= '0';
        end if;

        if will_update_dout = '1' then
          dout_valid <= '1';
        elsif rd_en = '1' then
          dout_valid <= '0';
        end if;

      end if;
    end if;
  end process;
end rtl;

--------------------------------------------------------------------------------
-- Module Name : p_fifo
-- Description : Top level entity to expose the FWFT select as generic
-- Author      : antts, DSCAL NKUA
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.numeric_std.all;
entity p_fifo is
  generic (g_wordsize  : integer              := 16;  -- Word size in bits
           g_fifodepth : integer              := 16;  -- FIFO depth in slots
           g_fifotech  : integer range 0 to 1 := 1;  -- 0 ditributed RAM, 1 BlockRAM
           g_pfull     : integer              := 0;  -- Approx. ±1 Programmable full threshold
           g_pempty    : integer              := 0;  -- Approx. ±1 Programmable empty threshold
           g_fwft      : integer range 0 to 1 := 1);  -- 0 normal FIFO, 1 FWFT
  port (clk    : in  std_logic;
        srst   : in  std_logic;         -- synchronous reset
        wr_en  : in  std_logic;         -- write enable
        din    : in  std_logic_vector(g_wordsize-1 downto 0);  -- data in
        full   : out std_logic;         -- FIFO full
        rd_en  : in  std_logic;         -- read enable
        dout   : out std_logic_vector(g_wordsize-1 downto 0);  -- data out
        empty  : out std_logic;         -- FIFO empty
        afull  : out std_logic;         -- FIFO almost full
        aempty : out std_logic;         -- FIFO almost empty
        pfull  : out std_logic;         -- Approx. ±1 FIFO programmable full
        pempty : out std_logic);        -- Approx. ±1 FIFO programmable empty
end p_fifo;

architecture RTL of p_fifo is
begin

  -- pragma translate_off
  process(clk)
  begin
    if rising_edge(clk) then
      if wr_en = '1' and full = '0' then
        assert (not is_x(din))
          report RTL'instance_name &
          " Data in is undefined during a valid input handshake."
          severity warning;
      end if;
      if wr_en = '1' and full = '1' then
        assert (not is_x(din))
          report RTL'instance_name &
          " Data in is undefined during a write enable while full."
          severity warning;
      end if;
      if rd_en = '1' and empty = '0' then
        assert (not is_x(din))
          report RTL'instance_name &
          " Data out is undefined during a valid output handshake."
          severity warning;
      end if;
      if rd_en = '1' and empty = '1' then
        assert (not is_x(din))
          report RTL'instance_name &
          " Data out is undefined during a read enable while empty."
          severity warning;
      end if;
    end if;
  end process;
  -- pragma translate_on

  nofwft_gen : if g_fwft = 0 generate
    fifo_nofwft_1 : entity work.fifo_nofwft
      generic map (g_wordsize  => g_wordsize,
                   g_fifodepth => g_fifodepth,
                   g_fifotech  => g_fifotech,
                   g_pfull     => g_pfull,
                   g_pempty    => g_pempty)
      port map (clk    => clk,
                srst   => srst,
                wr_en  => wr_en,
                din    => din,
                full   => full,
                rd_en  => rd_en,
                dout   => dout,
                empty  => empty,
                aempty => aempty,
                afull  => afull,
                pempty => pempty,
                pfull  => pfull);
  end generate;
  fwft_gen : if g_fwft = 1 generate
    fifo_fwft_1 : entity work.fifo_fwft
      generic map (g_wordsize  => g_wordsize,
                   g_fifodepth => g_fifodepth,
                   g_fifotech  => g_fifotech,
                   g_pfull     => g_pfull,
                   g_pempty    => g_pempty)
      port map (clk    => clk,
                srst   => srst,
                wr_en  => wr_en,
                din    => din,
                full   => full,
                rd_en  => rd_en,
                dout   => dout,
                empty  => empty,
                aempty => aempty,
                afull  => afull,
                pempty => pempty,
                pfull  => pfull);
  end generate;

end RTL;
--------------------------------------------------------------------------------
-- Module Name : p_fifo_eos
-- Description : p_fifo wrapper with separate EOS port
-- Author      : antts, DSCAL NKUA
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.numeric_std.all;
entity p_fifo_eos is
  generic (g_wordsize  : integer              := 16;  -- Word size in bits
           g_fifodepth : integer              := 16;  -- FIFO depth in slots
           g_fifotech  : integer range 0 to 1 := 1;  -- 0 ditributed RAM, 1 BlockRAM
           g_pfull     : integer              := 0;  -- Approx. ±1 Programmable full threshold
           g_pempty    : integer              := 0;  -- Approx. ±1 Programmable empty threshold
           g_fwft      : integer range 0 to 1 := 1);  -- 0 normal FIFO, 1 FWFT
  port (clk    : in  std_logic;
        srst   : in  std_logic;         -- synchronous reset
        -- Input I/F
        wr_en  : in  std_logic;         -- write enable
        eos_i  : in  std_logic;         -- EOS in
        din    : in  std_logic_vector(g_wordsize-1 downto 0);  -- data in
        full   : out std_logic;         -- FIFO full
        afull  : out std_logic;         -- FIFO almost full
        pfull  : out std_logic;         -- Approx. ±1 FIFO programmable full
        -- Output I/F
        rd_en  : in  std_logic;         -- read enable
        dout   : out std_logic_vector(g_wordsize-1 downto 0);  -- data out
        empty  : out std_logic;         -- FIFO empty
        eos_o  : out std_logic;         -- EOS out
        aempty : out std_logic;         -- FIFO almost empty
        pempty : out std_logic);        -- Approx. ±1 FIFO programmable empty
end p_fifo_eos;

architecture RTL of p_fifo_eos is
  signal fifo_din, fifo_dout : std_logic_vector(g_wordsize downto 0);
begin
  -- pragma translate_off
  process(clk)
  begin
    if rising_edge(clk) then
      if wr_en = '1' and full = '0' then
        assert (not is_x(fifo_din))
          report RTL'instance_name &
          " Data in is undefined during a valid input handshake."
          severity warning;
      end if;
      if wr_en = '1' and full = '1' then
        assert (not is_x(fifo_din))
          report RTL'instance_name &
          " Data in is undefined during a write enable while full."
          severity warning;
      end if;
    end if;
  end process;
  -- pragma translate_on

  fifo_din <= din & eos_i;
  dout     <= fifo_dout(fifo_dout'length-1 downto 1);
  eos_o    <= fifo_dout(0);

  nofwft_gen : if g_fwft = 0 generate
    fifo_nofwft_1 : entity work.fifo_nofwft
      generic map (g_wordsize  => g_wordsize+1,
                   g_fifodepth => g_fifodepth,
                   g_fifotech  => g_fifotech,
                   g_pfull     => g_pfull,
                   g_pempty    => g_pempty)
      port map (clk    => clk,
                srst   => srst,
                wr_en  => wr_en,
                din    => fifo_din,
                full   => full,
                rd_en  => rd_en,
                dout   => fifo_dout,
                empty  => empty,
                aempty => aempty,
                afull  => afull,
                pempty => pempty,
                pfull  => pfull);
  end generate;
  fwft_gen : if g_fwft = 1 generate
    fifo_fwft_1 : entity work.fifo_fwft
      generic map (g_wordsize  => g_wordsize+1,
                   g_fifodepth => g_fifodepth,
                   g_fifotech  => g_fifotech,
                   g_pfull     => g_pfull,
                   g_pempty    => g_pempty)
      port map (clk    => clk,
                srst   => srst,
                wr_en  => wr_en,
                din    => fifo_din,
                full   => full,
                rd_en  => rd_en,
                dout   => fifo_dout,
                empty  => empty,
                aempty => aempty,
                afull  => afull,
                pempty => pempty,
                pfull  => pfull);
  end generate;

end RTL;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rdy_vld_FIFO is
  generic (g_wordsize  : integer;-- := 16;      -- Word size in bits
           g_fifodepth : integer;-- := 16;      -- FIFO depth in slots
           g_fifotech  : integer);-- := 1);      -- 0 ditributed RAM, 1 BlockRAM
  port (clk   : in  std_logic;
        rst   : in  std_logic;
        -- Input I/F
        we    : in  std_logic;
        rdy   : out std_logic;
        eos_i : in  std_logic;
        din   : in  std_logic_vector(g_wordsize-1 downto 0);
        -- Output I/F
        re    : in  std_logic;
        vld   : out std_logic;
        eos_o : out std_logic;
        dout  : out std_logic_vector(g_wordsize-1 downto 0));
end rdy_vld_FIFO;

architecture Behavioral of rdy_vld_FIFO is
  signal fifo_full, fifo_empty : std_logic;
  signal fifo_din,fifo_dout : std_logic_vector(g_wordsize downto 0);
begin
  fifo_din <= din & eos_i;
  dout     <= fifo_dout(fifo_dout'length-1 downto 1);
  eos_o    <= fifo_dout(0);
  rdy      <= not fifo_full;
  vld      <= not fifo_empty;
  p_FIFO_inst : entity work.p_fifo
    generic map (g_wordsize  => g_wordsize+1,
                 g_fifodepth => g_fifodepth,
                 g_fifotech  => g_fifotech,
                 g_pfull     => 0,
                 g_pempty    => 0,
                 g_fwft      => 1)
    port map (clk    => clk,
              srst   => rst,
              wr_en  => we,
              din    => fifo_din,
              full   => fifo_full,
              rd_en  => re,
              dout   => fifo_dout,
              empty  => fifo_empty,
              afull  => open,
              aempty => open,
              pfull  => open,
              pempty => open
    );
end Behavioral;
