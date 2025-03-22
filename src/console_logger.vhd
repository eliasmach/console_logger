library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity console_logger is
  port (
    clk                   : in  std_logic;
    rst                   : in  std_logic;

    -- to be connected to a led
    console_logger_active : out std_logic;

    -- console_logger client interface
    -- 8 = 7 payload bits + 1 header bits
    -- TODO: Make header, payload sizes more abstract
    --       inside a package file
    commits_fifo_data_in  : in  std_logic_vector(7 downto 0);
    commits_fifo_vld_in   : in  std_logic;
    commits_fifo_full     : out std_logic;

    -- oled interface
    oled_spi_clk          : out std_logic;
    oled_spi_data         : out std_logic;
    oled_vdd              : out std_logic;
    oled_vbat             : out std_logic;
    oled_reset_n          : out std_logic;
    oled_dc_n             : out std_logic
  );
end console_logger;

architecture simple of console_logger is
  type state_t is (Idle, ConsumeCommit, ExecuteDataCommit, ExecuteCommandCommit);
  signal current_state : state_t;
  signal next_state    : state_t;

  -- consume the input fifo
  signal commits_fifo_data_out : std_logic_vector(7 downto 0);
  signal commits_fifo_empty    : std_logic;
  signal commits_fifo_rd_en    : std_logic;

  -- drive the oled controller
  signal sendData              : std_logic_vector(7 downto 0);
  signal sendDataValid         : std_logic;
  signal sendDone              : std_logic;

  signal chars_counter         : unsigned(7 downto 0);
  signal chars_counter_wr_en   : std_logic;

  signal commit                : std_logic_vector(7 downto 0);

begin

  commits_fifo_inst: entity work.p_fifo
     generic map (
        g_wordsize  => 8,     -- Word size in bits
        g_fifodepth => 64,    -- FIFO depth in slots
        g_fifotech  => 0,     -- 0 ditributed RAM, 1 BlockRAM
        g_pfull     => 0,     -- Approx. ±1 Programmable full threshold
        g_pempty    => 0,     -- Approx. ±1 Programmable empty threshold
        g_fwft      => 1      -- 0 normal FIFO, 1 FWFT
     )
     port map (
        clk         => clk,  
        srst        => rst,   -- synchronous reset

        wr_en       => commits_fifo_vld_in,
        din         => commits_fifo_data_in,
        full        => commits_fifo_full,

        rd_en       => commits_fifo_rd_en,
        dout        => commits_fifo_data_out,
        empty       => commits_fifo_empty,

        afull       => open,  -- FIFO almost full
        aempty      => open,  -- FIFO almost empty
        pfull       => open,  -- Approx. ±1 FIFO programmable full
        pempty      => open   -- Approx. ±1 FIFO programmable empty
  );

  oledControl_inst: entity work.oledControl --_vhdl_wrapper
    port map (
      clk            => clk            ,
      rst            => rst            ,
      oled_spi_clk   => oled_spi_clk   ,
      oled_spi_data  => oled_spi_data  ,
      oled_vdd       => oled_vdd       ,
      oled_vbat      => oled_vbat      ,
      oled_reset_n   => oled_reset_n   ,
      oled_dc_n      => oled_dc_n      ,
      sendData       => sendData       ,
      sendDataValid  => sendDataValid  ,
      sendDone       => sendDone       
    );

    -- Drive only the payload bits (6 down to 0) to the oled controller
    -- NOTE: The header bits (7 down to 7) are for us to know the type
    --       of the incoming commit
    sendData <= commit(6 downto 0);

    process(all) begin

      next_state            <= current_state;
      console_logger_active <= '1';
      commits_fifo_rd_en    <= '0';
      sendDataValid         <= '0';
      commit_wr_en          <= '0';

      case current_state is
        when Idle =>
          console_logger_active <= '0';
          next_state            <= ConsumeCommit;

        -- Reads a commit (entry) from the fifo and depending on the type
        -- of the commit (Data or Command) jumps to the corresponding state
        -- for handling it
        when ConsumeCommit =>
          sendDataValid <= '0';
          if commits_fifo_empty = '0' and sendDone = '0' then
            commits_fifo_rd_en <= '1';
            commit_wr_en       <= '1'; -- store fifo entry to 'commit' register
            if commits_fifo_data_out(7) = '0' then -- Data entry
              next_state <= ExecuteDataCommit;
            else -- Command entry 
              next_state <= ExecuteCommandCommit;
            end if;
          end if;
          
        -- Waits for 7 bits to be written serially to the SPI
        when ExecuteDataCommit =>
          sendDataValid <= '1';
          if sendDone = '1' then
            chars_counter_wr_en <= '1';
            next_state          <= ConsumeCommit;
          end if;

        when ExecuteCommandCommit => 
          next_state <= Idle; -- For now we do NOT support Type='Command' packets.
                              -- We just jump to the Idle state where the "active"
                              -- led should go off
      end case;
          
    end process;

    process(clk)
    begin
      if rising_edge(clk) then
        if rst = '1' then
          chars_counter <= (others => '0');
        elsif chars_counter_wr_en = '1' then
          chars_counter <= chars_counter + 1;
        end if;
      end if;
    end process;

    process(clk)
    begin
      if rising_edge(clk) then
        if rst = '1' then
          current_state <= Idle;
        else
          current_state <= next_state;
          if commit_wr_en = '1' then
            commit <= commits_fifo_data_out;
          end if;
        end if;
      end if;
    end process;

end simple;
