library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.oled.all;

entity console_logger_v2 is
  port (
    clk                   : in  std_logic;
    rst                   : in  std_logic;

    -- to be connected to a led
    console_logger_active : out std_logic;

    -- console_logger client interface
    -- 32 = 7*4=28 payload bits + 3 length bits + 1 type bit 
    -- TODO: Make header, payload sizes more abstract
    --       inside a package file
    -- commits_fifo_data_in  : in  std_logic_vector(31 downto 0);
    -- commits_fifo_vld_in   : in  std_logic;
    -- commits_fifo_full     : out std_logic;
    oled                  : in  oled_t;

    -- oled interface
    oled_spi_clk          : out std_logic;
    oled_spi_data         : out std_logic;
    oled_vdd              : out std_logic;
    oled_vbat             : out std_logic;
    oled_reset_n          : out std_logic;
    oled_dc_n             : out std_logic
  );
end console_logger_v2;

architecture simple of console_logger_v2 is
  type state_t is (Idle, ConsumeCommit, ExecuteDataCommit, ExecuteDataCommit_WaitSendDone, ExecuteCommandCommit);
  signal current_state : state_t;
  signal next_state    : state_t;

  -- consume the input fifo
  signal commits_fifo_data_out : std_logic_vector(31 downto 0);
  signal commits_fifo_empty    : std_logic;
  signal commits_fifo_rd_en    : std_logic;

  -- drive the oled controller
  signal sendData              : std_logic_vector(6 downto 0);
  signal sendDataValid         : std_logic;
  signal sendDone              : std_logic;

  signal chars_counter         : unsigned(7 downto 0);
  signal chars_counter_wr_en   : std_logic;

  -- signal commit                : std_logic_vector(31 downto 0);
  signal commit_data           : std_logic_vector(27 downto 0);
  signal commit_length_minus1  : unsigned( 2 downto 0);
  signal commit_type           : std_logic;
  signal commit_wr_en          : std_logic;

  function mux_vec(signal data_vec  : in std_logic_vector(27 downto 0);
                   signal counter   : in unsigned( 1 downto 0)
                  ) return std_logic_vector is
    subtype slv7_t  is std_logic_vector(6 downto 0);
    type    slvv7_t is array(3 downto 0) of slv7_t;
    variable data_array : slvv7_t;
  begin

    -- (Step 1/2) Convert vector to array
    for i in 3 downto 0 loop
      data_array(i) := data_vec((i+1)*7-1 downto i*7);
    end loop;

    -- (Step 2/2) Index the array
    return data_array(to_integer(counter));

  end function;

begin

  commits_fifo_inst: entity work.p_fifo
     generic map (
        g_wordsize  => 32,    -- Word size in bits
        g_fifodepth => 64,    -- FIFO depth in slots
        g_fifotech  => 0,     -- 0 ditributed RAM, 1 BlockRAM
        g_pfull     => 0,     -- Approx. ±1 Programmable full threshold
        g_pempty    => 0,     -- Approx. ±1 Programmable empty threshold
        g_fwft      => 1      -- 0 normal FIFO, 1 FWFT
     )
     port map (
        clk         => clk,  
        srst        => rst,   -- synchronous reset

        wr_en       => oled.vld_out,
        din         => oled.data_out,
        full        => open, --commits_fifo_full,

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
      clock          => clk            ,
      reset          => rst            ,
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
    -- sendData <= commit(6 downto 0);

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
            commit_wr_en       <= '1'; -- store fifo entry to 'commit' registers
            if commits_fifo_data_out(0) = '0' then -- Data entry
              next_state <= ExecuteDataCommit;
            else -- Command entry 
              next_state <= ExecuteCommandCommit;
            end if;
          end if;
          
	-- For each datum in commit (up to 4 data supported), waits for
	-- datum's 7 bits to be written serially to the SPI
        when ExecuteDataCommit =>
          sendDataValid <= '1';
          sendData <= mux_vec(commit_data, chars_counter(1 downto 0));
          if sendDone = '1' then
            chars_counter_wr_en <= '1';
            if chars_counter = commit_length_minus1 then
              next_state <= ConsumeCommit;
            else
              next_state <= ExecuteDataCommit_WaitSendDone;
            end if;
          end if;

        when ExecuteDataCommit_WaitSendDone =>
          sendDataValid <= '0';
          if sendDone = '0' then
            next_state <= ExecuteDataCommit;
          end if;

        when ExecuteCommandCommit => 
          next_state <= Idle; -- For now we do NOT support Type='Command' commits.
                              -- We just jump to the Idle state where the "active"
                              -- led should go off
      end case;
          
    end process;

    process(clk)
    begin
      if rising_edge(clk) then
        if rst = '1' or commit_wr_en = '1' then
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
            commit_data          <= commits_fifo_data_out(31 downto 4);
            commit_length_minus1 <= unsigned(commits_fifo_data_out( 3 downto 1)) - 1;
            commit_type          <= commits_fifo_data_out( 0);
          end if;
        end if;
      end if;
    end process;

end simple;
