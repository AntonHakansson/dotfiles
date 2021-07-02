{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.crypto;
in {
  options.modules.crypto = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ monero-gui ];

    boot.kernel.sysctl = { "vm.nr_hugepages" = 128; };

    systemd.services.xmr-stak.wantedBy = mkForce [ ]; # don't start on boot
    services.xmr-stak = {
      enable = true;
      configFiles = {
        "config.txt" = ''
          /*
           * Network timeouts.
           * Because of the way this client is written it doesn't need to constantly talk (keep-alive) to the server to make
           * sure it is there. We detect a buggy / overloaded server by the call timeout. The default values will be ok for
           * nearly all cases. If they aren't the pool has most likely overload issues. Low call timeout values are preferable -
           * long timeouts mean that we waste hashes on potentially stale jobs. Connection report will tell you how long the
           * server usually takes to process our calls.
           *
           * call_timeout - How long should we wait for a response from the server before we assume it is dead and drop the connection.
           * retry_time	- How long should we wait before another connection attempt.
           *                Both values are in seconds.
           * giveup_limit - Limit how many times we try to reconnect to the pool. Zero means no limit. Note that stak miners
           *                don't mine while the connection is lost, so your computer's power usage goes down to idle.
           */
          "call_timeout" : 10,
          "retry_time" : 30,
          "giveup_limit" : 0,

          /*
           * Output control.
           * Since most people are used to miners printing all the time, that's what we do by default too. This is suboptimal
           * really, since you cannot see errors under pages and pages of text and performance stats. Given that we have internal
           * performance monitors, there is very little reason to spew out pages of text instead of concise reports.
           * Press 'h' (hashrate), 'r' (results) or 'c' (connection) to print reports.
           *
           * verbose_level - 0  - Don't print anything.
           *                 1  - Print intro, connection event, disconnect event
           *                 2  - All of level 1, and new job (block) event if the difficulty is different from the last job
           *                 3  - All of level 1, and new job (block) event in all cases, result submission event.
           *                 4  - All of level 3, and automatic hashrate report printing
           *                 10 - Debug level for developer
           *
           * print_motd    - Display messages from your pool operator in the hashrate result.
           */
          "verbose_level" : 4,
          "print_motd" : true,

          /*
           * Automatic hashrate report
           *
           * h_print_time - How often, in seconds, should we print a hashrate report if verbose_level is set to 4.
           *                This option has no effect if verbose_level is not 4.
           */
          "h_print_time" : 300,

          /*
           * Manual hardware AES override
           *
           * Some VMs don't report AES capability correctly. You can set this value to true to enforce hardware AES or
           * to false to force disable AES or null to let the miner decide if AES is used.
           *
           * WARNING: setting this to true on a CPU that doesn't support hardware AES will crash the miner.
           */
          "aes_override" : null,

          /*
           * LARGE PAGE SUPPORT
           * Large pages need a properly set up OS. It can be difficult if you are not used to systems administration,
           * but the performance results are worth the trouble - you will get around 20% boost. Slow memory mode is
           * meant as a backup, you won't get stellar results there. If you are running into trouble, especially
           * on Windows, please read the common issues in the README and FAQ.
           *
           * On Linux you will need to configure large page support and increase your memlock limit (ulimit -l).
           *
           * To set large page support, add the following to "/etc/sysctl.d/60-hugepages.conf":
           *     vm.nr_hugepages=128
           * You WILL need to run "sudo sysctl --system" for these settings to take effect on your system (or reboot).
           *  In some cases (many threads, very large CPU, etc) you may need more than 128
           *   (try 256 if there are still complaints from thread inits)
           *
           * To increase the memlock (ulimit -l), add following lines to /etc/security/limits.d/60-memlock.conf:
           *     *    - memlock 262144
           *     root - memlock 262144
           * You WILL need to log out and log back in for these settings to take effect on your user (no need to reboot, just relogin in your session).
           *
           * Check with "/sbin/sysctl vm.nr_hugepages ; ulimit -l" to validate
           *
           * Memory locking means that the kernel can't swap out the page to disk - something that is unlikely to happen on a
           * command line system that isn't starved of memory. I haven't observed any difference on a CLI Linux system between
           * locked and unlocked memory. If that is your setup see option "no_mlck".
           *
           *
           * use_slow_memory defines our behaviour with regards to large pages. There are three possible options here:
           * always  - Don't even try to use large pages. Always use slow memory.
           * warn    - We will try to use large pages, but fall back to slow memory if that fails.
           * no_mlck - This option is only relevant on Linux, where we can use large pages without locking memory.
           *           It will never use slow memory, but it won't attempt to mlock
           * never   - If we fail to allocate large pages we will print an error and exit.
           */
          "use_slow_memory" : "warn",

          /*
           * TLS Settings
           * If you need real security, make sure tls_secure_algo is enabled (otherwise MITM attack can downgrade encryption
           * to trivially breakable stuff like DES and MD5), and verify the server's fingerprint through a trusted channel.
           *
           * tls_secure_algo - Use only secure algorithms. This will make us quit with an error if we can't negotiate a secure algo.
           */
          "tls_secure_algo" : true,

          /*
           * Daemon mode
           *
           * If you are running the process in the background and you don't need the keyboard reports, set this to true.
           * This should solve the hashrate problems on some emulated terminals.
           */
          "daemon_mode" : true,

          /*
           * Output file
           *
           * output_file  - This option will log all output to a file.
           *
           */
          "output_file" : "",

          /*
           * Built-in web server
           * I like checking my hashrate on my phone. Don't you?
           * Keep in mind that you will need to set up port forwarding on your router if you want to access it from
           * outside of your home network. Ports lower than 1024 on Linux systems will require root.
           *
           * httpd_port - Port we should listen on. Default, 0, will switch off the server.
           */
          "httpd_port" : 0,

          /*
           * HTTP Authentication
           *
           * This allows you to set a password to keep people on the Internet from snooping on your hashrate.
           * Keep in mind that this is based on HTTP Digest, which is based on MD5. To a determined attacker
           * who is able to read your traffic it is as easy to break a bog door latch.
           *
           * http_login - Login. Empty login disables authentication.
           * http_pass  - Password.
           */
          "http_login" : "",
          "http_pass" : "",

          /*
           * prefer_ipv4 - IPv6 preference. If the host is available on both IPv4 and IPv6 net, which one should be choose?
           *               This setting will only be needed in 2020's. No need to worry about it now.
           */
          "prefer_ipv4" : true,
        '';
        "pools.txt" = ''
          /*
           * pool_address    - Pool address should be entered as "pool_address:port" (e.g "pool.ryo-currency.com:4444"). Only stratum pools are supported.
           * wallet_address  - Your wallet, or pool login.
           * rig_id          - Rig identifier for pool-side statistics (needs pool support).
           * pool_password   - Can be empty in most cases or "x".
           * use_nicehash    - Limit the nonce to 3 bytes as required by nicehash.
           * use_tls         - This option will make us connect using Transport Layer Security.
           * tls_fingerprint - Server's SHA256 fingerprint. If this string is non-empty then we will check the server's cert against it.
           * pool_weight     - Pool weight is a number telling the miner how important the pool is. Miner will mine mostly at the pool
           *                   with the highest weight, unless the pool fails. Weight must be an integer larger than 0.
           */

          "pool_list" :
          [
            {
              "pool_address" : "stratum+tcp://xmr.f2pool.com:13531",
              "wallet_address" : "84pFDREuwembc9XXYvXoFyTX8Kzhb84KsZYfCyoQByhRirmFmw6HQuxSLTDy1iZWYRMLqnji4oKA8LZCojmLkrsDJtG2GZk",
              "rig_id" : "",
              "pool_password" : "x",
              "use_nicehash" : false,
              "use_tls" : false,
              "tls_fingerprint" : "",
              "pool_weight" : 1
            },
          ],

          /*
           * Currency to mine. Supported values:
           *
           *    bbscoin (automatic switch with block version 3 to cryptonight_v7)
           *    bittube (uses cryptonight_bittube2 algorithm)
           *    graft
           *    haven (automatic switch with block version 3 to cryptonight_haven)
           *    lethean
           *    masari
           *    qrl - Quantum Resistant Ledger
           *    ryo
           *    turtlecoin
           *    plenteum
           *    torque
           *    xcash
           *
           * Native algorithms which do not depend on any block versions:
           *
           *    # 256KiB scratchpad memory
           *    cryptonight_turtle
           *    # 1MiB scratchpad memory
           *    cryptonight_lite
           *    cryptonight_lite_v7
           *    cryptonight_lite_v7_xor (algorithm used by ipbc)
           *    # 2MiB scratchpad memory
           *    cryptonight
           *    cryptonight_gpu (for Ryo's 14th of Feb fork)
           *    cryptonight_superfast
           *    cryptonight_v7
           *    cryptonight_v8
           *    cryptonight_v8_double (used by xcash)
           *    cryptonight_v8_half (used by masari and torque)
           *    cryptonight_v8_reversewaltz (used by graft)
           *    cryptonight_v8_zelerius
           *    # 4MiB scratchpad memory
           *    cryptonight_bittube2
           *    cryptonight_haven
           *    cryptonight_heavy
           */

          "currency" : "cryptonight_v8",
        '';

        "cpu.txt" = ''
          /*
           * Thread configuration for each thread. Make sure it matches the number above.
           * low_power_mode - This can either be a boolean (true or false), or a number between 1 to 5. When set to true,
           *                  this mode will double the cache usage, and double the single thread performance. It will
           *                  consume much less power (as less cores are working), but will max out at around 80-85% of
           *                  the maximum performance. When set to a number N greater than 1, this mode will increase the
           *                  cache usage and single thread performance by N times.
           *
           * no_prefetch    - Some systems can gain up to extra 5% here, but sometimes it will have no difference or make
           *                  things slower.
           *
           * asm            - Allow to switch to a assembler version of cryptonight_v8; allowed value [auto, off, intel_avx, amd_avx]
           *                    - auto: xmr-stak will automatically detect the asm type (default)
           *                    - off: disable the usage of optimized assembler
           *                    - intel_avx: supports Intel cpus with avx instructions e.g. Xeon v2, Core i7/i5/i3 3xxx, Pentium G2xxx, Celeron G1xxx
           *                    - amd_avx: supports AMD cpus with avx instructions e.g. AMD Ryzen 1xxx and 2xxx series
           *
           * affine_to_cpu  - This can be either false (no affinity), or the CPU core number. Note that on hyperthreading
           *                  systems it is better to assign threads to physical cores. On Windows this usually means selecting
           *                  even or odd numbered cpu numbers. For Linux it will be usually the lower CPU numbers, so for a 4
           *                  physical core CPU you should select cpu numbers 0-3.
           *
           * On the first run the miner will look at your system and suggest a basic configuration that will work,
           * you can try to tweak it from there to get the best performance.
           *
           * A filled out configuration should look like this:
           * "cpu_threads_conf" :
           * [
           *      { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 0 },
           *      { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 1 },
           * ],
           * If you do not wish to mine with your CPU(s) then use:
           * "cpu_threads_conf" : null,
           */

          "cpu_threads_conf" :
          [
              { "low_power_mode" : true, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 0 },
              { "low_power_mode" : true, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 1 },
              { "low_power_mode" : true, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 2 },
              { "low_power_mode" : true, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 3 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 4 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 5 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 6 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 7 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 8 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 9 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 10 },
              { "low_power_mode" : false, "no_prefetch" : true, "asm" : "auto", "affine_to_cpu" : 11 },
          ],
        '';
      };
    };
  };
}
