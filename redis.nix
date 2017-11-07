{ config, pkgs, lib, ...}:
{
   services.redis.enable = true;
   services.redis.bind = "0.0.0.0";
   networking.firewall.allowedTCPPorts = [ 6379 ];
}
