# ~/.config/fish/functions/vpncon.fish
function vpncon --description "Connect to work VPN"
    set -l VPN_USER 'y.benlhaj'
    set -l VPN_PASS (pass show WorkVpn)

    doas bash -c "openvpn --config /opt/shared/access/work/Y.benlhaj/y.benlhaj__ssl_vpn_config.ovpn \
        --auth-user-pass <(echo -e \"$VPN_USER\n$VPN_PASS\")"
end
