#!/usr/bin/env bash
# Local syntax/regex check of gwern.net.conf and its redirect includes.
# Requires nginx (with libnginx-mod-http-lua) and openssl.
set -e
cd -- "$(dirname -- "$0")/../nginx"
NGINX_TEST_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/gwern-nginx-test"
umask 077
mkdir --parents "$NGINX_TEST_DIR"

if [[ ! -s "$NGINX_TEST_DIR/test.key" || ! -s "$NGINX_TEST_DIR/test.cert" ]]; then
    openssl req -x509 -newkey ed25519 -nodes -days 36500 -subj /CN=localhost \
        -keyout "$NGINX_TEST_DIR/test.key" -out "$NGINX_TEST_DIR/test.cert"
fi
cp -- test.conf "$NGINX_TEST_DIR/nginx.conf"
bash memoriam.sh > "$NGINX_TEST_DIR/memoriam.conf"
# Adapt deployment paths/ports in the test copy only; use the real redirect files.
sed --regexp-extended \
    -e "s|/home/gwern/gwern\.net/static/nginx/|$PWD/|g" \
    -e "s|/home/gwern/ssl/cloudflare|$NGINX_TEST_DIR/test|g" \
    -e "s|/var/log/nginx/|$NGINX_TEST_DIR/|g" \
    -e "s|/etc/nginx/conf\.d/memoriam\.conf|$NGINX_TEST_DIR/memoriam.conf|g" \
    -e 's/listen[[:space:]]+80([[:space:];])/listen 127.0.0.1:18080\1/g' \
    -e 's/listen[[:space:]]+443([[:space:];])/listen 127.0.0.1:18443\1/g' \
    gwern.net.conf > "$NGINX_TEST_DIR/site.conf"
exec nginx -t -e stderr -p "$NGINX_TEST_DIR/" -c nginx.conf
