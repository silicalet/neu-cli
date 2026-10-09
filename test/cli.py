"""Offline CLI and HTTP tests. All gateway traffic goes to a local mock proxy."""
import http.server
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import urllib.parse

exe = str(Path(sys.argv[1]).resolve())

with tempfile.TemporaryDirectory(prefix="neu-cli-") as tmp:
    env = dict(os.environ, XDG_CONFIG_HOME=tmp)

    def run(args, code=0, stdin=None):
        result = subprocess.run([exe, *args], input=stdin, text=True,
                                capture_output=True, env=env, timeout=15)
        assert result.returncode == code, (args, result.returncode, result.stdout, result.stderr)
        return result

    for args in (["--help"], ["ipgw", "--help"], ["ipgw", "login", "--help"], ["--version"]):
        run(args)
    assert not (Path(tmp) / "neu").exists(), "help must have no side effects"
    run(["ipgw", "bogus"], 1)
    run(["ipgw", "--timeout", "0", "status"], 1)
    run(["ipgw", "add", "-u", "u", "--password-stdin", "-p", "secret"], 1)
    config = Path(tmp) / "neu/ipgw.json"
    assert not config.exists()
    result = run(["ipgw", "add", "-u", "u", "--password-stdin"], stdin="test-secret\n")
    assert "test-secret" not in result.stdout + result.stderr
    assert config.stat().st_mode & 0o777 == 0o600
    run(["ipgw", "default", "-u", "u"])
    assert json.loads(config.read_text())["default_username"] == "u"
    run(["ipgw", "--json", "default", "-u", "missing"], 1)
    assert json.loads(config.read_text())["default_username"] == "u"
    config.write_text("broken: test-secret")
    failure = run(["ipgw", "--json", "default", "-u", "u"], 1)
    assert json.loads(failure.stderr)["ok"] is False
    assert "test-secret" not in failure.stderr
    assert config.read_text() == "broken: test-secret"

    seen = []
    state = {"online": False, "bad": False, "fields": {}}

    class Mock(http.server.BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            self.do_GET()

        def do_GET(self):
            url = urllib.parse.urlsplit(self.path)
            assert url.hostname == "ipgw.neu.edu.cn", url
            seen.append((self.command, url.path, dict(self.headers)))
            query = urllib.parse.parse_qs(url.query)
            if state["bad"]:
                self.send_response(503)
                self.end_headers()
                return
            if url.path == "/cgi-bin/rad_user_info":
                payload = {"error": "ok", "user_name": "fixture-user", "online_ip": "10.0.0.1"} if state["online"] else {"error": "not_online_error"}
                if state["online"]:
                    payload.update(state["fields"])
                body = query["callback"][0] + "(" + json.dumps(payload) + ");"
                self.send_response(200)
                self.send_header("Set-Cookie", "session=fixture; Path=/")
            elif url.path in ("", "/"):
                assert "session=fixture" in self.headers.get("Cookie", "")
                self.send_response(302)
                self.send_header("Location", "http://ipgw.neu.edu.cn/srun_portal_pc?ac_id=7")
                body = ""
            elif url.path == "/srun_portal_pc":
                assert "session=fixture" in self.headers.get("Cookie", "")
                self.send_response(200)
                body = "<title>IP控制网关</title>"
            elif url.path == "/cgi-bin/srun_portal":
                assert query["username"] == ["fixture-user"]
                assert query["ac_id"] == ["7"]
                self.send_response(200)
                body = 'neu_logout({"ecode":0})'
            elif url.path == "/v1/batch-online-drop":
                assert self.command == "POST"
                assert "session=fixture" in self.headers.get("Cookie", "")
                self.send_response(200)
                body = '{"code":0}'
            else:
                raise AssertionError(url)
            self.end_headers()
            self.wfile.write(body.encode())

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Mock)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        proxy = f"http://127.0.0.1:{server.server_port}"
        for key in ("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "all_proxy", "ALL_PROXY"):
            env[key] = proxy
        env["no_proxy"] = env["NO_PROXY"] = ""
        offline = run(["ipgw", "--timeout", "2", "--json", "status"])
        assert json.loads(offline.stdout)["online"] is False
        assert len(seen) == 1, "status must only contact mock proxy, no SSO"
        run(["ipgw", "o"], 1)
        state["online"] = True
        state["fields"] = {"online_ip6": "2001:db8::1234", "sum_bytes": 1610612736}
        online = run(["ipgw", "s", "--json"])
        data = json.loads(online.stdout)
        assert data["online"] is True
        assert data["data"]["online_ip6"] == "2001:db8::1234"
        assert data["data"]["sum_bytes"] == 1610612736
        readable = run(["ipgw", "status"]).stdout
        assert "IPv4：10.0.0.1" in readable
        assert "IPv6：2001:db8::1234" in readable
        assert "已用流量：1.50 GiB（1610612736 字节）" in readable
        for count, expected in [(0, "0 B"), (1023, "1023 B"), (1024, "1.00 KiB"),
                                (1048576, "1.00 MiB"), (1099511627776, "1.00 TiB"),
                                ("1536", "1.50 KiB")]:
            state["fields"] = {"online_ip6": "", "sum_bytes": count}
            readable = run(["ipgw", "s"]).stdout
            assert "IPv6：未提供" in readable
            assert f"已用流量：{expected}（{count} 字节）" in readable
        for fields in ({}, {"sum_bytes": None}, {"sum_bytes": -1}, {"sum_bytes": "bad"}):
            state["fields"] = fields
            readable = run(["ipgw", "s"]).stdout
            assert "IPv6：未提供" in readable
            assert "已用流量：未知" in readable
        # Already-online login must neither read corrupt config nor ask for credentials.
        run(["ipgw", "i"])
        run(["ipgw", "logout"])
        run(["ipgw", "o", "--all"])
        state["bad"] = True
        failed = run(["ipgw", "--json", "status"], 1)
        assert "503" in json.loads(failed.stderr)["error"]
    finally:
        server.shutdown()
        server.server_close()
        thread.join()

print("CLI and local HTTP integration checks passed (cookies, redirects, proxy, logout, JSON, config).")
