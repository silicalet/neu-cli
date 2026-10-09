# neu

东北大学校园服务 CLI，Haskell 实现。目前提供 `neu ipgw` 网关子命令，后续可扩展教务系统。

## 开发

```sh
devenv shell
cabal build all
cabal run neu -- ipgw --help
neu-check
```

环境沿用 Nix + devenv，使用 `pkgs.ghc`、Cabal、HLS 和 Fourmolu；当前锁定环境的
GHC 为 9.10.3。`devenv.lock` 固定 Nix 输入，更新工具链可运行 `devenv update`。
已有 direnv 集成的终端可执行 `direnv allow` 自动加载环境。

```sh
neu-fmt                    # 格式化源码和测试
neu-check                  # 构建、离线测试、CLI/HTTP 集成测试、格式检查
cabal repl lib:neu          # 库的交互式开发
cabal install exe:neu       # 安装 CLI 到 Cabal 的用户 bin 目录
```

首次构建需要下载 Hackage 依赖，必要时先运行 `cabal update`。
测试使用模拟网关响应、本地 HTTP 代理和专用测试 RSA 密钥，不使用真实校园账号。
测试套件全部使用 Haskell 编写，CLI 集成测试通过 WAI/Warp 启动本地模拟代理，
由 Cabal 构建并调用 `neu` 可执行文件。RSA 互操作测试需要 OpenSSL（已包含在 devenv 中）。
`test/fixtures/test-only-private.pem` 仅用于离线测试，不用于真实认证。

## 网关命令

以下示例使用已安装的 `neu`；开发时可把 `neu` 替换成 `cabal run neu --`。

```sh
neu ipgw status                      # 查询当前设备，无需保存账号
neu ipgw s --json                    # JSON 输出，适合脚本
neu ipgw login -u 20200001            # 隐藏回显输入密码，本次登录不保存密码
neu ipgw i -u 20200001 --password-stdin  # 从 stdin 读取一行密码
neu ipgw logout                      # 注销当前设备
neu ipgw o --all                     # 下线当前账号的所有设备

neu ipgw add -u 20200001              # 添加或更新账号，隐藏回显输入密码
neu ipgw default -u 20200001          # 设置已保存的账号为默认
neu ipgw login                       # 使用已保存的默认账号
```

`login/i`、`logout/o`、`status/s` 互为别名。`login -u` 优先使用该账号已保存的密码，
未保存时交互询问；`--password-stdin` 或 `-p/--password` 可覆盖保存的密码。
已在线时登录命令直接显示状态，不切换账号。

状态文本分别显示 IPv4 和 IPv6 地址，网关未返回地址时显示“未提供”。
已用流量同时显示 IEC 单位（每级 1024，如 `1.50 GiB`）和精确字节数；
`--json` 保留网关原始字段和数值，便于脚本处理。

通用选项：

- `--config FILE`：指定账号配置文件。
- `--json`：成功结果输出到 stdout；执行错误以 JSON 输出到 stderr。
- `--timeout SECONDS`：每次 HTTP 请求的超时，默认 10 秒，范围 1–300。
- `--bypass-system-proxy`：忽略 HTTP(S) 代理环境变量；不绕过系统透明代理。

成功退出码为 0，执行失败为 1。未在线是正常查询结果，`status` 仍返回 0；
未在线时 `logout` 返回 1。命令行语法错误由参数解析器以文本报告。

## 账号配置

默认路径为 `$XDG_CONFIG_HOME/neu/ipgw.json`，未设置 XDG 时为 `~/.config/neu/ipgw.json`。
`add` 将密码**明文保存**在权限为 `0600` 的文件中，通过同目录临时文件和原子替换写入。
不希望保存密码时使用 `login -u`。命令行 `-p` 参数可能出现在 shell 历史或进程列表中，
建议使用交互输入或 `--password-stdin`。

`status`、`logout` 和帮助命令不读取账号配置。配置文件损坏时，账号修改命令会报错，
不会覆盖原文件。本程序使用独立配置格式，不自动读取或修改 Python 版的配置。

## 实现和验证范围

网关登录使用 SSO 流程：获取 `ac_id`、登录表单和 `login_neu.js` 中的 RSA 公钥，
使用 RSA PKCS#1 v1.5 加密用户名与密码，通过 Cookie 会话取得 ticket 后开通网关。
包含未绑定邮箱跳转、欠费和认证失败处理。遇到额外验证码或未知认证页面时明确报错。

网关 API 位于 `src/NEU/IPGW/`，CLI 参数和输出位于 `src/NEU/CLI.hs`，
入口为 `app/Main.hs`。网络传输可注入，便于通过模拟响应测试业务流程。

离线测试覆盖 JSON/JSONP、RSA 与 OpenSSL 互操作、SSO 请求顺序和 ticket 编码、
凭据目的地检查、Cookie/重定向、单设备与批量注销、配置权限和 CLI 错误。
尚未使用真实账号完成校园网端到端验证；学校接口改动仍可能需要适配。

## 致谢

本项目受 [Neboer/ipgw-py-manager](https://github.com/Neboer/ipgw-py-manager) 启发，
感谢原作者对东北大学校园网网关管理工具的开发与分享。

原项目的 MIT 许可证保留在 `third-party/ipgw-py-manager.LICENSE`。
本项目自身许可证尚未选择（Cabal 中为 `NONE`）。
