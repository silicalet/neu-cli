# neu

东北大学校园服务命令行工具。

```sh
neu ipgw      # 校园网登录、状态和只读账单
neu webvpn    # 生成并打开 WebVPN 地址
neu lib       # 馆藏检索和图书馆公告
neu --help
neu --version
```

成功退出码为 0，执行失败为 1。`--json` 把结果写到 stdout；失败时 stderr 为 `{"ok":false,"error":"..."}`。参数写错仍由参数解析器输出文本。

## 账号

需要登录的命令使用已保存账号。默认文件是 `$XDG_CONFIG_HOME/neu/ipgw.json`，未设置时为 `~/.config/neu/ipgw.json`。`--config FILE` 可改用其他文件。

```sh
neu ipgw add -u 20200001                 # 添加或更新，密码不回显
neu ipgw default -u 20200001             # 设为默认账号
neu ipgw login -u 20200001               # 只用这一次，不写入文件
neu ipgw login --password-stdin          # 从标准输入读一行密码
```

`add` 把密码明文写入权限为 `0600` 的文件。`-p` 会进入 shell 历史和进程列表，优先用交互输入或 `--password-stdin`。配置损坏时命令报错并保留原文件。`status`、`logout`、`webvpn` 和帮助不读取账号。

## 校园网

`login`/`i`、`logout`/`o`、`status`/`s` 分别是别名。

```sh
neu ipgw status
neu ipgw s --json
neu ipgw login
neu ipgw logout
neu ipgw o --all                         # 下线该账号的全部设备
```

`status` 不需要账号，显示 IPv4、IPv6、套餐、已用流量和余额。未在线也是正常结果，退出码仍为 0；这时 `logout` 返回 1。已经在线时 `login` 只显示状态，不切换账号。

流量同时显示 IEC 单位和精确字节数。`--json` 保留网关返回的原始字段。

通用选项：

| 选项 | 作用 |
| --- | --- |
| `--json` | 结果输出 JSON |
| `--timeout SECONDS` | 单次请求超时，默认 10，范围 1–300 |
| `--bypass-system-proxy` | 忽略 HTTP(S) 代理环境变量，不绕过系统透明代理 |
| `--config FILE` | 指定账号文件 |

## 账单和用量

这些命令只查询，不修改套餐、不缴费、不下线设备。未指定账号时用默认账号。日期按北京时间，包含起止当天。

```sh
neu ipgw info                            # 账号状态和各产品余额
neu ipgw devices                         # 当前在线设备
neu ipgw traffic-log                     # 上网明细，默认读完全部分页
neu ipgw billing-log                     # 结算扣费，不含充值
neu ipgw payment-log                     # 充值记录
neu ipgw usage                           # 默认按日
neu ipgw usage --monthly --from 2025-01 --to 2026-10
neu ipgw usage --by-ip --from 2026-10-01 --to 2026-10-09
```

`--page N` 只读一页，每页 10 条。省略时读取当前能拿到的全部分页，最多 1000 页；记录数变化或出现重复记录时整次失败，不给部分统计。

- `traffic-log` 含上下线时间、IP、产品、流量、时长、消费额和余额。日期按上线日，跨日会话整段计入上线日。
- `billing-log` 含结算时间、费用、实时费用、流量和使用次数。费用和实时费用分开汇总。
- `payment-log` 按缴费日期筛选，并按缴费类型汇总。负金额原样保留，不自动当成退款。
- `info` 分开显示每个产品的余额，不求总和。未知值在文本里显示“未提供”，JSON 里是 `null`。
- `devices` 保留每条在线会话。没有设备时是空列表，退出码仍为 0。

`usage` 的三种模式互斥：

| 模式 | 结果 |
| --- | --- |
| `--daily` | 每日流量，日期写成 `YYYY-MM-DD` |
| `--monthly` | 每月流量，月份写成 `YYYY-MM` |
| `--by-ip` | 按上网明细里的 IP 汇总 |

日流量和月流量只包含网站实际返回的点，不把缺失日期补成 0。同一时段重复时不合并，文本会提示暂不汇总。

`--by-ip` 和 `traffic-log` 还可以组合筛选：

| 条件 | 含义 |
| --- | --- |
| `--ip IP` | IP 原文精确匹配，可重复 |
| `--product NAME` | 产品名原文精确匹配，可重复 |
| `--min-bytes N` / `--max-bytes N` | 单条会话的字节范围，包含边界 |
| `--min-seconds N` / `--max-seconds N` | 单条会话的秒数范围，包含边界 |

同一选项重复表示“或”，不同选项之间表示“且”。字节和秒数只接受非负整数，不接受 `1GiB` 这种后缀。IP 不是设备：地址会重新分配，明细里也没有 MAC。

## WebVPN

把校内网址转换成 `webvpn.neu.edu.cn` 地址。路径和查询串保持不变；没写协议时按 `http`。生成后仍要登录统一身份认证。

```sh
neu webvpn https://pass.neu.edu.cn/tpass/login
neu webvpn --json ipgw.neu.edu.cn
neu webvpn --open https://ipgw.neu.edu.cn:8800/home
neu webvpn --browser firefox https://pass.neu.edu.cn/tpass/login
```

`--open` 用 `xdg-open`。`--browser` 直接执行所给命令，不经过 shell，也会打开转换后的地址。

## 图书馆

只读查询，不看借阅记录，不做预约和荐购。校园网在线时直接访问；不在线时用已保存账号登录 WebVPN。

```sh
neu lib search haskell
neu lib search haskell --field WTI --base NEU01 --limit 10
neu lib news
neu lib --json news --kind resource --limit 10
```

`--field` 可以是 `WRD`（全部）、`WTI`（题名关键词）、`TIT`（题名精确）、`WAU`（作者）、`WSU`（主题）、`WPU`（出版社）、`ISS`、`ISB`、`CAL`（索书号）、`CLC`（分类号）、`SYS`、`BAR`（条码）。`--base NEU01` 是中文库，`NEU09` 是西文库。`--limit` 为 1–50，默认 10。

`news --kind notice` 是公告，`resource` 是资源动态。

## 致谢

本项目受 [Neboer/ipgw-py-manager](https://github.com/Neboer/ipgw-py-manager) 启发，
感谢原作者对东北大学校园网网关管理工具的开发与分享。

原项目的 MIT 许可证保留在 `third-party/ipgw-py-manager.LICENSE`。
