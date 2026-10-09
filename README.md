# neu

东北大学校园服务 CLI，Haskell 实现。目前提供 `neu ipgw` 网关和只读自助服务命令。

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

## 流量与扣费日志

```sh
neu ipgw traffic-log -u 20200001     # 交互输入密码，本次使用不保存
neu ipgw traffic-log                # 使用 neu 已保存的默认账号
neu ipgw billing-log                # 结算清单，不是充值/缴费清单
neu ipgw traffic-log --page 2        # 只读取第 2 页，每页 10 条
neu ipgw traffic-log --from 2026-10-01 --to 2026-10-09
neu ipgw --json billing-log --from 2026-01-01
```

两个命令均支持 `-u`、`--password-stdin`、`--config` 和通用网络选项，账号与网关相同。
它们独立登录 `:8800` 自助服务，只查询日志，不调用网关开通或下线接口。
默认读取网站**当前可获取的全部分页**；`--page` 限定到单页。
最多自动读取 1000 页，超过限制请显式分页；记录数变化或重复记录会报错，不输出部分统计。

日期筛选在读取后于本地进行，按北京时间，包含起止日期：流量按**上线日期**、
扣费按**结算日期**。跨日上网记录整体计入其上线日期，不按时间比例拆分流量。
输出列出来源记录数、读取数、筛选后记录数；统计仅覆盖实际展示的记录，
不能将网站保留的历史记录视为账号自开户以来的全部历史。

- 流量日志：上下线时间、IP、套餐、流量、时长、消费额、余额；汇总流量、秒数和消费额。
- 扣费日志：结算时间、ID、费用、实时费用、总流量和使用次数；费用与实时费用分别汇总，
  保留网站口径，不假设二者可以相加。

流量显示 IEC 单位和精确字节数，计算使用页面隐藏的原始字节数、秒数；
金额采用精确十进制，保留原始精度（包括余额的小数点后四位）。
JSON 包含 `entries`、`statistics`、`source_total`、`fetched_records`、`scope`、
`page`、`from`、`to`、`date_basis` 和 `timezone`。
`bytes`、`seconds` 是整数，`*_yuan` 是以元为单位的十进制数；空结果输出空数组和零合计。
`traffic-log` 还支持下文“按 IP 用量与组合筛选”的全部筛选项；有筛选条件时 JSON 增加
`filters`，未使用新筛选项时现有日志 JSON 保持不变。扣费/充值日志不接受 IP、产品、流量或时长条件。

## 账号概况与在线设备

```sh
neu ipgw info -u 20200001            # 账号状态与各产品概况
neu ipgw --json info
neu ipgw devices                    # 所有在线会话与数量
neu ipgw --json devices
```

`info` 与 `devices` 使用自助服务首页，不改变 `status` 查询当前设备网关状态的含义。
设备保留网站返回的 IP（包括 IPv6）、MAC、上线时间和计费策略；重复 IP 的会话不合并。
没有在线设备是正常空列表；登录页、服务异常或核心表格丢失会报错，不能当成零台设备。
产品余额分别展示，不计算跨产品总余额；套餐详情仅取已有可见文本，不发起展开或修改请求。

未知值在文本中显示“未提供”，JSON 为 `null`。首页只有格式化流量/时长时保留原文，
不反推精确字节数/秒数，例如 `{"used_bytes":null,"used_traffic_display":"1.23G"}`。
普通十进制金额使用精确数值；只有格式化金额时保留 `cost_display` / `balance_display`，
对应 `cost_yuan` / `balance_yuan` 为 `null`。

`info` JSON 包含 `account`、`products`、`online_device_count`、`timezone`；
`devices` 包含 `devices`、`count`、`timezone`。时间为 `YYYY-MM-DD HH:MM:SS`，
时区固定为 `Asia/Shanghai`，设备行序号不作为稳定设备 ID。

## 日/月流量

```sh
neu ipgw usage                      # 默认 --daily
neu ipgw usage --daily --from 2026-10-01 --to 2026-10-08
neu ipgw usage --monthly --from 2025-01 --to 2026-10
neu ipgw --json usage --monthly
```

`--daily` / `--monthly` 互斥。日范围严格使用 `YYYY-MM-DD`，月范围使用 `YYYY-MM`；
无效日期和反向区间在读取密码或联网前拒绝。读取服务器当前返回的图表数据后本地筛选，
含首尾期间，不请求未经证实的历史范围参数或分页，不补缺失日期为零，不推算全月/全年，
也不假定当日/当月完整。

日/月接口的源单位不同：日统计 `/statistic/index` 返回十进制 kB（1 kB = 1000 字节），
月统计 `/statistic/flow` 返回字节。解析时先统一为字节，不能仅凭两页共用的图表格式化脚本
判断源单位。日统计如 `123.456` 精确换算为 `123456` 字节，不乘 1024、不直接当字节。
`bytes`、`total_bytes`、`peak_bytes` 均为精确整数；转换使用 `Scientific` 运算，
不经 `Double`，也不四舍五入。换算后仍有小数字节、负数、字符串、`null` 或对象时明确报错。
文本用 IEC 单位（KiB/MiB/GiB，每级 1024）并附完整字节数；JSON 使用相同的规范化字节数。
精度和单位说明仅保留在文档中，不在表头与数据行之间插入说明文字。

所有源数据点按原顺序保留，`source_index` 从 0 开始，筛选后不重新编号。
同一期间重复，即使数值相同也不去重、不相加；文本提示“存在重复期间，暂不汇总”，
JSON `statistics.aggregation_status="ambiguous"`、`total_bytes=null`、`peak_bytes=null`、
`peak_periods=[]`。无重复时状态为 `complete`，只汇总筛选后的实际点，并列出全部并列峰值。
空结果合计为 0、峰值为 null。

JSON 顶层包含 `period`、`timezone`、`from`、`to`、`source_points`、
`source_distinct_periods`、`source_duplicate_periods`、`entries`、`statistics`、`warnings`。
`source_*` 描述筛选前数据；统计和 `duplicate_periods` 警告描述筛选后数据。
筛掉重复期间后可恢复汇总，但仍保留来源重复信息。月页面的重复业务含义尚未确认，
不提供强制求和开关，也不要求趋势与上网明细统计相等。

## 按 IP 用量与组合筛选

```sh
neu ipgw usage --by-ip                         # 按 IP 汇总当前可获取的全部上网明细
neu ipgw usage --by-ip --from 2026-10-01 --to 2026-10-09
neu ipgw usage --by-ip --ip 192.0.2.1 --ip 2001:db8::1
neu ipgw --json usage --by-ip --product '产品名称' --min-bytes 1073741824
neu ipgw traffic-log --ip 192.0.2.1 --min-seconds 3600 --max-seconds 86400
```

`--by-ip` 从 `/log/detail` 的上网明细计算，与 `--daily` / `--monthly` 互斥，
不改变账号日/月图表的数据来源。流量明细没有 MAC 或稳定设备 ID；**IP 不等于物理设备**，
地址重新分配后可能包含不同设备，同一设备换 IP 后也会分组。不能据此按 MAC 追溯设备历史，
也不使用当前在线设备的 MAC/IP 关联推断旧记录。

| 条件 | 含义 |
| --- | --- |
| `--from YYYY-MM-DD` / `--to YYYY-MM-DD` | 北京时间上线日期，包含边界 |
| `--ip IP` | 日志 IP 原文精确匹配，支持 IPv4/IPv6，可重复 |
| `--product NAME` | 产品名原文精确匹配，可重复 |
| `--min-bytes N` / `--max-bytes N` | 每条会话的流量上下限，含边界，单位字节 |
| `--min-seconds N` / `--max-seconds N` | 每条会话的时长上下限，含边界，单位秒 |
| `--page N` | 仅读取指定页；省略时读取全部可获取分页 |

这些条件同样适用于 `traffic-log`。同一参数重复是“或”，不同条件之间是“且”；
IP/产品不做子串、大小写或 IPv6 写法归一化，请复制日志中的值。字节/秒数接受非负整数，
不接受 `1GiB` 等后缀；反向范围、空 IP/产品、无效日期和模式冲突在读取凭据/联网前拒绝。
账号日/月图表没有这些维度，不能搭配 `--ip`、`--product`、字节/时长条件或 `--page`。

先完成分页完整性检查，再逐条筛选，最后按 IP 汇总；阈值不是对分组后的总量筛选。
跨日会话整体计入上线日期，不按时长拆分流量；只反映已返回的明细，不推算实时在线流量，
不假定与账号日/月图表一致。组按原始 IP 文本排序，产品去重排序，统计只覆盖筛选后的记录。

JSON 保留日志的来源数、读取数、页码、日期和时区；新增 `source="/log/detail"`、
`group_by="ip"`、`ip_count`、`filters`。`entries` 每组为 `ip`、`products`、`records`、
`bytes`、`seconds`、`cost_yuan`、`first_started_at`、`last_ended_at`；
`statistics` 为记录数、整数字节/秒数及精确消费额。无匹配项时空数组、零合计。

## 充值记录

```sh
neu ipgw payment-log                 # 当前可获取的全部分页
neu ipgw payment-log --page 2         # 仅第 2 页，每页 10 条
neu ipgw --json payment-log --from 2026-01-01 --to 2026-12-31
```

数据来自 `/log/pay`，与 `billing-log` 的结算费用分开。复用日志分页保护和本地含首尾日筛选，
按北京时间缴费日期筛选。记录 JSON 为 `id`、`username`、`paid_at`、`amount_yuan`、`payment_type`；
报告 `kind="payment"`、`date_basis="paid_at"`，其余范围/来源字段沿用日志报告。
统计含 `records`、精确 `amount_yuan` 和按缴费类型文本排序的 `by_type` 数组；
每组含 `payment_type`、`records`、`amount_yuan`。总计和分组均仅覆盖筛选后的记录。
保留未知类型原文和负金额，金额仅做账面代数和，不将负数擅自解释为退款；不返回虚构流量字段。

上述所有只读命令均支持 `-u`、`-p`、`--password-stdin` 和通用 `--config`、`--json`、
`--timeout`、`--bypass-system-proxy`。复用 neu 自身账号选择与 CAS，每条命令认证一次，
后续请求复用内存 Cookie，不保存密码、不修改默认账号、不自动导入旧 Python 配置。
成功 JSON 是 stdout 上的单个值；执行失败向 stderr 输出 `{"ok":false,"error":"..."}`，
退出码 1；语法错误保持 optparse 文本行为。文本输出清理页面控制字符。
不实现或执行指定设备下线、停机保号、套餐变更、开通或支付操作。

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

公共认证位于 `NEU.Auth.CAS`，表单解析和 RSA 位于 `NEU.Auth.CAS.Protocol`，
进程内 HTTP/Cookie 会话位于 `NEU.HTTP.Session`。这些模块不依赖网关或账号配置。
网关 API 位于 `src/NEU/IPGW/`，CLI 参数和输出位于 `src/NEU/CLI.hs`，
入口为 `app/Main.hs`。网络传输可注入，便于通过模拟响应测试业务流程。

### 复用统一身份认证

`authenticate session service username password` 为指定 CAS `service` 完成认证跳转，
返回最终页面 `Reply`，由业务模块检查是否登录成功。已有 CAS Cookie 时可以跳过表单；
不同服务仍分别发起认证请求，不共用 ticket。网关随后单独校验回调路径、`ac_id` 和 ticket，
再调用开通接口，公共认证本身不会开通网关。

自助服务适配器已处理校内服务的已知内部跳转，可直接使用：

```haskell
import NEU.IPGW.SelfService
import NEU.IPGW.Logs

-- 以下 Text 字面量需要 OverloadedStrings。
-- username/password 由调用方提供，业务模块不读取或保存账号文件。
readTraffic username password = do
    client <- newSelfService False 10
    authenticateSelfService client username password
    fetchLogs client TrafficLog (LogQuery Nothing Nothing Nothing)
```

`NEU.IPGW.SelfService` 负责首页登录校验、已知回调兼容、只读页面请求和日志分页；
`NEU.IPGW.Logs` 负责日志原始列和精确统计；`NEU.IPGW.Overview`、`NEU.IPGW.Usage`
负责概况和趋势的纯解析、JSON 及文本展示。`fetchOverview`、`fetchUsage` 与 `fetchLogs`
复用同一已认证客户端。日志 `entryBytes` 返回 `Maybe Integer`，充值记录为 `Nothing`。
其他业务模块可通过 `mkService` 声明目标，验证 `authenticate` 返回的落地页面，
再用 `requestService session service` 发起业务请求。
网关可通过 `NEU.IPGW.Client.clientWithSession` 使用同一会话。

会话仅保存在内存中，按同一身份顺序使用；切换账号应创建新会话。
Cookie 不按端口隔离，同主机不同端口若存在同名 Cookie 冲突，应使用独立会话。
地址白名单精确匹配协议、主机和端口，检查首次请求及每次重定向；
公钥只能从 HTTPS CAS 源获取，307/308 不重发认证表单。
业务请求跳回 CAS 会报错，由调用方显式重新认证。
`CASError` 区分认证被拒绝或需要额外验证、需要交互、协议错误；
网络和地址限制错误为 `ServiceError`，网关 CLI 统一转换为现有的 `IPGWError`。

离线测试覆盖 JSON/JSONP、RSA 与 OpenSSL 互操作、SSO 请求顺序和 ticket 编码、
凭据目的地检查、跨服务认证、已有 CAS 会话、Cookie/重定向和端口隔离、
单设备与批量注销、配置权限和 CLI 错误，以及日志原始列、分页、日期筛选、精确统计、
空结果、页面变化和重复记录。新增测试覆盖嵌套首页/多产品/IPv6/未知值、图表 JSON 提取、
严格日/月范围、重复期间、日 kB/月字节归一化、超大数精度、充值分组及完整模拟认证读取流程。
实际 CLI 参数测试使用本地请求陷阱，检查帮助、模式/日期/凭据冲突不联网、不写配置。
全部夹具人工构造；普通测试不请求校园主机。模拟业务 Transport 与本地 HTTP 代理测试
不等于真实 HTTPS CAS 端到端测试。

2026-10-09 真实只读 CLI 验证了 `info`、`devices`、`usage --monthly` 和 `payment-log`
的文本及 JSON；充值读取全部分页并独立复算总金额与类型分组，月统计保留重复并输出空合计。
该月样本所有月份均重复，没有可用于实测非空无歧义汇总的月范围。
日统计单位经日/月同月样本、日志原始计数及产品显示量交叉核实：日原值合计乘以 1000
恰好等于同月每个重复源点的字节数。默认 `usage` 文本、默认/显式 daily JSON 和日期筛选
均读取成功；逐项验证精确归一化、独立复算字节合计和峰值，月统计重复保护不变。
按 IP 用量和组合筛选也完成真实 CLI 文本/JSON 验证：对照未筛选明细独立计算每个 IP 的
产品集合、记录数、字节、秒数、消费额和时间范围，覆盖日期/IP/产品/字节/时长组合、单页及空结果。
日/月命令仍可读取，账号配置未改写；历史明细没有 MAC，未承诺物理设备级别的用量归属。
真实页面与输出仅在私有临时目录处理，不纳入源码包。
已有流量/结算日志曾完成真实读取；尚未用真实账号完成网关开通/下线端到端验证。
学校接口改动仍可能需要适配；以上实测记录不保证网站保留范围或字段永久不变。

## 致谢

本项目受 [Neboer/ipgw-py-manager](https://github.com/Neboer/ipgw-py-manager) 启发，
感谢原作者对东北大学校园网网关管理工具的开发与分享。

原项目的 MIT 许可证保留在 `third-party/ipgw-py-manager.LICENSE`。
本项目自身许可证尚未选择（Cabal 中为 `NONE`）。
