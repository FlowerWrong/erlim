## Scheduler api(http)

```bash
curl -i -X GET "http://127.0.0.1:8083/?port=8080&type=onechat"
```

```json
{
    "ip": "192.168.10.140",
    "port": 8080
}
```

#### error

```json
{
    "error": "No ips to select",
    "code": 10500
}
```

服务器错误: 服务器ip地址池为空